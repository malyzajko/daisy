package daisy
package analysis

import java.io.FileWriter
import java.io.BufferedWriter
import scala.util.Random

import lang.Trees._
import utils.{Rational, Interval, MPFRFloat}
import MPFRFloat.{abs => mpfr_abs, max => mpfr_max, min => mpfr_min}
import Rational._
import lang.Identifiers._
import Sampler._

/**
  Phase for dynamic analysis of errors.
  Currently supports only estimation of roundoff errors for double floating-point
  computations. Work by running the double float computation side-by-side with
  one performed in rationals or arbitrary precision (mpfr) and by comparing the
  results.

  When using rationals, only min/max absolute and relative errors can be tracked.
  Also, square root and transcendental functions are not supported.
  MPFR supports also ulp errors, and average errors.

  Roundoff errors considered:
    When using rationals, input roundoff errors are not considered, i.e. the inputs
    are considered to be exact. Roundoff errors from constants are taken into account.

    MPFR does not consider input errors either.

  Prerequisites:
    - SpecsProcessingPhase
 */
object DynamicPhase extends DaisyPhase {

  var numSamples = 100000
  var inputRangeFactor = Rational.one

  override val name = "dynamic phase"
  override val description = "dynamic evaluation of errors"
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ParamOptionDef("sampleSize", "number of inputs for dynamic evaluation", numSamples.toString),
    ParamOptionDef("inputRangeFactor", "factor for scaling input ranges", inputRangeFactor.toString),
    FlagOptionDef("mpfr", "Use MPFR as the higher precision"),
    FlagOptionDef("log", "log results to file"),
    ParamOptionDef("seed", "seed to use for random number generator", "System.currentTimeMillis")
    )

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.dynamic.start

    // make rationals default as they are a bit more reliable
    var useRationals = true
    var logToFile = false
    var seed = System.currentTimeMillis // 4783

    /* Process relevant options */
    for (opt <- ctx.options) opt match {
      case ParamOption("sampleSize", value) => numSamples = value.toInt
      case ParamOption("inputRangeFactor", value) =>
        inputRangeFactor = Rational.fromString(value)

      case FlagOption("mpfr") => useRationals = false
      case FlagOption("log") => logToFile = true
      case ParamOption("seed", value) => seed = value.toLong
      case _ =>
    }

    if (useRationals) reporter.info("using Rational")
    else reporter.info("using MPFR")

    reporter.info("seed: " + seed)

    //val timestamp: Long = System.currentTimeMillis / 1000
    //val fstream = new FileWriter(s"rawdata/${filePrefix}_${prg.id}_$timestamp.txt")
    val logFile = if (logToFile) {
      val fstream = new FileWriter(s"rawdata/dynamic_${prg.id}.txt", true) // append
      val out = new BufferedWriter(fstream)
      out.write(s"# sampleSize: $numSamples, ")
      out.write(s"inputRangeFactor: $inputRangeFactor, ")
      if (useRationals) {
        out.write("rationals\n")
        // ulp and average errors are only tracked for mpfr
        out.write(s"# benchmark maxAbsError maxRelError minAbsError minRelError\n\n")
      } else {
        out.write("mpfr\n")
        out.write(s"# benchmark maxAbsError maxRelError maxUlpError minAbsError " +
            "minRelError minUlpError avrgAbsError avrgRelError avrgUlpError\n\n")
      }
      out
    } else {
      null
    }


    for (fnc <- prg.defs) if (!fnc.precondition.isEmpty && !fnc.body.isEmpty) {

      val id = fnc.id
      val body = fnc.body.get
      reporter.info("evaluating " + id + "...")

      val inputRanges: Map[Identifier, Interval] = ctx.specInputRanges(id).map({
        case (id, i) =>
          (id, Interval(i.mid - inputRangeFactor * i.radius,
                        i.mid + inputRangeFactor * i.radius))
        })

      if (useRationals) {

        val sampler = new Uniform(inputRanges, seed)
        val measurer = new ErrorMeasurerRational()

        var i = 0
        while (i < numSamples) {
          i = i + 1
          //if (i % 10000 == 0) println (s"i: $i")  // showing progress

          // does not consider input roundoff errors
          val dblInputs: Map[Identifier, Double] = sampler.next
          val ratInputs: Map[Identifier, Rational] = dblInputs.map({
            case (x, d) => (x -> Rational.fromDouble(d))
            })

          val dblOutput: Double = evalDouble(body, dblInputs)
          val ratOutput: Rational = evalRational(body, ratInputs)

          measurer.nextValues(dblOutput, ratOutput)
        }

        if (logToFile) {
          logFile.write(s"${prg.id}-$id" +
            s" ${measurer.maxAbsError}"+
            s" ${measurer.maxRelError}" +
            s" ${measurer.minAbsError}" +
            s" ${measurer.minRelError}\n")
        }
        reporter.info(s"$id maxAbsError: ${measurer.maxAbsError}" +
          s" maxRelError: ${measurer.maxRelError}")

      } else {

        // this functionality is duplicated in ErrorFunctions.errorDynamic,
        // but we are printing here all sorts of stats...
        val sampler = new Uniform(inputRanges, seed)  //no seed = System millis 485793
        val measurer = new ErrorMeasurerMPFR()
        var currentMaxAbsMPFR = measurer.maxAbsError
        var currentMaxAbs: Double = measurer.maxAbsError.doubleValue

        var i = 0
        while (i < numSamples) {
          i = i + 1
          //if (i % 10000 == 0) println (s"i: $i")

          // no input errors
          val dblInputs: Map[Identifier, Double] = sampler.next
          val mpfrInputs: Map[Identifier, MPFRFloat] = dblInputs.map({
            case (x, d) => (x -> MPFRFloat.fromDouble(d))
            })

          val dblOutput: Double = evalDouble(body, dblInputs)
          val mpfrOutput: MPFRFloat = evalMPFR(body, mpfrInputs)

          measurer.nextValues(dblOutput, mpfrOutput)

          // Invariant that absolute errors have to grow monotonically
          assert(currentMaxAbsMPFR <= measurer.maxAbsError)
          currentMaxAbsMPFR = measurer.maxAbsError

          assert(currentMaxAbs <= measurer.maxAbsError.doubleValue)
          currentMaxAbs = measurer.maxAbsError.doubleValue
        }

        if (logToFile) {
          logFile.write(s"${prg.id}-$id" +
            s" ${measurer.maxAbsError.toDoubleString}"+
            s" ${measurer.maxRelError.toDoubleString}" +
            s" ${measurer.maxUlpError}"+
            s" ${measurer.minAbsError.toDoubleString}" +
            s" ${measurer.minRelError.toDoubleString} " +
            s" ${measurer.minUlpError}" +
            s" ${measurer.avrgAbsError.toDoubleString}" +
            s" ${measurer.avrgRelError.toDoubleString}"+
            s" ${measurer.avrgUlpError}\n")
        }
        reporter.info(s"$id maxAbsError: ${measurer.maxAbsError.toDoubleString}" +
          s" maxRelError: ${measurer.maxRelError.toDoubleString}")

      }


    }
    timer.stop

    if (logToFile) {
      logFile.write(s"\ntime: ${timer}\n\n")
      logFile.close()
    }

    ctx.reporter.info(s"Finished $name")
    (ctx, prg)
  }


  def evalRational(expr: Expr, _valMap: Map[Identifier, Rational]): Rational = {
    var valMap = _valMap

    def eval(e: Expr): Rational = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case RealLiteral(r) => r
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      case UMinus(x) => - eval(x)
      case Let(id, v, b) =>
        valMap += (id -> eval(v))
        eval(b)

    }
    eval(expr)

  }

  def evalDouble(expr: Expr, _valMap: Map[Identifier, Double]): Double = {
    var valMap = _valMap

    def eval(e: Expr): Double = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case RealLiteral(r) => r.toDouble
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      case Pow(x, y) => math.pow( eval(x), eval(y))
      case UMinus(x) => - eval(x)
      case Sqrt(x) => math.sqrt(eval(x))
      case Sin(x) => math.sin(eval(x))
      case Cos(x) => math.cos(eval(x))
      case Tan(x) => math.tan(eval(x))
      case Exp(x) => math.exp(eval(x))
      case Log(x) => math.log(eval(x))
      case Let(id, v, b) =>
        valMap += (id -> eval(v))
        eval(b)

    }
    eval(expr)

  }

  def evalMPFR(expr: Expr, _valMap: Map[Identifier, MPFRFloat]): MPFRFloat = {
    var valMap = _valMap

    def eval(e: Expr): MPFRFloat = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case r: RealLiteral => MPFRFloat.fromString(r.stringValue)
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      case Pow(x, y) => MPFRFloat.pow( eval(x), eval(y))
      case UMinus(x) => - eval(x)
      case Sqrt(x) => MPFRFloat.sqrt( eval(x) )
      case Sin(x) => MPFRFloat.sin(eval(x))
      case Cos(x) => MPFRFloat.cos(eval(x))
      case Tan(x) => MPFRFloat.tan(eval(x))
      case Exp(x) => MPFRFloat.exp(eval(x))
      case Log(x) => MPFRFloat.log(eval(x))
      case Let(id, v, b) =>
        valMap += (id -> eval(v))
        eval(b)

    }
    eval(expr)
  }
}
