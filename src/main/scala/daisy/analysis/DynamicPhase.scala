// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import java.io.FileWriter
import java.io.BufferedWriter

import lang.Trees._
import tools.{Rational, Interval, MPFRFloat, DynamicEvaluators}
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
object DynamicPhase extends PhaseComponent {
  override val name = "Dynamic"
  override val description = "dynamic evaluation of errors"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption(
      "sampleSize",
      100000,
      "Number of inputs for dynamic evaluation"),
    StringOption(
      "inputRangeFactor",
      "Factor for scaling input ranges"),
    // FlagOption(
    //   "mpfr",
    //   "Use MPFR as the higher precision"),
    FlagOption(
      "dynamic-log",
      "Log results to file"),
    NumOption(
      "dynamic-custom-seed", // pseudo argument "dynamic-seed"
      0,
      "Seed to use for random number generator. 0 for System.currentTimeMillis()")
  )
  override def apply(cfg: Config) = new DynamicPhase(cfg, name, "dynamic")
}

class DynamicPhase(val cfg: Config, val name: String, val shortName: String) extends DaisyPhase
    with DynamicEvaluators {
  implicit val debugSection = DebugSectionAnalysis

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    startRun()

    val numSamples = cfg.option[Long]("sampleSize")
    val inputRangeFactor: Rational = Rational.fromString(cfg.option[Option[String]]("inputRangeFactor").getOrElse("1"))

    val useRationals = false //!cfg.hasFlag("mpfr")
    val logToFile = cfg.hasFlag("dynamic-log")
    val useRoundoff = !cfg.hasFlag("noRoundoff")
    val seed = cfg.option[Long]("dynamic-seed")

    if (useRationals) { cfg.reporter.info("using Rational")
    } else { cfg.reporter.info("using MPFR") }

    cfg.reporter.info("seed: " + seed)

    // val timestamp: Long = System.currentTimeMillis / 1000
    // val fstream = new FileWriter(s"rawdata/${filePrefix}_${prg.id}_$timestamp.txt")
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
      cfg.reporter.info("evaluating " + id + "...")
      //cfg.reporter.info(s"expression is $body")

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
          // if (i % 10000 == 0) cfg.reporter.info(s"i: $i")  // showing progress

          // does not consider input roundoff errors
          val strInputs: Map[Identifier, String] = sampler.nextString
          val dblInputs: Map[Identifier, Double] = strInputs.map({
            case (x, s) => (x -> s.toDouble)
          })
          val ratInputs: Map[Identifier, Rational] = if (useRoundoff) {
            // WITH input errors
            strInputs.map({
              case (x, s) => (x -> Rational.fromString(s))
            })
          } else {
            // no input errors
            dblInputs.map({
            case (x, d) => (x -> Rational.fromDouble(d))
            })
          }

          val dblOutput: Double = evalDouble(body, dblInputs)
          val ratOutput: Rational = evalRational(body, ratInputs)

          measurer.nextValues(dblOutput, ratOutput, ratInputs, dblInputs)
        }

        if (logToFile) {
          logFile.write(s"${prg.id}-$id" +
            s" ${measurer.maxAbsError}" +
            s" ${measurer.maxRelError}" +
            s" ${measurer.minAbsError}" +
            s" ${measurer.minRelError}\n")
        }
        cfg.reporter.info(s"$id maxAbsError: ${measurer.maxAbsError}" +
          s" maxRelError: ${measurer.maxRelError}")

      } else {

        // this functionality is duplicated in ErrorFunctions.errorDynamic,
        // but we are printing here all sorts of stats...
        val sampler = new Uniform(inputRanges, seed)  // no seed = System millis 485793
        val measurer = new ErrorMeasurerMPFR()
        var currentMaxAbsMPFR = measurer.maxAbsError
        var currentMaxAbs: Double = measurer.maxAbsError.doubleValue

        var i = 0
        while (i < numSamples) {
          i = i + 1
          // if (i % 10000 == 0) cfg.reporter.info(s"i: $i")

          // WITH input errors

          val strInputs: Map[Identifier, String] = sampler.nextString
          val dblInputs: Map[Identifier, Double] = strInputs.map({
            case (x, s) => (x -> s.toDouble)
          })
          val mpfrInputs: Map[Identifier, MPFRFloat] =
            if (useRoundoff) {
              // WITH input errors
              strInputs.map({
              case (x, s) => (x -> MPFRFloat.fromString(s))
              })
            } else {
              // no input errors
              dblInputs.map({
                case (x, d) => (x -> MPFRFloat.fromDouble(d))
              })
            }
          val dblOutput: Double = evalDouble(body, dblInputs)
          val mpfrOutput: MPFRFloat = evalMPFR(body, mpfrInputs)

          measurer.nextValues(dblOutput, mpfrOutput)

          // Invariant that absolute errors have to grow monotonically
          assert(currentMaxAbsMPFR <= measurer.maxAbsError)
          currentMaxAbsMPFR = measurer.maxAbsError

          assert(currentMaxAbs <= measurer.maxAbsError.doubleValue)
          currentMaxAbs = measurer.maxAbsError.doubleValue
          if (abs(dblOutput) <= java.lang.Double.MIN_NORMAL) {
            cfg.reporter.warning(s"THE SUBNORMAL")
            i = i - 1
          }
        }

        if (logToFile) {
          logFile.write(s"${prg.id}-$id" +
            s" ${measurer.maxAbsError.toDoubleString}" +
            s" ${measurer.maxRelError.toDoubleString}" +
            s" ${measurer.maxUlpError}" +
            s" ${measurer.minAbsError.toDoubleString}" +
            s" ${measurer.minRelError.toDoubleString} " +
            s" ${measurer.minUlpError}" +
            s" ${measurer.avrgAbsError.toDoubleString}" +
            s" ${measurer.avrgRelError.toDoubleString}" +
            s" ${measurer.avrgUlpError}\n")
        }
        cfg.reporter.info(s"$id maxAbsError: ${measurer.maxAbsError.toDoubleString}" +
          s" maxRelError: ${measurer.maxRelError.toDoubleString}")

      }


    }

    finishRun(ctx, prg)

    if (logToFile) {
      logFile.write(s"\ntime: ${cfg.timers.get(shortName)}\n\n")
      logFile.close()
    }

    (ctx, prg)
  }

}
