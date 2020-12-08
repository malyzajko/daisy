// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import java.io.FileWriter
import java.io.BufferedWriter

import daisy.analysis.Sampler.Uniform
import lang.Trees._
import tools.{DynamicEvaluators, Interval, MPFRFloat, Rational}
import lang.Identifiers._

import scala.collection.immutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.Random.shuffle

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

  BGRT Dynamic Phase is adapted form a conference paper "Efficient  Search  for  Inputs  Causing
  High  Floating-point  Errors" published by Wei-Fan Chiang ,Ganesh Gopalakrishnan
  Zvonimir Rakamari and Alexey Solovyev in proc. of PPoPP 2014.
 */
object DynamicPhase extends DaisyPhase with DynamicEvaluators {
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
    FlagOption(
      "dynamic-log",
      "Log results to file"),
    NumOption(
      "dynamic-seed", // pseudo argument "dynamic-seed"
      0,
      "Seed to use for random number generator. 0 for System.currentTimeMillis()"),
    StringChoiceOption(
      "dynamic-method",
      Set("bgrt", "uniformRational", "uniformMPFR"),
      "uniformMPFR",
      "Method for dynamic analysis")
  )

  override implicit val debugSection = DebugSectionAnalysis



  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val numSamples = ctx.option[Long]("sampleSize")
    inputRangeFactor = Rational.fromString(ctx.option[Option[String]]("inputRangeFactor").getOrElse("1"))
    val logToFile = ctx.hasFlag("dynamic-log")
    val useRoundoff = !ctx.hasFlag("noRoundoff")
    val dynamic_method = ctx.option[String]("dynamic-method")
    val initSeed = if (ctx.option[Long]("dynamic-seed") == 0) {
      System.currentTimeMillis()
    } else {
      ctx.option[Long]("dynamic-seed")
    }

    val logFile = if (logToFile) {
      val fstream = new FileWriter(s"rawdata/dynamic_${prg.id}.txt", true) // append
      val out = new BufferedWriter(fstream)
      out.write(s"# sampleSize: $numSamples, ")
      out.write(s"inputRangeFactor: $inputRangeFactor, ")
      out.write(s"dynamicG: $inputRangeFactor, ")
      (dynamic_method: @unchecked) match {
        case "bgrt" =>
          out.write("BGRT\n")
          out.write("# benchmark maxAbsMPFRError maxAbsError maxRelError\n\n")
        case "uniformRational" =>
          out.write("rationals\n")
          out.write("# benchmark maxAbsError maxRelError minAbsError minRelError\n\n")
        case "uniformMPFR" =>
          out.write("mpfr\n")
          out.write(s"# benchmark maxAbsError maxRelError maxUlpError minAbsError " +
            "minRelError minUlpError avrgAbsError avrgRelError avrgUlpError\n\n")
      }
      out
    } else {
      null
    }

    // returns max absolute and max relative error found
    val res: Map[Identifier, (Rational, Rational, Long)] = analyzeConsideredFunctions(ctx, prg) { fnc =>
      val id = fnc.id
      val body = fnc.body.get
      ctx.reporter.info("evaluating " + id + "...")
      // ctx.reporter.info(s"expression is $body")
      val inputRanges: Map[Identifier, Interval] = ctx.specInputRanges(id).map({
        case (id, i) =>
          (id, Interval(i.mid - inputRangeFactor * i.radius,
            i.mid + inputRangeFactor * i.radius))
      })

      val (measurer,totalSamples): (ErrorMeasurerMPFR, Long) = (dynamic_method: @unchecked) match {
        case "bgrt" =>
          val (measurer, totalSamples) = bgrtEvaluation(body, inputRanges, initSeed, numSamples, useRoundoff)
          (measurer, totalSamples)

        case "uniformRational" =>
          val measurer = dynamicErrorEvaluationRational(body, inputRanges, initSeed, numSamples, useRoundoff)
          // converting Rational to MPFR float
          val mpfr_measurer = new ErrorMeasurerMPFR()
          mpfr_measurer.currentAbsError_Max = MPFRFloat.fromDouble(measurer.currentAbsError_Max.doubleValue())
          mpfr_measurer.currentAbsError_Min = MPFRFloat.fromDouble(measurer.currentAbsError_Min.doubleValue())
          mpfr_measurer.currentRelError_Max = MPFRFloat.fromDouble(measurer.currentRelError_Max.doubleValue())
          mpfr_measurer.currentRelError_Min = MPFRFloat.fromDouble(measurer.currentRelError_Min.doubleValue())
          (mpfr_measurer, numSamples)

        case "uniformMPFR" =>
          val measurer = dynamicErrorEvaluation(body, inputRanges, initSeed, numSamples, useRoundoff)
          (measurer, numSamples)
      }
      if (logToFile) {
        logFile.write(s"${prg.id}-$id" +
          s" ${measurer.maxAbsError}" +
          s" ${measurer.maxRelError}" +
          s" ${measurer.maxUlpError}" +
          s" ${measurer.minAbsError}" +
          s" ${measurer.minRelError} " +
          s" ${measurer.minUlpError}" +
          s" ${measurer.avrgAbsError}" +
          s" ${measurer.avrgRelError}" +
          s" ${measurer.avrgUlpError}\n")
      }
      ctx.reporter.info(s"$id maxAbsError: ${measurer.maxAbsError.toDoubleString}" +
        s" maxRelError: ${measurer.maxRelError.toDoubleString}")
      (Rational.fromString(measurer.maxAbsError.toString),
        Rational.fromString(measurer.maxRelError.toString),
        totalSamples)
    }


    if (logToFile) {
      logFile.write(s"\ntime: ${ctx.timers.get(name)}\n\n")
      logFile.close()
    }

    (ctx.copy(
      resultAbsoluteErrors = res.mapValues(_._1).toMap,
      resultRelativeErrors = res.mapValues((x: (Rational, Rational, Long)) => Some(x._2)).toMap,
      resultNumberSamples = res.mapValues(_._3).toMap,
      seed = initSeed),
      prg)

  }


  /**
  Takes a configuration and enumerates a set of its tighter configurations to explore.
  First partitions configuration into two non-overlapping configurations and then it computes the
  upper and lower halves of the two non-overlapping partitions and permutes them. Such divide-and-permute
  will be repeated nPart times.
   */
  private def nextGen(conf: Map[Identifier, Interval], nPart: Int): List[Map[Identifier, Interval]] = {
    val nextg = ArrayBuffer[Map[Identifier, Interval]]()
    val splitConf : Map[Identifier, (Interval, Interval)] = conf.map({
      case (id, interval) =>
        val (lo, hi) = interval.getBinaryDivision
        (id, (lo, hi))
    })

    var i = 0

    while (i < nPart) {
      val inputVariables = conf.keys.toList
      val permutedInput =  shuffle(inputVariables)
      val (ls,rs) = permutedInput.splitAt((permutedInput.size)/2)

      val p1: Map[Identifier,(Interval,Interval)] = splitConf.filterKeys(ls.toSet).toMap
      val p2: Map[Identifier,(Interval,Interval)] = splitConf.filterKeys(rs.toSet).toMap

      val m1 = p1.map({case(id, (lo,hi)) => (id,lo)}) ++ p2.map({case(id, (lo,hi)) => (id,hi)})
      val m2 = p2.map({case(id, (lo,hi)) => (id,lo)}) ++ p1.map({case(id, (lo,hi)) => (id,hi)})

      nextg += m1
      nextg += m2

      i = i + 1
    }
    nextg.toSet.toList
  }

  /**
   * bgrt Evaluation : takes input ranges and computes max absolute MPFR ,max relative error and
   * max absolute error
   */
  private def bgrtEvaluation(body: Expr, inputRanges: Map[Identifier, Interval],
                             initSeed: Long, numSamples: Long, useRoundoff: Boolean): (ErrorMeasurerMPFR,Long) = {
    var nextInput = inputRanges
    val r = new Random()
    var totalSamples: Long = 0
    /* adjustable params */
    val nPart = 10 // divide-and-permute will be repeated nPart times to generate next tighter set of configurations
    val resourceLimit = 10 // acts as a time resource allocated to find tighter configuration. Here, number of times it can zoom into configuration interval.
    var current_measurer = new ErrorMeasurerMPFR()

    var i = 0
    var restart = false

    while (i < resourceLimit) {
      i = i + 1
      val nextRanges: List[Map[Identifier, Interval]] = nextGen(nextInput, nPart)
      for (irange <- nextRanges) {
        totalSamples = totalSamples + numSamples
        val measurer: ErrorMeasurerMPFR = dynamicErrorEvaluation(body, irange, initSeed, numSamples, useRoundoff)
        if (current_measurer.currentAbsError_Max <= measurer.maxAbsError) {
          // updating error values
          current_measurer = measurer
          nextInput = irange
        }
      }
      restart = r.nextBoolean()
      if (restart == true) {
        nextInput = inputRanges
      }
    }
    (current_measurer,totalSamples)
  }

  private def dynamicErrorEvaluationRational(expr: Expr, inputConfig: Map[Identifier, Interval],
                             seed: Long, numSamples: Long, useRoundoff: Boolean): ErrorMeasurerRational = {

    val measurer = new ErrorMeasurerRational()
    val sampler = new Uniform(inputConfig, seed)

    var i = 0
    while (i < numSamples) {
      i = i + 1
      // if (i % 10000 == 0) ctx.reporter.info(s"i: $i")  // showing progress

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

      val dblOutput: Double = evalDouble(expr, dblInputs)
      val ratOutput: Rational = evalRational(expr, ratInputs)

      measurer.nextValues(dblOutput, ratOutput, ratInputs, dblInputs)
    }
    measurer
  }


}
