// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import java.io.FileWriter
import java.io.BufferedWriter

import lang.Trees._
import tools.{Rational, Interval, DynamicEvaluators}
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
object DynamicPhase extends DaisyPhase with DynamicEvaluators {
  override val name = "Dynamic"
  override val shortName = "dynamic"
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

  implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val numSamples = ctx.option[Long]("sampleSize")
    inputRangeFactor = Rational.fromString(ctx.option[Option[String]]("inputRangeFactor").getOrElse("1"))
    val useRationals = false //!ctx.hasFlag("mpfr")
    val logToFile = ctx.hasFlag("dynamic-log")
    val useRoundoff = !ctx.hasFlag("noRoundoff")
    val initSeed = if (ctx.option[Long]("dynamic-custom-seed") == 0) {
      System.currentTimeMillis()
    } else {
      ctx.option[Long]("dynamic-custom-seed")
    }

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

    // returns max absolute and max relative error found
    val res: Map[Identifier, (Rational, Rational, Long)] = analyzeConsideredFunctions(ctx, prg){ fnc =>

      val id = fnc.id
      val body = fnc.body.get
      ctx.reporter.info("evaluating " + id + "...")
      //ctx.reporter.info(s"expression is $body")
      val inputRanges: Map[Identifier, Interval] = ctx.specInputRanges(id).map({
        case (id, i) =>
          (id, Interval(i.mid - inputRangeFactor * i.radius,
            i.mid + inputRangeFactor * i.radius))
        })
      
      if (useRationals) {
        val measurer = new ErrorMeasurerRational()
        val sampler = new Uniform(inputRanges, initSeed)
      
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
        ctx.reporter.info(s"$id maxAbsError: ${measurer.maxAbsError}" +
          s" maxRelError: ${measurer.maxRelError}")

        (measurer.maxAbsError, measurer.maxRelError, numSamples)

      } else {
        val measurer = dynamicErrorEvaluation(body,inputRanges, initSeed, numSamples, useRoundoff)
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
        ctx.reporter.info(s"$id maxAbsError: ${measurer.maxAbsError.toDoubleString}" +
          s" maxRelError: ${measurer.maxRelError.toDoubleString}")
        (Rational.fromString(measurer.maxAbsError.toString),
          Rational.fromString(measurer.maxRelError.toString),
          numSamples)
      }
    }

    if (logToFile) {
      logFile.write(s"\ntime: ${ctx.timers.get(shortName)}\n\n")
      logFile.close()
    }

    (ctx.copy(
      resultAbsoluteErrors = res.mapValues(_._1),
      resultRelativeErrors = res.mapValues((x: (Rational, Rational,Long)) => Some(x._2)),
      resultNumberSamples = res.mapValues(_._3),
      seed = initSeed),
      prg)
  }

}
