// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis


import lang.Trees._
import tools.{Rational, Interval, DynamicEvaluators, MPFRFloat}
import lang.Identifiers._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random.shuffle
import scala.util.Random

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
object BGRTDynamicPhase extends DaisyPhase with DynamicEvaluators {
  override val name = "BGRTDynamic"
  override val shortName = "bgrtdynamic"
  override val description = "dynamic evaluation of errors using binary guided random testing"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption(
      "bgrtSampleSize",
      100000,
      "Number of inputs for dynamic evaluation"),
    StringOption(
      "bgrtInputRangeFactor",
      "Factor for scaling input ranges"),
    FlagOption(
      "bgrt-dynamic-log",
      "Log results to file"),
    NumOption(
      "bgrt-dynamic-custom-seed", // pseudo argument "dynamic-seed"
      0,
      "Seed to use for random number generator. 0 for System.currentTimeMillis()")
  )

  implicit val debugSection = DebugSectionAnalysis

  

  override def runPhase(ctx: Context, prg: Program):  (Context, Program)  = {
    val numSamples = ctx.option[Long]("bgrtSampleSize")
    inputRangeFactor = Rational.fromString(ctx.option[Option[String]]("bgrtInputRangeFactor").getOrElse("1"))
    val useRoundoff = !ctx.hasFlag("noRoundoff")

    val initSeed = if (ctx.option[Long]("bgrt-dynamic-custom-seed") == 0) {
      System.currentTimeMillis()
    } else {
      ctx.option[Long]("bgrt-dynamic-custom-seed")
    }

    val res: Map[Identifier, (Rational, Rational, Long)] = analyzeConsideredFunctions(ctx, prg){ fnc =>

      val id = fnc.id
      val body = fnc.body.get
      ctx.reporter.info("evaluating " + id + "...")

      val initRanges: Map[Identifier, Interval] = ctx.specInputRanges(id).map({
        case (id, i) =>
          (id, Interval(i.mid - inputRangeFactor * i.radius,
            i.mid + inputRangeFactor * i.radius))
      })

      var nextInput = initRanges
      val r = new Random()
      var totalSamples: Long = 0
      /* adjustable params */
      val nPart = 10 // divide-and-permute will be repeated nPart times to generate next tighter set of configurations
      val resourceLimit = 10 // acts as a time resource allocated to find tighter configuration. Here, number of times it can zoom into configuration interval.
      
      var currentMaxAbsMPFR = MPFRFloat.fromDouble(0.0)
      var currentMaxAbs: Double = currentMaxAbsMPFR.doubleValue
      var currentRelError = MPFRFloat.fromDouble(0.0)
      
      var i = 0
      var resrtart = false

      while (i < resourceLimit){
        i = i + 1
        val nextRanges : List[Map[Identifier, Interval]] = nextGen(nextInput, nPart)
        for (irange <- nextRanges){
          totalSamples = totalSamples + numSamples
          val measurer: ErrorMeasurerMPFR = dynamicErrorEvaluation(body, irange, initSeed, numSamples, useRoundoff)
          if(currentMaxAbsMPFR <= measurer.maxAbsError){ 
            // updating error values
            currentMaxAbsMPFR = measurer.maxAbsError
            currentMaxAbs = measurer.maxAbsError.doubleValue
            currentRelError = measurer.maxRelError

            nextInput = irange
          }
        }
        resrtart = r.nextBoolean()
          if(resrtart == true){
            nextInput = initRanges 
        }
      }
      ctx.reporter.info(s"$id maxAbsError: ${currentMaxAbsMPFR.toDoubleString}" +
          s" maxRelError: ${currentRelError.toDoubleString}")
      (Rational.fromString(currentMaxAbsMPFR.toString),
      Rational.fromString(currentMaxAbs.toString),
      totalSamples)
    }
    (ctx.copy(
      resultAbsoluteErrors = res.mapValues(_._1),
      resultRelativeErrors = res.mapValues((x: (Rational, Rational, Long)) => Some(x._2)),
      resultNumberSamples = res.mapValues(_._3),
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

        val p1: Map[Identifier,(Interval,Interval)] = splitConf.filterKeys(ls.toSet)
        val p2: Map[Identifier,(Interval,Interval)] = splitConf.filterKeys(rs.toSet)

        val m1 = p1.map({case(id, (lo,hi)) => (id,lo)}) ++ p2.map({case(id, (lo,hi)) => (id,hi)})
        val m2 = p2.map({case(id, (lo,hi)) => (id,lo)}) ++ p1.map({case(id, (lo,hi)) => (id,hi)})

        nextg += m1
        nextg += m2

        i = i + 1
      }
    nextg.toSet.toList
  }
}