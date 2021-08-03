// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package opt

import util.Random
import scala.collection.parallel.{ParSeq}
import scala.collection.parallel.CollectionConverters._
import java.io.{File, FileWriter}

import lang.Trees.{Program, Expr}
import search.GeneticSearch
import tools._
import lang.Identifiers._
import FinitePrecision._
import tools.Rational.max


/**
  Optimizes the order of expressions.

  Prerequisites:
    - SpecsProcessingPhase
 */
object RewritingOptimizationPhase extends DaisyPhase with GeneticSearch[Expr] with RewritingOps with
  opt.CostFunctions with RoundoffEvaluators with DynamicEvaluators with Subdivision  {

  override val name = "Rewriting-Optimization"
  override val description = "Optimization by rewriting"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("rewrite-generations",      30,   "Number of generations to search for"),
    NumOption("rewrite-population",  10,   "Size of the population for genetic search"),
    NumOption("rewrite-seed",              0,   "Seed to use for random number generator." +
      "If not set or 0, will use System.currentTimeMillis()"),
    StringChoiceOption("rewrite-fitness-fnc", Set("interval-affine", "affine-affine", "dynamic", "subdiv"),
      "interval-affine", "Fitness function to be used for rewriting")
  )
  override implicit val debugSection = DebugSectionOptimization

  var reporter: Reporter = null

  // ridiculously high value to signify that an expression is VERY bad, e.g. due to division by zero
  val fitnessOnFail = Rational(1000)

  val activeRules = COMMUTATIVITY ++ ASSOCIATIVITY ++ DISTRIBUTIVITY ++ List(IDENTITIES) ++
    FRACTIONS_TRANSFORM ++ FRACTIONS_DISTRIBUTE

  var initSeed: Long = 0L
  var rand: Random = null

  var optimizeForAccuracy = true

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    maxGenerations = ctx.option[Long]("rewrite-generations").toInt
    populationSize = ctx.option[Long]("rewrite-population").toInt
    initSeed = if (ctx.option[Long]("rewrite-seed") == 0) {
      System.currentTimeMillis()
    } else {
      ctx.option[Long]("rewrite-seed")
    }
    val uniformPrecision: Precision = ctx.option[Precision]("precision")

    optimizeForAccuracy = !ctx.hasFlag("mixed-tuning")

    // Affine arithmetic for ranges is needed to make this work for jetEngine
    val fitnessFunctionName = ctx.option[String]("rewrite-fitness-fnc")


    val infoString = s"fitness function: $fitnessFunctionName, # generations: $maxGenerations, " +
      s"population size: $populationSize, seed: $initSeed"
    ctx.reporter.info(infoString)


    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>
      ctx.reporter.info(s"\nGoing to rewrite ${fnc.id}")

      val inputValMap = ctx.specInputRanges(fnc.id)
      val inputErrors = ctx.specInputErrors(fnc.id)

      val fitnessFunction: (Expr) => Rational =
        fitnessFunctionName match {
          case "interval-affine" =>
            uniformRoundoff_IA_AA(_, inputValMap, inputErrors, uniformPrecision, true, true)._1
          case "affine-affine" =>
            uniformRoundoff_AA_AA(_, inputValMap, inputErrors, uniformPrecision, true, true)._1
          case "subdiv" =>
            val subIntervals: ParSeq[Map[Identifier, Interval]] = // can be pre-computed
              getCustomSubintervals(inputValMap, getDivLimit(inputValMap)).par
            val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)
            val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id)
            uniformSubdivisionRoundoff(_, subIntervals, inputErrorMap, precisionMap, uniformPrecision)

          case "dynamic" =>
            (e: Expr) =>
              Rational.fromString(generalErrorDynamicWithInputRoundoff(e, inputValMap, 256).avrgRelError.toString())
              //errorDynamicWithInputRoundoff(e, in, 256)
        }

      val (oldError, newBody, newError) = rewriteExpression(fnc.body.get, fitnessFunction(_))

      ctx.reporter.info("error after: " + fitnessFunction(newBody))
      ctx.reporter.debug("expr after: " + newBody)

      if (fitnessFunctionName == "dynamic") {
        ctx.reporter.info("evaluating the error of final program")

        val overallErrorOld = generalErrorDynamicWithInputRoundoff(fnc.body.get,
          ctx.specInputRanges(fnc.id), 100000).avrgRelError
        val overallErrorNew = generalErrorDynamicWithInputRoundoff(newBody,
          ctx.specInputRanges(fnc.id), 100000).avrgRelError
        ctx.reporter.result(s"error before: $overallErrorOld\nerror after:  $overallErrorNew")
        val o = new FileWriter(new File("output", "regime-rewriting-baseline.csv"), true)

        // run several trials with different seeds to get reliable data
        val overallErrorOld2 = generalErrorDynamicWithInputRoundoff(fnc.body.get, ctx.specInputRanges(fnc.id), 100000, 274583).avrgRelError
        val overallErrorNew2 = generalErrorDynamicWithInputRoundoff(newBody, ctx.specInputRanges(fnc.id), 100000, 274583).avrgRelError
        ctx.reporter.result(s"error before: $overallErrorOld2\nerror after:  $overallErrorNew2")

        val overallErrorOld3 = generalErrorDynamicWithInputRoundoff(fnc.body.get, ctx.specInputRanges(fnc.id), 100000, 997245).avrgRelError
        val overallErrorNew3 = generalErrorDynamicWithInputRoundoff(newBody, ctx.specInputRanges(fnc.id), 100000, 997245).avrgRelError
        ctx.reporter.result(s"error before: $overallErrorOld3\nerror after:  $overallErrorNew3")

        o.write(s"${fnc.id}, ${overallErrorOld}, ${overallErrorNew}, ${overallErrorOld2}, ${overallErrorNew2} ${overallErrorOld3}, ${overallErrorNew3}\n")
        o.close
      } else if (fitnessFunctionName == "subdiv") {
        val o = new FileWriter(new File("output", "regime-rewriting-baseline.csv"), true)

        o.write(s"${fnc.id}, ${oldError}, ${newError}\n")
        o.close
      }

      fnc.copy(body = Some(newBody))
    }
    (ctx.copy(seed = initSeed), Program(prg.id, newDefs))
  }

  // refactor as we need to call this several times
  def rewriteExpression(initExpr: Expr, roundoffFunction: (Expr) => Rational): (Rational, Expr, Rational) = {

    val fitnessBefore = roundoffFunction(initExpr)

    val costBefore = countOps(initExpr)

    var bestCostExpr = initExpr
    var bestError = fitnessBefore
    var bestTotalError = fitnessBefore

    rand = new Random(initSeed)  // reset generator to obtain deterministic search

    // we assign a very high fitness to signal that something is wrong
    // the offending expression should be filtered out "naturally"
    val (bestExprFound, _) = runGenetic(initExpr,
      (e: Expr) => {
        try {
          val fitness = roundoffFunction(e)

          // saves the expression with smallest error, which does not increase the initial cost
          if (fitness < bestError && countOps(e) <= costBefore) {
            bestCostExpr = e
            bestError = fitness
          }
          if (fitness < bestTotalError) {
            bestTotalError = fitness
          }

          fitness
        } catch {
          case _: DivisionByZeroException |
               _: NonPositiveLogException |
               _: NegativeSqrtException => fitnessOnFail

        }
      })

    if (optimizeForAccuracy) {
      reporter.info(s"RW - error before: $fitnessBefore, error after: $bestError")
      reporter.debug(s"expr before: $initExpr, after:  $bestExprFound")
      (fitnessBefore, bestExprFound, bestTotalError)
    } else {// instead of the most accurate, choose the one with least cost
      (fitnessBefore, bestCostExpr, bestError)
    }
  }

  def mutate(expr: Expr): Expr = _mutate(expr, rand.nextInt(sizeWithoutTerminals(expr)), activeRules)

  def uniformSubdivisionRoundoff(e: Expr, subIntervals: ParSeq[Map[Identifier, Interval]], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], constPrecision: Precision): Rational = {

    val absErrors = subIntervals.map(subInt => {
      val (resRange, intermediateRanges) = evalRange[Interval](e, subInt, Interval.apply)

      val (resRoundoff, allErrors) = evalRoundoff[MPFRAffineForm](e, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(MPFRAffineForm.+/-).toMap,
          zeroError = MPFRAffineForm.zero,
          fromError = MPFRAffineForm.+/-,
          interval2T = MPFRAffineForm.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrors = true)

      Interval.maxAbs(resRoundoff.toInterval)

    })

    absErrors.max
  }

}
