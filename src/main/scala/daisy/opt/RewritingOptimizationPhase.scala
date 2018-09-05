// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package opt

import util.Random

import lang.Trees.{Program, Expr}
import search.GeneticSearch
import tools._
import lang.Identifiers._
import FinitePrecision._

/**
  Optimizes the order of expressions.

  Prerequisites:
    - SpecsProcessingPhase
 */
object RewritingOptimizationPhase extends DaisyPhase with GeneticSearch[Expr] with RewritingOps with
  opt.CostFunctions with RoundoffEvaluators with DynamicEvaluators {

  override val name = "Rewriting-Optimization"
  override val shortName = "rewriting"
  override val description = "Optimization by rewriting"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("rewrite-generations",      30,   "Number of generations to search for"),
    NumOption("rewrite-population",  30,   "Size of the population for genetic search"),
    NumOption("rewrite-seed",              0,   "Seed to use for random number generator." +
      "If not set or 0, will use System.currentTimeMillis()"),
    StringChoiceOption("rewrite-fitness-fnc", Set("interval-affine", "affine-affine", "dynamic"),
      "interval-affine", "Fitness function to be used for rewriting")
  )

  implicit val debugSection = DebugSectionOptimization

  var reporter: Reporter = null

  // ridiculously high value to signify that an expression is VERY bad, e.g. due to division by zero
  val fitnessOnFail = Rational(1000)

  val activeRules = COMMUTATIVITY ++ ASSOCIATIVITY ++ DISTRIBUTIVITY ++ List(IDENTITIES) ++
    FRACTIONS_TRANSFORM ++ FRACTIONS_DISTRIBUTE

  var initSeed: Long = 0l
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
    val fitnessFunction: (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) => Rational =
      fitnessFunctionName match {
      case "interval-affine" =>
        uniformRoundoff_IA_AA(_, _, _, uniformPrecision, true, true)._1
      case "affine-affine" =>
        uniformRoundoff_AA_AA(_, _, _, uniformPrecision, true, true)._1
      case "dynamic" =>
        (e: Expr, in: Map[Identifier, Interval], err: Map[Identifier, Rational]) =>
          errorDynamicWithInputRoundoff(e, in, 256)
    }

    val infoString = s"fitness function: $fitnessFunctionName, # generations: $maxGenerations, " +
      s"population size: $populationSize, seed: $initSeed"
    ctx.reporter.info(infoString)


    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>
      ctx.reporter.info(s"\nGoing to rewrite ${fnc.id}")

      val inputValMap = ctx.specInputRanges(fnc.id)
      val inputErrors = ctx.specInputErrors(fnc.id)

      val newBody = rewriteExpression(fnc.body.get, fitnessFunction(_, inputValMap, inputErrors))

      ctx.reporter.info("error after: " + fitnessFunction(newBody, inputValMap, inputErrors))
      ctx.reporter.debug("expr after: " + newBody)

      fnc.copy(body = Some(newBody))
    }

    (ctx.copy(seed = initSeed), Program(prg.id, newDefs))
  }

  // refactor as we need to call this several times
  def rewriteExpression(initExpr: Expr, roundoffFunction: (Expr) => Rational): Expr = {

    val fitnessBefore = roundoffFunction(initExpr)
    reporter.info(s"error before: $fitnessBefore")
    reporter.debug("expr before: " + initExpr)

    val costBefore = countOps(initExpr)

    var bestCostExpr = initExpr
    var bestError = fitnessBefore

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

          fitness
        } catch {
          case _: DivisionByZeroException |
               _: NonPositiveLogException |
               _: NegativeSqrtException => fitnessOnFail

        }
      })

    if (optimizeForAccuracy) {
      bestExprFound
    } else {// instead of the most accurate, choose the one with least cost
      bestCostExpr
    }
  }

  def mutate(expr: Expr): Expr = _mutate(expr, rand.nextInt(sizeWithoutTerminals(expr)), activeRules)



}