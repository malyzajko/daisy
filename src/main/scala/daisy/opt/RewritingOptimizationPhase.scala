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
  RoundoffEvaluators with DynamicEvaluators {

  override val name = "Rewriting-Optimization"
  override val shortName = "rewriting"
  override val description = "Optimization by rewriting"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption(
      "rewrite-generations",
      30,
      "Number of generations to search for"),
    NumOption(
      "rewrite-population-size",
      30,
      "Size of the population for genetic search"),
    //    ChoiceOption(
    //      "rewrite-fitness-fnc",
    //      Map("interval-affine" -> uniformRoundoffApprox_IA_AA(_, _, _, uniformPrecision)._1,
    //           "affine-affine" -> uniformRoundoff_AA_AA(_, _, _, uniformPrecision)._1,
    //           "smt-affine", -> uniformRoundoff_SMT_AA(_, _, _, uniformPrecision)._1,
    //           "dynamic-256" -> (e: Expr, in: Map[Identifier, Interval], err: Map[Identifier, Rational]) =>
    //              errorDynamicWithInputRoundoff(e, in, 256)),
    //      "interval-affine",
    //      "Fitness function to be used during search"),
    FlagOption(
      "rewrite-baseline",
      "Compute baseline errors dynamically (expensive!)"),
    // this could be just one option: rewrite-seed and allow the option 'systemmillis'
    NumOption(
      "rewrite-custom-seed", // pseudo argument "rewrite-seed"
      4781,
      "Seed to use for random number generator. 0 for System.currentTimeMillis()")
  )

  implicit val debugSection = DebugSectionOptimization

  var reporter: Reporter = null

  val baselineDynamicSamples = 100000
  // ridiculously high value to signify that an expression is VERY bad,
  // e.g. due to division by zero
  val fitnessOnFail = Rational(1000)

  val activeRules = commRules ++ assocRules ++ distRules ++ idReduceRules ++
    fracTransRules ++ fracDistRules

  var seed = System.currentTimeMillis()
  var rand = new Random(seed)

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    maxGenerations = ctx.option[Long]("rewrite-generations").toInt
    populationSize = ctx.option[Long]("rewrite-population-size").toInt

    val uniformPrecision: Precision = ctx.option[Precision]("precision")

    // Affine arithmetic for ranges is needed to make this work for jetEngine
    val fitnessFunction: (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) => Rational =
      uniformRoundoff_IA_AA(_, _, _, uniformPrecision, true, true)._1
    val fitnessFunctionName = "interval-affine"

    val seed = if (ctx.option[Long]("rewrite-custom-seed") == 0) {
      System.currentTimeMillis()
    } else {
      ctx.option[Long]("rewrite-custom-seed")
    }
    rand = new Random(seed)

    val infoString = s"fitness function: $fitnessFunctionName, # generations: $maxGenerations, " +
      s"population size: $populationSize, seed: $seed"
    ctx.reporter.info(infoString)



    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>

      ctx.reporter.info(s"\nGoing to rewrite ${fnc.id}")

      val allIDs = fnc.params.map(_.id)
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val inputErrors = allIDs.map {
        id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))
      }.toMap

      val newBody = rewriteExpression(fnc.body.get, fitnessFunction, inputValMap, inputErrors)

      ctx.reporter.info("error after: " + fitnessFunction(newBody, inputValMap, inputErrors))
      ctx.reporter.debug("expr after: " + newBody)

      if(ctx.hasFlag("rewrite-baseline")) {
        val dynamicErrorBefore = errorDynamicWithInputRoundoff(fnc.body.get, inputValMap, baselineDynamicSamples)
        val dynamicErrorAfter = errorDynamicWithInputRoundoff(newBody, inputValMap, baselineDynamicSamples)
        ctx.reporter.info(s"dynamic error before: $dynamicErrorBefore, after: $dynamicErrorAfter")

        val improvement = (dynamicErrorBefore - dynamicErrorAfter) / dynamicErrorBefore
        ctx.reporter.info(s"improvement: $improvement")
      }

      fnc.copy(body = Some(newBody))
    }

    (ctx, Program(prg.id, newDefs))
  }

  // refactor as we need to call this several times
  def rewriteExpression(initExpr: Expr,
    roundoffFunction: (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) => Rational,
    inputValMap: Map[Identifier, Interval],
    inputErrors: Map[Identifier, Rational]): Expr = {

    val fitnessBefore = roundoffFunction(initExpr, inputValMap, inputErrors)
    reporter.info(s"error before: $fitnessBefore")
    reporter.debug("expr before: " + initExpr)

    var bestErrorExpr = initExpr
    var bestError = fitnessBefore

    rand = new Random(seed)  // reset generator to obtain deterministic search

    // we assign a very high fitness to signal that something is wrong
    // the offending expression should be filtered out "naturally"
    val (bestExprFound, _) = runGenetic(initExpr,
      (e: Expr) => {
        try {
          val fitness = roundoffFunction(e, inputValMap, inputErrors)

          // saves the expression with smallest error, which does not increase the initial cost
          if (fitness < bestError) {
            bestErrorExpr = e
            bestError = fitness
          }

          fitness
        } catch {
          case e: daisy.tools.DivisionByZeroException =>
            fitnessOnFail
        }
      })

    bestExprFound
  }

  def mutate(expr: Expr): Expr = _mutate(expr, rand.nextInt(sizeWithoutTerminals(expr)), activeRules)



}
