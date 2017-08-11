// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package opt

import scala.collection.immutable.Seq
import java.io.FileWriter
import java.io.BufferedWriter
import util.Random

import lang.Trees.{Program, Expr, Terminal}
import search.GeneticSearch
import tools._
import lang.Identifiers._
import FinitePrecision._

import lang.TreeOps._


/**
  Optimizes the order of expressions.

  Prerequisites:
    - SpecsProcessingPhase
 */
object RewritingOptimizationPhase extends DaisyPhase with GeneticSearch[Expr] with RewritingOps with
  RoundoffEvaluatorsApprox with DynamicEvaluators {

  override val name = "rewriting-optimization phase"
  override val description = "optimization by rewriting"

  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ParamOptionDef("rewrite-generations", "Number of generations to search for",
      maxGenerations.toString),
    ParamOptionDef("rewrite-population-size", "Size of the population for genetic search",
      populationSize.toString),
    // ChoiceOptionDef("rewrite-fitness-fnc", "Fitness function to be used during search",
    //   Set("interval-affine", "affine-affine", "smt-affine", "dynamic-256"), "interval-affine"),
    FlagOptionDef("rewrite-baseline", "Compute baseline errors dynamically (expensive!)"),

    // this could be just one option: rewrite-seed and allow the option 'systemmillis'
    FlagOptionDef("rewrite-seed-system-millis", "Use the system time for random seed"),
    ParamOptionDef("rewrite-custom-seed", "Use the given seed", "4781")
    )

  implicit val debugSection = DebugSectionOptimization
  override var reporter: Reporter = null

  var seed: Long = 4781l // System.currentTimeMillis // 1469010147126l
  var rand: Random = null
  var computeBaseline: Boolean = false
  val baselineDynamicSamples = 100000
  // ridiculously high value to signify that an expression is VERY bad,
  // e.g. due to division by zero
  val fitnessOnFail = Rational(1000)

  val uniformPrecision = Float64

  val activeRules = commRules ++ assocRules ++ distRules ++ idReduceRules ++
    fracTransRules ++ fracDistRules

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.rewriting.start

    var fitnessFunction: (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) => Rational =
      uniformRoundoffApprox_IA_AA(_, _, _, uniformPrecision)._1
    var fitnessFunctionName = "interval-affine"

    /* Process relevant options */
    for (opt <- ctx.options) opt match {
      case ParamOption("rewrite-generations", value) =>
        maxGenerations = value.toInt

      case ParamOption("rewrite-population-size", value) =>
        populationSize = value.toInt

      // case ChoiceOption("rewrite-fitness-fnc", value) =>
      //   fitnessFunction = value match {
      //     case "interval-affine" => uniformRoundoff_IA_AA(_, _, _, uniformPrecision)._1
      //     case "affine-affine" => uniformRoundoff_AA_AA(_, _, _, uniformPrecision)._1
      //     case "smt-affine" => uniformRoundoff_SMT_AA(_, _, _, uniformPrecision)._1
      //     // this does not conform to the interface of the static analyses,
      //     // so we need to do an outer anonymous function
      //     case "dynamic-256" =>
      //       (e: Expr, in: Map[Identifier, Interval], err: Map[Identifier, Rational]) =>
      //         errorDynamicWithInputRoundoff(e, in, 256)
      //   }
      //   fitnessFunctionName = value

      case FlagOption("rewrite-baseline") =>
        computeBaseline = true

      case FlagOption("rewrite-seed-system-millis") =>
        seed = System.currentTimeMillis

      case ParamOption("rewrite-custom-seed", value) =>
        seed = value.toLong

      case _ => ;
    }

    val infoString = s"fitness function: $fitnessFunctionName, # generations: $maxGenerations, " +
      s"population size: $populationSize, seed: $seed"
    reporter.info(infoString)

    val newDefs = for (fnc <- prg.defs) yield
    if (!fnc.precondition.isEmpty && !fnc.body.isEmpty) {

      reporter.info(s"\nGoing to rewrite ${fnc.id}")

      val allIDs = fnc.params.map(_.id)
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      var inputErrors = allIDs.map {
        id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))
      }.toMap

      val newBody = rewriteExpression(fnc.body.get, fitnessFunction, inputValMap, inputErrors)

      reporter.info("error after: " + fitnessFunction(newBody, inputValMap, inputErrors))
      reporter.debug("expr after: " + newBody)

      if(computeBaseline) {
        val dynamicErrorBefore = errorDynamicWithInputRoundoff(fnc.body.get, inputValMap, baselineDynamicSamples)
        val dynamicErrorAfter = errorDynamicWithInputRoundoff(newBody, inputValMap, baselineDynamicSamples)
        reporter.info(s"dynamic error before: $dynamicErrorBefore, after: $dynamicErrorAfter")

        val improvement = (dynamicErrorBefore - dynamicErrorAfter) / dynamicErrorBefore
        reporter.info(s"improvement: $improvement")
      }

      fnc.copy(body = Some(newBody))

    } else {
      fnc
    }


    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, Program(prg.id, newDefs.toSeq))
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
      (e: Expr) => e.deepCopy,
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
