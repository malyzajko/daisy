package daisy
package opt

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.Trees._
import daisy.lang.Types.FinitePrecisionType
import daisy.lang.{TreeOps, Trees}
import daisy.opt.{CostFunctions, MixedPrecisionOptimizationPhase}
import daisy.search.GeneticSearch
import daisy.tools.FinitePrecision._
import daisy.tools._
import tools.Rational.max
import daisy.{Context, DaisyPhase, _}
import analysis.DataflowPhase

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Seq
import scala.collection.mutable.{Map => MMap}
import scala.util.Random
import scala.collection.parallel.CollectionConverters._

/**
 * A container for results related to regime parts
 *
 * @param intervalMap     The variable intervals used to create this regime part result
 * @param rewrittenBody   The rewritten function body created by the phase that is driven by the RegimeInferencePhase
 * @param evaluationBody  The function body to use for performing a cost evaluation on this regime part
 * @param inputPrecisions The input precisions resulting from the driven phase's execution
 * @param resultPrecision The result precision resulting from the driven phase's execution
 */
case class RegimePartResult(
  intervalMap: Map[Identifier, Interval],
  rewrittenBody: Expr,
  evaluationBody: Expr,
  inputPrecisions: Map[Identifier, Precision],
  resultPrecision: Precision,
  bestError: Option[Rational] = None) // for rewriting

/**
 * An evaluation aggregating multiple results of SingleEvaluators
 *
 * @param max             The maximum value generated from those SingleEvaluators
 * @param weightedAverage A weighted average of results generated from the SingleEvaluators
 * @param min             The minimal value generated from those SingleEvaluators
 * @param offset          The offset used to create this aggregate
 */
case class OffsetAggregateEvaluation(max: Rational, weightedAverage: Rational, min: Rational, offset: Rational)
  extends Ordered[OffsetAggregateEvaluation] {

  override def compare(that: OffsetAggregateEvaluation): Int = {
    this.weightedAverage.compare(that.weightedAverage)
  }

  override def toString: String = s"OffsetAggregateEvaluation(max: $max, avg: $weightedAverage, min: $min, " +
    s"offset: $offset)"

  def toCSV: String = s"$min, $max, $weightedAverage"
}

/**
 * A container class to facilitate genetic search
 *
 * @param regime                    The current regime that is subject to mutations
 * @param optimizationFunction The function that allows applying mixed precision tuning to the regime
 */
case class GeneticSearchContext(regime: Seq[RegimePartResult],
  optimizationFunction: Map[Identifier, Interval] => RegimePartResult,
  rewriting: Boolean = false)

/**
 * A phase that generates a regime split for given functions based on another phase (currently only mixed precision
 * tuning). Stores the created regimes in the context's .regimes property.
 *
 * @author Robert Rabe on 26.03.20.
 */
object RegimeInferencePhase extends DaisyPhase with Subdivision with CostFunctions
  with GeneticSearch[GeneticSearchContext] with Taylor with DynamicEvaluators with RoundoffEvaluators {

  type Regime = Seq[RegimePartResult]
  private type ReturnPrecision = Precision
  private type InputDomain = Map[Identifier, Interval]

  override val name: String = "regime inference phase"
  override val description: String = "A phase to infer regimes for program functions"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption("regime-strategy", Set(
      "naive", // bottom-up with merging of identical bodies
      "bottomUpTree",
      "bottomUp", // bottom-up with merging of different bodies
      "exhaustiveTopDown",
      "genetic",
      "topDown",
      "naiveWithoutMerging", // bottom-up without merging anything (baseline)
      "bottomGenetic", // combination of bottom-up and genetic
      "bottomTop"), // combination of bottom-up and top-down
      "bottomTop", // default
      "Strategy to use for regime-inference"),
    //NumOption("naiveDivLimit", 4, "Div Limit to use in naive strategy"),
    //NumOption("bottomUpTreeDivLimit", 4, "Div Limit to use in bottom up strategy"),
    //NumOption("advancedDivLimit", 4, "Div Limit to use in advanced strategy"),
    NumOption("exhaustiveTopDownDepth", 5, "Top down depth for exhaustive top down strategy"),
    NumOption("topDownDepth", 5, "Top down depth for standard top down strategy"),
    NumOption("populationSize", 10, "Population size for genetic search"), // was: 4
    NumOption("maxGenerations", 10, "Maximum number of generations for genetic search"), // was: 20
    StringChoiceOption("geneticVariableChoice", Set("width", "derivative"), "width",
      "Choose variable to split during genetic search based on this measure"),
    FlagOption("geneticVariableRandomSplit", "Choose a random split point to split variables at instead of splitting in the middle"),
    StringChoiceOption("topDownVariableChoice", Set("all", "widest", "cost"), "cost",
      "Split either all variables of the worst performing regime part or the widest one or by choosing the cheapest variant."),
    NumOption("regime-seed", 123456789, "The seed to use for regime inference with the genetic strategy."),
    FlagOption("regime-rewriting", "Generate regimes with rewriting optimization")
  )

  // Used for genetic search
  override var rand: Random = _

  implicit val debugSection: DebugSectionExperiment.type = DebugSectionExperiment

  private var ctx: Context = _
  private var reporter: Reporter = _

  private var defaultPrecision: Precision = _
  private var rangeMethod: String = ""

  private var costFunction: (Expr, Map[Identifier, Precision]) => Rational = _
  private var offsetFunction: Regime => Rational = _
  private var topDownVariableChoice: String = _
  private var geneticVariableChoice: String = _
  private var geneticSplitVariableRandomly: Boolean = _
  private var exhaustiveTopDownDepth: Int = _
  private var topDownDepth: Int = _
  //private var totalOpt: Int = _

  private val geneticVariableSplitStandardDeviation = 0.15

  /**
   * A cache for regime part results that takes into account the strategy for which the result was cached.
   * Can be used to count the number of distinct executions of mixed precision tunings for a strategy.
   */
  private class RegimePartResultCache {
    /**
     * The cache in which all mixed precision tunings that are performed are put (regardless of their strategy)
     */
    private val baseCache: MMap[InputDomain, RegimePartResult] = TrieMap()
    /**
     * A cache containing all the interval maps for a strategy for which regime parts were created. This is regardless
     * of whether these regime parts were already stored in the baseCache.
     */
    private val strategyCache: MMap[String, Set[InputDomain]] = TrieMap()

    def getOrUpdateForStrategy(key: InputDomain,
      strategy: String,
      op: => RegimePartResult): RegimePartResult = {
      strategyCache.synchronized {
        strategyCache += (strategy -> strategyCache.get(strategy).map(s => s + key).getOrElse(Set(key)))
      }

      baseCache.getOrElseUpdate(key, op)
    }

    def numberOfCachedItems(strategy: String): Int = strategyCache(strategy).size
  }

  override def runPhase(ctx: Context, prog: Trees.Program): (Context, Trees.Program) = {
    val strategy = ctx.option[String]("regime-strategy")

    costFunction = ctx.option[String]("mixed-cost-function") match {
      case "default" | "simple" => simpleMixedPrecisionCost
      case "area" => areaBasedCostFunction
      case "benchmark" => benchmarkedMixedPrecisionCost
      case "benchmarkC" => benchmarkedMixedPrecisionCostForC
    }

    offsetFunction = ctx.option[String]("mixed-cost-function") match {
      case "default" | "simple" => r => Rational(Math.max(r.length - 1, 0))
      case "area" => r => Rational(Math.max(r.length - 1, 0))
      case "benchmark" => r => {
        val regimePartSizeWithSplit = numberOfRegimePartsForNestedSplit(r.map(_.intervalMap))
        Rational(684, 1000) * Rational.fromDouble(Math.log(regimePartSizeWithSplit)) * Rational.fromReal(1.6114) // Benchmarked Plus Cost
      }
      case "benchmarkC" => r => {
        val regimePartSizeWithSplit = numberOfRegimePartsForNestedSplit(r.map(_.intervalMap))
        Rational(783, 10000) * Rational.fromDouble(Math.log(regimePartSizeWithSplit)) * Rational.fromReal(1.019) // Benchmarked Plus Cost
      }
      //case "accuracy" => ??? // run rewriting, penalize too many branches
    }

    defaultPrecision = ctx.option[Precision]("precision")
    rangeMethod = ctx.option[String]("rangeMethod")

    this.ctx = ctx
    reporter = ctx.reporter

    //totalOpt = ctx.option[Long]("totalOpt").toInt

    // TODO: fix next
    exhaustiveTopDownDepth = ctx.option[Long]("exhaustiveTopDownDepth").toInt

    topDownDepth = ctx.option[Long]("topDownDepth").toInt
    topDownVariableChoice = ctx.option[String]("topDownVariableChoice")

    populationSize = ctx.option[Long]("populationSize").toInt
    maxGenerations = ctx.option[Long]("maxGenerations").toInt
    geneticVariableChoice = ctx.option[String]("geneticVariableChoice")
    geneticSplitVariableRandomly = ctx.option[Boolean]("geneticVariableRandomSplit")
    rand = new Random(ctx.option[Long]("regime-seed"))

    MixedPrecisionOptimizationPhase.optimizationMethod = ctx.option[String]("mixed-opt-method")
    RewritingOptimizationPhase.reporter = reporter
    RewritingOptimizationPhase.initSeed = ctx.option[Long]("rewrite-seed")
    FPTunerPhase.reporter = reporter
    RewritingOptimizationPhase.maxGenerations = ctx.option[Long]("rewrite-generations").toInt
    RewritingOptimizationPhase.populationSize = ctx.option[Long]("rewrite-population").toInt

    val regimesAndUpdatedContexts = analyzeConsideredFunctions(ctx, prog) { f =>

      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(f.id)

      // set the total number of regimes to those generated by naive bottom up
      val divLimits = getDivLimit(inputValMap)
      val maxRegimes: Int = totalNumberOfSubdivisions(divLimits)

      val costEvaluationFunction: (InputDomain, Regime) => OffsetAggregateEvaluation =
      if (ctx.hasFlag("regime-rewriting")) { // rewriting
        rewritingCost(_, _, ctx.specInputErrors(f.id), defaultPrecision)
      } else {
        mixedPrecisionCost
      }

      // A cache to save regime part results for variable sub-intervals to reduce redundant computation
      val cache = new RegimePartResultCache()

      // def applyMixedPrecisionTuningForMethod(strategy: String, intervalMap: InputDomain): RegimePartResult = {
      //   cache.getOrUpdateForStrategy(intervalMap, strategy,
      //     applyMixedPrecisionTuningOnSubInterval(intervalMap, f, ctx.specResultErrorBounds.get(f.id)))
      // }

      // def applyFPTunerForMethod(strategy: String, intervalMap: InputDomain): RegimePartResult = {
      //   cache.getOrUpdateForStrategy(intervalMap, strategy,
      //     applyFPTunerOnSubInterval(intervalMap, f, ctx.specResultErrorBounds.get(f.id)))
      // }

      // def applyRewritingForMethod(strategy: String, intervalMap: InputDomain): RegimePartResult = {
      //   cache.getOrUpdateForStrategy(intervalMap, strategy,
      //     applyRewritingOnSubInterval(intervalMap, f, ctx.specResultErrorBounds.get(f.id),
      //       ctx.specInputErrors(f.id), defaultPrecision))
      // }

      // Sort variables by their interval widths (largest first)
      val variablesByIntervalWidth = inputValMap.toSeq.sortBy(-_._2.width).map(_._1).toSeq

      def calculateNumberOfTuningsRemaining(): Int = {
        Math.max(maxRegimes - cache.numberOfCachedItems(strategy), 0)
      }

      val optimizationFunction: InputDomain => RegimePartResult =
        if (ctx.hasFlag("regime-rewriting")) { // rewriting

          //applyRewritingForMethod(strategy, _)
          (intervalMap: InputDomain) =>
            cache.getOrUpdateForStrategy(intervalMap, strategy,
              applyRewritingOnSubInterval(intervalMap, f, ctx.specResultErrorBounds.get(f.id),
                ctx.specInputErrors(f.id), defaultPrecision))


        } else if (ctx.hasFlag("fptuner")) {
          println("Using FPTuner for Regime Inference!")
          //applyFPTunerForMethod(strategy, _)
          (intervalMap: InputDomain) =>
            cache.getOrUpdateForStrategy(intervalMap, strategy,
              applyFPTunerOnSubInterval(intervalMap, f, ctx.specResultErrorBounds.get(f.id)))

        } else {
          //applyMixedPrecisionTuningForMethod(strategy, _)
          (intervalMap: InputDomain) =>
            cache.getOrUpdateForStrategy(intervalMap, strategy,
              applyMixedPrecisionTuningOnSubInterval(intervalMap, f, ctx.specResultErrorBounds.get(f.id)))
        }


      val (initialRegime: Regime, initialEvaluation: OffsetAggregateEvaluation) = strategy match {
        // For the bottom-up strategies, the initial regime is the finest split allowed with the given parameters
        case "naive" | "bottomUp" | "bottomUpTree" | "naiveWithoutMerging" | "bottomGenetic" | "bottomTop" =>
          //val divLimit = ctx.option[Long](s"${strategy}DivLimit").toInt

          //val subIntervals = getEqualSubintervals(ctx.specInputRanges(f.id), divLimit, divParameter = -1, totalOpt)
          val subIntervals = getCustomSubintervals(ctx.specInputRanges(f.id), divLimits)

          //val initialRegime = subIntervals.par.map(optimizationFunction).seq.toSeq
          val initialRegime = subIntervals.map(optimizationFunction).seq.toSeq

          // println("initual regimes:")
          // println(initialRegime.map(r => (r.intervalMap, r.inputPrecisions)).mkString("\n"))

          val initialEvaluation = costEvaluationFunction(ctx.specInputRanges(f.id), initialRegime)

          ctx.reporter.debug(s"[${f.id.name}] Initial size: ${subIntervals.size}")
          ctx.reporter.debug(s"[${f.id.name}] Initial evaluation: $initialEvaluation")

          (initialRegime, initialEvaluation)

        // For the top-down strategies, the initial regime is made up of the full intervals
        case "exhaustiveTopDown" | "topDown" | "genetic" =>
          val initialRegime = Seq(optimizationFunction(ctx.specInputRanges(f.id)))
          val initialResult = costEvaluationFunction(ctx.specInputRanges(f.id), initialRegime)
          (initialRegime, initialResult)
      }

      val resultRegime: Regime = strategy match {


        case "naiveWithoutMerging" =>
          initialRegime

        case "naive" =>
          inferRegimeByMergingDirectNeighbors(initialRegime.toList,
            variablesByIntervalWidth,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            tryToMergeNeighborsWithDifferentBodies = false
          )

        case "bottomUpTree" =>
          var intervals = initialRegime.map(_.intervalMap)

          for (variable <- variablesByIntervalWidth) {
            if (cache.numberOfCachedItems(strategy) < maxRegimes) {
              val (newIntervals, _, _) = combineBottomUpInVariable(
                intervals.map(i => i(variable)).distinct.sortBy(_.xlo),
                intervals,
                variable,
                initialEvaluation.weightedAverage,
                optimizationFunction,
                costEvaluationFunction(ctx.specInputRanges(f.id), _)
              )

              intervals = newIntervals
            }
          }

          val bottomUpMergedRegime = intervals.par
            .map(i => optimizationFunction(i)).seq

          inferRegimeByMergingDirectNeighbors(bottomUpMergedRegime.toList,
            variablesByIntervalWidth,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            tryToMergeNeighborsWithDifferentBodies = false
          )

        case "bottomUp" =>
          inferRegimeByMergingDirectNeighbors(initialRegime.toList,
            variablesByIntervalWidth,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            tryToMergeNeighborsWithDifferentBodies = true
          ) //(calculateNumberOfTuningsRemaining) // do not limit # optimizations

        case "exhaustiveTopDown" =>
          performExhaustiveTopDownSplit(exhaustiveTopDownDepth / 3,
            exhaustiveTopDownDepth - exhaustiveTopDownDepth / 3,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            initialRegime,
            (initialRegime, initialEvaluation.weightedAverage),
            variablesByIntervalWidth)(calculateNumberOfTuningsRemaining)._1

        case "topDown" =>

          if (ctx.hasFlag("regime-rewriting")) {
            performTopDownSplit(optimizationFunction,
              costEvaluationFunction(ctx.specInputRanges(f.id), _),
              p => rewritingCostIndividual(p.intervalMap, p.evaluationBody, ctx.specInputErrors(f.id), defaultPrecision),
              (initialRegime, initialEvaluation.weightedAverage),
              initialRegime,
              topDownDepth,
              maxRegimes
            )._1
          } else {
            performTopDownSplit(optimizationFunction,
              costEvaluationFunction(ctx.specInputRanges(f.id), _),
              p => costFunction(p.evaluationBody, p.inputPrecisions),
              (initialRegime, initialEvaluation.weightedAverage),
              initialRegime,
              topDownDepth,
              maxRegimes
            )._1
          }

        case "bottomTop" =>
          val tmpRegime = inferRegimeByMergingDirectNeighbors(initialRegime.toList,
            variablesByIntervalWidth,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            tryToMergeNeighborsWithDifferentBodies = false
          )
          val tmpEvaluation = costEvaluationFunction(ctx.specInputRanges(f.id), tmpRegime)
          println("running bottom-top")

          if (ctx.hasFlag("regime-rewriting")) {
            performTopDownSplit(optimizationFunction,
              costEvaluationFunction(ctx.specInputRanges(f.id), _),
              p => rewritingCostIndividual(p.intervalMap, p.evaluationBody, ctx.specInputErrors(f.id), defaultPrecision),
              (tmpRegime, tmpEvaluation.weightedAverage),
              tmpRegime,
              topDownDepth,
              maxRegimes
            )._1
          } else {
            performTopDownSplit(optimizationFunction,
              costEvaluationFunction(ctx.specInputRanges(f.id), _),
              p => costFunction(p.evaluationBody, p.inputPrecisions),
              (tmpRegime, tmpEvaluation.weightedAverage),
              tmpRegime,
              topDownDepth,
              maxRegimes
            )._1
          }

        case "genetic" =>
          val initialSearchContext =
            if (ctx.hasFlag("regime-rewriting")) {
              GeneticSearchContext(initialRegime, optimizationFunction, rewriting=true)
            } else {
              GeneticSearchContext(initialRegime, optimizationFunction)
            }

          val fitnessFunction = (r: GeneticSearchContext) =>
            if (r.regime.size > maxRegimes) {  // penalize too large regimes
              10 * costEvaluationFunction(ctx.specInputRanges(f.id), r.regime).weightedAverage
            } else {
              costEvaluationFunction(ctx.specInputRanges(f.id), r.regime).weightedAverage
            }

          val (bestRegimeGeneticContext, _) = runGenetic(initialSearchContext, fitnessFunction)

          inferRegimeByMergingDirectNeighbors(bestRegimeGeneticContext.regime.toList,
            variablesByIntervalWidth,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            tryToMergeNeighborsWithDifferentBodies = false
          )

        case "bottomGenetic" =>
          val tmpRegime = inferRegimeByMergingDirectNeighbors(initialRegime.toList,
            variablesByIntervalWidth,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            tryToMergeNeighborsWithDifferentBodies = false
          )

          val initialSearchContext =
            if (ctx.hasFlag("regime-rewriting")) {
              GeneticSearchContext(tmpRegime, optimizationFunction, rewriting=true)
            } else {
              GeneticSearchContext(tmpRegime, optimizationFunction)
            }

          val fitnessFunction = (r: GeneticSearchContext) =>
            if (r.regime.size > maxRegimes) {  // penalize too large regimes
              10 * costEvaluationFunction(ctx.specInputRanges(f.id), r.regime).weightedAverage
            } else {
              costEvaluationFunction(ctx.specInputRanges(f.id), r.regime).weightedAverage
            }

          val (bestRegimeGeneticContext, _) = runGenetic(initialSearchContext, fitnessFunction)

          inferRegimeByMergingDirectNeighbors(bestRegimeGeneticContext.regime.toList,
            variablesByIntervalWidth,
            optimizationFunction,
            costEvaluationFunction(ctx.specInputRanges(f.id), _),
            tryToMergeNeighborsWithDifferentBodies = false
          )

      }

      ctx.reporter.debug(s"[${f.id.name}] $strategy body size: ${resultRegime.size}")
      val resultRegimeEvaluation = costEvaluationFunction(ctx.specInputRanges(f.id), resultRegime)
      ctx.reporter.debug(s"[${f.id.name}] $strategy body eval: $resultRegimeEvaluation")

      val resultRegimeFinal: (Regime, Map[Identifier, Precision], ReturnPrecision) =
        if (ctx.hasFlag("regime-rewriting")) {
          // // need to apply finite precision
          // val typeConfig = lang.TreeOps.allVariablesOf(f.body.get).map(id => (id -> defaultPrecision)).toMap

          // resultRegime.map(reg => reg.copy(
          //   rewrittenBody = MixedPrecisionOptimizationPhase.applyFinitePrecision(reg.rewrittenBody, typeConfig)(defaultPrecision)))

          (resultRegime, Map(), defaultPrecision)
        } else {
          introduceCastsForInputAndReturnValues(f.params.map(_.id), resultRegime, ctx.specInputPrecisions(f.id))
        }

      ctx.reporter.debug(s"[${f.id.name}] $strategy final body size: ${resultRegimeFinal._1.size}")
      val finalEvaluation = costEvaluationFunction(ctx.specInputRanges(f.id), resultRegimeFinal._1)
      ctx.reporter.debug(s"[${f.id.name}] $strategy final body eval: $finalEvaluation")

      ctx.reporter.debug(s"[${f.id.name}] Strategy: $strategy. Number of distinct mixed precision optimizations: ${cache.numberOfCachedItems(strategy)}")

      // println("final regimes:")
      // println(resultRegimeFinal._1.map(r => (r.intervalMap, r.inputPrecisions)).mkString("\n"))

      // sanity checks
      assert(resultRegimeFinal._1.size <= maxRegimes, "Max. number of regimes exceeded!")

      resultRegimeFinal

    }

    val regimes = regimesAndUpdatedContexts.mapValues(_._1.toSeq).toMap

    val specInputPrecisions = regimesAndUpdatedContexts.map {
      case (id, (_, inputPrecisions, _)) => (id, inputPrecisions)
    }

    val specResultPrecisions = regimesAndUpdatedContexts.map {
      case (id, (_, _, returnPrecision)) => (id, returnPrecision)
    }

    val combinedContext = ctx.copy(
      regimes = regimes,
      specInputPrecisions = specInputPrecisions,
      specResultPrecisions = specResultPrecisions
    )

    val renamedProgram = prog.copy(id = FreshIdentifier(s"${prog.id.toString}", prog.id.getType))

    (combinedContext, renamedProgram)
  }

  /**
   *
   * @param eagerLevel                The remaining depth for which all possibilities should be eagerly explored
   * @param heuristicLevel            The remaining depth for which only some of the possibilities should be explored
   *                                  (after the eagerLevel has been used up)
   * @param optimizationFunction A function that applies mixed precision tuning to InputDomain
   * @param costEvaluationFunction        A function that computes the mixed precision cost of a regime
   * @param regime                    The regime on which the top down split should be performed
   * @param bestResult                The best result that was encountered (a tuple of the regime and corresponding cost)
   * @param variablesByIntervalWidth  Sorted list of considered variables by their interval widths (descending)
   * @return The best encountered regime and its cost
   */
  private def performExhaustiveTopDownSplit(eagerLevel: Int,
    heuristicLevel: Int,
    optimizationFunction: InputDomain => RegimePartResult,
    costEvaluationFunction: Regime => OffsetAggregateEvaluation,
    regime: Regime,
    bestResult: (Regime, Rational),
    variablesByIntervalWidth: Seq[Identifier])(implicit numberOfRemainingTuningsLeft: () => Int = () => Int.MaxValue): (Regime, Rational) = {

    (numberOfRemainingTuningsLeft(), eagerLevel, heuristicLevel) match {
      // We are not allowed to perform any further splits.
      case (0, _, _) => bestResult
      case (_, 0, 0) => bestResult
      // We have exhausted our exhaustive depth and will now only expand the best-performing options
      case (_, 0, heuristicLevel) =>

        var promisingRegimes: Set[Regime] = Set()
        var (updatedBestRegime, updatedBestCost) = bestResult

        for (regimePartToSplit <- regime) {
          for (variable <- variablesByIntervalWidth) {
            val interval = regimePartToSplit.intervalMap(variable)
            val Seq(firstPart, secondPart) = interval.divide(2)
            val firstRegimePart = optimizationFunction(regimePartToSplit.intervalMap + (variable -> firstPart))
            val secondRegimePart = optimizationFunction(regimePartToSplit.intervalMap + (variable -> secondPart))

            val regimeWithoutSplitPart = regime.filterNot(_ == regimePartToSplit)

            val (withSplit, withSplitCost) = if (firstRegimePart.rewrittenBody == secondRegimePart.rewrittenBody) {
              val newRegime = firstRegimePart.copy(intervalMap = regimePartToSplit.intervalMap) +: regimeWithoutSplitPart
              val newRegimeCost = costEvaluationFunction(newRegime).weightedAverage

              (newRegime, newRegimeCost)
            } else {
              val splitRegime = firstRegimePart +: secondRegimePart +: regimeWithoutSplitPart
              val splitRegimeCost = costEvaluationFunction(splitRegime).weightedAverage

              (splitRegime, splitRegimeCost)
            }

            if (withSplitCost < bestResult._2) {
              updatedBestCost = withSplitCost
              updatedBestRegime = withSplit

              promisingRegimes += withSplit
            }
          }
        }

        reporter.debug(s"Running heuristic top down on ${promisingRegimes.size} regimes!")

        promisingRegimes.foldLeft((updatedBestRegime, updatedBestCost)) {
          case ((updatedBestRegime, updatedBestCost), promisingRegime) =>
            val (furtherSplit, furtherSplitCost) = performExhaustiveTopDownSplit(0,
              heuristicLevel - 1,
              optimizationFunction,
              costEvaluationFunction,
              promisingRegime,
              (updatedBestRegime, updatedBestCost),
              variablesByIntervalWidth)

            if (furtherSplitCost < updatedBestCost) {
              (furtherSplit, furtherSplitCost)
            } else {
              (updatedBestRegime, updatedBestCost)
            }
        }

      // We still have some eager depth available and will expand all possibilities (regardless of their performance)
      case _ =>
        var (updatedBestRegime, updatedBestCost) = bestResult

        for (regimePartToSplit <- regime) {
          for (variable <- variablesByIntervalWidth) {
            val interval = regimePartToSplit.intervalMap(variable)
            val Seq(firstPart, secondPart) = interval.divide(2)
            val firstRegimePart = optimizationFunction(regimePartToSplit.intervalMap + (variable -> firstPart))
            val secondRegimePart = optimizationFunction(regimePartToSplit.intervalMap + (variable -> secondPart))

            val regimeWithoutSplitPart = regime.filterNot(_ == regimePartToSplit)

            val (withSplit: Regime, withSplitCost: Rational) =
              if (firstRegimePart.rewrittenBody == secondRegimePart.rewrittenBody) {
                val newRegime = firstRegimePart.copy(intervalMap = regimePartToSplit.intervalMap) +: regimeWithoutSplitPart
                val newRegimeCost = costEvaluationFunction(newRegime).weightedAverage

                (newRegime, newRegimeCost)
              } else {
                val splitRegime = firstRegimePart +: secondRegimePart +: regimeWithoutSplitPart
                val splitRegimeCost = costEvaluationFunction(splitRegime).weightedAverage

                (splitRegime, splitRegimeCost)
              }

            if (withSplitCost < updatedBestCost) {
              updatedBestCost = withSplitCost
              updatedBestRegime = withSplit
            }

            val (furtherSplit: Regime, furtherSplitCost: Rational) = performExhaustiveTopDownSplit(eagerLevel - 1,
              heuristicLevel,
              optimizationFunction,
              costEvaluationFunction, firstRegimePart +: secondRegimePart +: regimeWithoutSplitPart,
              (updatedBestRegime, updatedBestCost),
              variablesByIntervalWidth)

            if (furtherSplitCost < updatedBestCost) {
              updatedBestCost = furtherSplitCost
              updatedBestRegime = furtherSplit
            }
          }
        }

        (updatedBestRegime, updatedBestCost)
    }
  }

  /**
   * Perform a regime split top-down by iteratively improving upon a regime. Here, the worst regime part is always
   * updated in some way (depending on the topDownVariableChoice property).
   *
   * @param optimizationFunction A function that applies mixed precision tuning to InputDomain
   * @param costEvaluationFunction        A function that computes the mixed precision cost of a regime
   * @param partCostEvaluationFunction    A function that computes the mixed precision cost of a regime part
   * @param bestResult                The current best result (a tuple of regime and the corresponding cost)
   * @param workList                  The work list of regime parts that remain to be improved
   * @param remainingDepth            The remaining recursive depth
   * @return The best possible regime split achievable using the top down split
   */
  @scala.annotation.tailrec
  private def performTopDownSplit(
    optimizationFunction: InputDomain => RegimePartResult,
    costEvaluationFunction: Regime => OffsetAggregateEvaluation,
    partCostEvaluationFunction: RegimePartResult => Rational,
    bestResult: (Regime, Rational),
    workList: Regime,
    remainingDepth: Int,
    maxRegimes: Int): (Regime, Rational) = {

    if (remainingDepth <= 0 || workList.isEmpty) {

      bestResult

    } else {
      // The work list regime parts sorted by cost descending
      val workListByCost = workList.sortBy(-partCostEvaluationFunction(_))

      workListByCost match {
        //case Seq() => bestResult
        case head +: tail =>
          val (bestRegime, bestEvaluation) = bestResult
          val subdomainsAsRegimePartResults = topDownVariableChoice match {
            case "all" =>
              // TODO: review whether this still makes sense
              val newSubdomains = getEqualSubintervals(head.intervalMap, divLimit = 2,
                totalOpt = Math.min(maxRegimes, Math.pow(2, head.intervalMap.keys.size).toInt))

              //newSubdomains.par.map(optimizationFunction).seq
              newSubdomains.map(optimizationFunction).seq

            case "widest" =>
              val (variable, interval) = head.intervalMap.toList.maxBy(_._2.width)
              val newSubdomains = interval.divide(2).map(subinterval => head.intervalMap + (variable -> subinterval))

              //newSubdomains.par.map(optimizationFunction).seq
              newSubdomains.map(optimizationFunction).seq

            case "cost" =>
              //head.intervalMap.toList.par.map {
              head.intervalMap.toList.map {
                case (variable, interval) =>
                  val withSplit = interval.divide(2)
                    .map(subinterval => head.intervalMap + (variable -> subinterval))
                    .map(optimizationFunction)

                  (withSplit, withSplit.map(partCostEvaluationFunction).foldLeft(Rational.zero) { (a, b) => a + b })
              }.seq
                .minBy(_._2)
                ._1
          }
          // println("split regime:")
          // println(subdomainsAsRegimePartResults.map(r => (r.intervalMap, r.inputPrecisions)).mkString("\n"))

          val newRegime = bestRegime.filterNot(_ == head) ++ subdomainsAsRegimePartResults
          val newRegimeCost = costEvaluationFunction(newRegime).weightedAverage

          if (newRegime.size > maxRegimes) {
            // we have exhausted the upper limit on regimes
            bestResult

          } else if (newRegimeCost < bestEvaluation) {
            performTopDownSplit(optimizationFunction,
              costEvaluationFunction,
              partCostEvaluationFunction,
              (newRegime, newRegimeCost),
              tail ++ subdomainsAsRegimePartResults,
              remainingDepth - 1,
              maxRegimes)
          } else {
            performTopDownSplit(optimizationFunction,
              costEvaluationFunction,
              partCostEvaluationFunction,
              bestResult,
              tail,
              remainingDepth - 1,
              maxRegimes)
          }
      }
    }
  }

  /**
   * Combine regime parts in a tree-like bottom-up manner in one variable
   *
   * @param rangesInVariable          The different ranges that exist for a variable (sorted in ascending order)
   * @param intervals                 The regime intervals
   * @param variable                  The variable in which to merge intervals
   * @param currentCostEvaluation     The cost evaluation for the current regime intervals
   * @param optimizationFunction A function that performs mixed precision tuning on one subinterval map
   * @param costEvaluationFunction        The mixed precision cost function for a regime
   * @return A tuple of (updated interval map, evaluation cost of the updated interval map,
   *         whether or not all subparts were merged)
   */
  def combineBottomUpInVariable(rangesInVariable: Seq[Interval],
    intervals: Seq[Map[Identifier, Interval]],
    variable: Identifier,
    currentCostEvaluation: Rational,
    optimizationFunction: InputDomain => RegimePartResult,
    costEvaluationFunction: Regime => OffsetAggregateEvaluation): (Seq[Map[Identifier, Interval]], Rational, Boolean) = {

    /*
     * Given a sorted sequence of possible intervals a variable may have, try to merge neighboring intervals of that
     * variable in a bottom-up manner and replace all occurrences of the intervals used in the merger by their merged
     * product if the merged version performs better or at least as good as the non-merged.
     *
     * Example:
     *
     * Initial interval split:
     *
     * [a: [0, 1], b: [5, 6]], [a: [1, 2], b: [5, 6]], [a: [2, 3], b: [5, 6]], [a: [3, 4], b: [5, 6]],
     * [a: [0, 1], b: [6, 7]], [a: [1, 2], b: [6, 7]], [a: [2, 3], b: [6, 7]], [a: [3, 4], b: [6, 7]]
     *
     * Intervals in Variable a:
     *
     *   [0 , 1]   [1 , 2]     [2 , 3]   [3 , 4]
     *      \        /            \        /
     *       \      /              \      /
     *        \    /                \    /
     *        [0 , 2]             [2 , 4]
     *               \           /
     *                \         /
     *                 \       /
     *                  [0, 4]
     *
     * Assuming all of the combinations above yield overall cheaper regime splits, the resulting regime will look like:
     * [a: [0, 4], b: [5, 6]], [a: [0, 4], b: [6, 7]]
     *
     * If merging intervals does not result in an improved regime split on one branch of the tree, we will not be able
     * to merge its parent with the parent's siblings.
     */

    rangesInVariable match {
      case Seq(a, b) =>
        // We have arrived at the sibling leaves of the tree. We will try to merge these.

        val mergedInterval = Interval(a.xlo, b.xhi)

        // Replace all occurrences of intervals a or b with the result of merging them and make sure there is only one
        // of each of the resulting interval maps.
        val updatedIntervals = intervals.map(interval =>
          if (interval(variable) == a || interval(variable) == b) {
            interval + (variable -> mergedInterval)
          } else {
            interval
          }
        ).distinct

        val mergedRegimeParts = updatedIntervals.par
          .map(optimizationFunction)
          .seq

        val mergedResult = costEvaluationFunction(mergedRegimeParts).weightedAverage

        if (mergedResult <= currentCostEvaluation) {
          // If the merged intervals have lesser (or equal cost) compared to the current cost,
          // return the merged results and indicate that merging improved the result.
          (updatedIntervals, mergedResult, true)
        } else {
          // Else, return the original intervals and indicate that merging did not improve anything.
          (intervals, currentCostEvaluation, false)
        }

      case Seq(_) =>
        (intervals, currentCostEvaluation, false)

      case _ =>
        // Split the current variable ranges in two
        val (leftIntervals, rightIntervals) = rangesInVariable.splitAt(rangesInVariable.length / 2)

        // Merge the left subtree as far as possible
        val (leftUpdatedIntervals, leftUpdatedEvaluation, leftCombinationSucceeded) =
          combineBottomUpInVariable(leftIntervals,
            intervals,
            variable,
            currentCostEvaluation,
            optimizationFunction,
            costEvaluationFunction)

        // Based on the result of the left merger, merge the right subtree as far as possible
        val (rightUpdatedIntervals, rightUpdatedEvaluation, rightCombinationSucceeded) =
          combineBottomUpInVariable(rightIntervals,
            leftUpdatedIntervals,
            variable,
            leftUpdatedEvaluation,
            optimizationFunction,
            costEvaluationFunction)

        if (leftCombinationSucceeded && rightCombinationSucceeded) {
          // If all left and right children have been merged, try to merge the two resulting intervals
          val leftInterval = Interval(leftIntervals.map(_.xlo).min, leftIntervals.map(_.xhi).max)
          val rightInterval = Interval(rightIntervals.map(_.xlo).min, rightIntervals.map(_.xhi).max)

          combineBottomUpInVariable(Seq(leftInterval, rightInterval),
            rightUpdatedIntervals,
            variable,
            rightUpdatedEvaluation,
            optimizationFunction,
            costEvaluationFunction)
        } else {
          (rightUpdatedIntervals, rightUpdatedEvaluation, false)
        }
    }
  }


  /**
   * Greedily combine neighboring intervals that have the same bodies
   *
   * @param regime                                 The regime for which neighboring same-bodied intervals should be combined
   * @param variables                              The variables for which neighboring same-bodied intervals should be combined
   * @param optimizationFunction              A function that performs mixed precision tuning on one subinterval map
   * @param costEvaluationFunction                     The mixed precision cost function for a regime
   * @param tryToMergeNeighborsWithDifferentBodies Whether or not neighbors that have different bodies should be merged
   *                                               via mixed precision tuning (if the merged performance is an improvement)
   * @return The result of representing the regime in as few parts as possible
   */
  @scala.annotation.tailrec
  def inferRegimeByMergingDirectNeighbors(regime: List[RegimePartResult],
    variables: Seq[Identifier],
    optimizationFunction: InputDomain => RegimePartResult,
    costEvaluationFunction: Regime => OffsetAggregateEvaluation,
    tryToMergeNeighborsWithDifferentBodies: Boolean)(implicit numberOfRemainingTuningsLeft: () => Int = () => Int.MaxValue): Regime = {

    /*

      If tryToMergeNeighborsWithDifferentBodies is false:

      Merges neighboring intervals that have the same body. Finding the optimal solution to this is an NP-complete
      problem: Exact-Set-Cover. Our goal is for each possible regime part we are handed to be situated in *exactly one*
      regime part after the combination transformation. If that were not the case, there would be ambiguity about the
      weighting and costs associated with regime parts.

      Therefore, we only allow regime parts to be reused in combinations along one variable axis.

      In the example below (ORIGINAL) that means we can combine (a -> (0,1), b -> (0, 1)), (a -> (1,2), b -> (0, 1)) and
      (a -> (2,3), b -> (0, 1)) to (a -> (0,2), b -> (0, 1)) and (a -> (1,3), b -> (0, 1)). Since the variable order
      stays the same across recursive calls, we would then combine those to (a -> (0,3), b -> (0, 1)) (VARIANT 1).

      If, instead, we were to first handle variable b, we would combine (a -> (1,2), b -> (0, 1)) and
      (a -> (1,2), b -> (1, 2)) to (a -> (1,2), b -> (0, 2)) (VARIANT 2).

      This method does not aim to find the smallest number of regime parts containing all provided regime parts exactly
      once (meaning in some cases something of form VARIANT 2 might be produced even though VARIANT 1 was possible),
      however it does promise to not worsen the regime part split.

              ORIGINAL                                VARIANT 1                              VARIANT 2

             Variable b                              Variable b                             Variable b
         0        1        2                     0        1        2                    0        1        2
       0 +-----------------+                   0 +-----------------+                  0 +-----------------+
         |        |        |   V                 |        |        |   V                |        |        |   V
         | X      |     Y  |   a                 |        |     Y  |   a                | X      |     Y  |   a
         |        |        |   r                 |        |        |   r                |        |        |   r
       1 +-----------------+   i               1 +        |--------+   i              1 +-----------------+   i
         |        |        |   a                 |        |        |   a                |                 |   a
         | X      |     X  |   b                 | X      |     X  |   b                |        X        |   b
         |        |        |   l                 |        |        |   l                |                 |   l
       2 +-----------------+   e               2 +        |--------+   e              2 +-----------------+   e
         |        |        |                     |        |        |                    |        |        |
         | X      |     Y  |   a                 |        |     Y  |   a                | X      |     Y  |   a
         |        |        |                     |        |        |                    |        |        |
       3 +-----------------+                   3 +-----------------+                  3 +-----------------+

       If tryToMergeNeighborsWithDifferentBodies is true:

       Applies exactly the same merging strategy as detailed above for regime parts that are assigned the same bodies.
       For neighboring regime parts that are assigned _different_ bodies, this strategy performs a mixed precision
       tuning on the merged regime interval map and continues using that merged result if its cost is less than the cost
       of keeping the regime parts separated.
     */

    if (variables.isEmpty) {
      regime
    } else {

      val variableToMergeBy = variables.head

      // Map from interval maps ignoring one variable to corresponding regime parts
      var partsWithSameIntervalsExceptInVariable: Map[InputDomain, List[RegimePartResult]] = regime
        .map(part => (part.intervalMap - variableToMergeBy, part))
        .groupBy(_._1)
        .transform((_, v) => v.map(_._2).sortBy(_.intervalMap(variableToMergeBy).xlo))

      var workList = regime
      var handled: Set[RegimePartResult] = Set()

      while (workList.nonEmpty) {
        var current = workList.head

        // Remove the current part from the neighbor map.
        partsWithSameIntervalsExceptInVariable = partsWithSameIntervalsExceptInVariable
          .transform {
            case (_, value) => value.filterNot(_ == current)
          }

        val possibleNeighbors: List[RegimePartResult] =
          partsWithSameIntervalsExceptInVariable(current.intervalMap - variableToMergeBy)

        var currentIntervalInVariable = current.intervalMap(variableToMergeBy)
        var usedInMerge: List[RegimePartResult] = List()

        val bottomNeighborIndex = possibleNeighbors
          .indexWhere(_.intervalMap(variableToMergeBy).xhi == currentIntervalInVariable.xlo)

        val topNeighborIndex = possibleNeighbors
          .indexWhere(_.intervalMap(variableToMergeBy).xlo == currentIntervalInVariable.xhi,
            from = Math.max(bottomNeighborIndex, 0))

        var didMergeBottomNeighbor = false
        var didMergeTopNeighbor = false

        if (bottomNeighborIndex >= 0) {
          val bottomNeighbor = possibleNeighbors(bottomNeighborIndex)
          if (workList.contains(bottomNeighbor) && bottomNeighbor.rewrittenBody == current.rewrittenBody) {
            currentIntervalInVariable = currentIntervalInVariable
              .copy(xlo = bottomNeighbor.intervalMap(variableToMergeBy).xlo)
            current = current.copy(intervalMap = current.intervalMap + (variableToMergeBy -> currentIntervalInVariable))
            usedInMerge :+= bottomNeighbor
            didMergeBottomNeighbor = true
          }
        }

        if (topNeighborIndex >= 0) {
          val topNeighbor = possibleNeighbors(topNeighborIndex)
          if (workList.contains(topNeighbor) && topNeighbor.rewrittenBody == current.rewrittenBody) {
            currentIntervalInVariable = currentIntervalInVariable
              .copy(xhi = topNeighbor.intervalMap(variableToMergeBy).xhi)
            current = current.copy(intervalMap = current.intervalMap + (variableToMergeBy -> currentIntervalInVariable))
            usedInMerge :+= topNeighbor
            didMergeTopNeighbor = true
          }
        }

        if (tryToMergeNeighborsWithDifferentBodies && !didMergeBottomNeighbor && bottomNeighborIndex >= 0
          && numberOfRemainingTuningsLeft() > 0) {
          // We are allowed to merge neighbors that do not have the same body, we have not yet merged the bottom
          // neighbor into our current interval and there exists a bottom neighbor.

          val bottomNeighbor = possibleNeighbors(bottomNeighborIndex)

          // Construct the interval that would result from merging the current interval and the bottom neighbor
          // in the current variable.
          val potentialNewInterval = currentIntervalInVariable
            .copy(xlo = bottomNeighbor.intervalMap(variableToMergeBy).xlo)

          // Apply mixed precision tuning to the interval map that results from performing the merge
          val regimePartResultOfMerge = optimizationFunction(current.intervalMap +
            (variableToMergeBy -> potentialNewInterval))

          val regimeWithoutMerged: Regime = workList.tail.filterNot(usedInMerge.contains) ++ handled :+ current

          val filteredWorkList = workList.tail.filter(x => x != bottomNeighbor && !usedInMerge.contains(x))
          val regimeWithMerged = regimePartResultOfMerge +: (filteredWorkList ++ handled.toList)

          assert(regimeWithMerged.length == regimeWithoutMerged.length - 1,
            "Merging should decrease the number of parts in the regime by exactly one.")

          val costWithoutMerge = costEvaluationFunction(regimeWithoutMerged)
          val costWithMerge = costEvaluationFunction(regimeWithMerged)

          // Is the cost of having merged the two neighbors and having produced a new regime part less than not merging?
          if (costWithMerge <= costWithoutMerge) {
            currentIntervalInVariable = potentialNewInterval
            usedInMerge :+= bottomNeighbor
            current = regimePartResultOfMerge
            didMergeBottomNeighbor = true
          }
        }

        if (tryToMergeNeighborsWithDifferentBodies && !didMergeTopNeighbor && topNeighborIndex >= 0
          && numberOfRemainingTuningsLeft() > 0) {
          // We are allowed to merge neighbors that do not have the same body, we have not yet merged the top
          // neighbor into our current interval and there exists a top neighbor.

          val topNeighbor = possibleNeighbors(topNeighborIndex)

          // Construct the interval that would result from merging the current interval and the top neighbor
          // in the current variable.
          val potentialNewInterval = currentIntervalInVariable
            .copy(xhi = topNeighbor.intervalMap(variableToMergeBy).xhi)

          // Since we might have previously merged with the bottom neighbor, the current rewritten body might have
          // changed and we might be able to trivially merge the topNeighbor with the current interval.
          if (didMergeBottomNeighbor && topNeighbor.rewrittenBody == current.rewrittenBody) {
            currentIntervalInVariable = potentialNewInterval
            current = current.copy(intervalMap = current.intervalMap + (variableToMergeBy -> currentIntervalInVariable))
            usedInMerge :+= topNeighbor
            didMergeTopNeighbor = true
          } else {
            // Apply mixed precision tuning to the interval map that results from performing the merge
            val regimePartResultOfMerge = optimizationFunction(current.intervalMap +
              (variableToMergeBy -> potentialNewInterval))

            val regimeWithoutMerged: Regime = workList.tail.filterNot(usedInMerge.contains) ++ handled :+ current

            val filteredWorkList = workList.tail.filter(x => x != topNeighbor && !usedInMerge.contains(x))
            val regimeWithMerged = regimePartResultOfMerge +: (filteredWorkList ++ handled.toList)

            val costWithoutMerge = costEvaluationFunction(regimeWithoutMerged)
            val costWithMerge = costEvaluationFunction(regimeWithMerged)

            assert(regimeWithMerged.length == regimeWithoutMerged.length - 1,
              "Merging should decrease the number of parts in the regime by exactly one.")

            // Is the cost of having merged the two neighbors and having produced a new regime part less than not merging?
            if (costWithMerge <= costWithoutMerge) {
              currentIntervalInVariable = potentialNewInterval
              usedInMerge :+= topNeighbor
              current = regimePartResultOfMerge
              didMergeTopNeighbor = true
            }
          }
        }

        // Remove parts that were merged from the neighbor map and the work list. Since they were already merged, all
        // future merges should be done in their merged product (i.e. the current part).
        workList = workList.tail.diff(usedInMerge)
        partsWithSameIntervalsExceptInVariable = partsWithSameIntervalsExceptInVariable
          .transform {
            case (_, value) => value.filterNot(usedInMerge.contains)
          }

        if (usedInMerge.nonEmpty) {
          workList = current +: workList
        } else {
          handled += current
        }
      }

      inferRegimeByMergingDirectNeighbors(handled.toList,
        variables.tail,
        optimizationFunction,
        costEvaluationFunction,
        tryToMergeNeighborsWithDifferentBodies
      )
    }
  }

  /**
   * Mutate a regime by splitting one of the regime parts along one variable at a random split point
   *
   * @param expr The genetic search context containing the regime that should be mutated
   * @return A genetic search context containing the mutated regime
   */
  override def mutate(expr: GeneticSearchContext): GeneticSearchContext = {
    val regime = expr.regime
    // Sort by cost (largest first)
    val sortedByCost = if (expr.rewriting) {
        regime.sortBy(p => -p.bestError.get)
      } else {
        regime.sortBy(p => -costFunction(p.evaluationBody, p.inputPrecisions))
      }
    val partToMutate: RegimePartResult = rankedChoice(sortedByCost)

    val sortedVariables = geneticVariableChoice match {
      case "width" => partToMutate.intervalMap.toList.sortBy(-_._2.width).map(_._1)
      case "derivative" => estimateMaximumAbsDerivative(partToMutate.evaluationBody, partToMutate.intervalMap)
        .sortBy(-_._2).map(_._1)
    }

    val variableToSplitAt: Identifier = rankedChoice(sortedVariables)
    val interval = partToMutate.intervalMap(variableToSplitAt)

    // Split at least 5% from top or bottom -> smaller splits are not likely to make sense
    val minimumSplitPercentage = 0.05
    val splitPercentage = if (geneticSplitVariableRandomly) {
      Math.min(
        Math.max(0.5 + geneticVariableSplitStandardDeviation * rand.nextGaussian(), minimumSplitPercentage),
        1 - minimumSplitPercentage
      )
    } else {
      0.5
    }

    val pointToSplitAt = interval.xlo + interval.width * splitPercentage

    val otherParts: Regime = regime.filterNot(_ == partToMutate)
    val mutated: Regime = Seq(
      partToMutate.intervalMap + (variableToSplitAt -> Interval(interval.xlo, pointToSplitAt)),
      partToMutate.intervalMap + (variableToSplitAt -> Interval(pointToSplitAt, interval.xhi))
    ).map(expr.optimizationFunction).seq
    //).par.map(expr.optimizationFunction).seq

    expr.copy(regime = otherParts ++ mutated)
  }

  /**
   * Select choices randomly based on rank:
   * [1] Whitley, D. (1989). The GENITOR algorithm and selection pressure: Why rank-based allocation of reproductive trials is best.
   * Proceedings of the Third International Conference on Genetic Algorithms, 1, 116–121.
   *
   * @param sortedChoices The choices sorted by their rank (descending)
   * @param alpha         The alpha parameter of the geometric distribution [1]
   * @return The selected choice
   */
  private def rankedChoice[T](sortedChoices: Seq[T], alpha: Double = 0.3): T = {
    val weights = (1 to sortedChoices.size).map(j => alpha * Math.pow(1 - alpha, j - 1))
    val weightSum = weights.sum
    val probabilities = weights.map(_ / weightSum)

    val r = rand.nextDouble()
    val index = indexForChoiceAccordingToDistribution(r, probabilities)

    sortedChoices(index)
  }

  /**
   * Return the index i of the first probability for which holds: sum(prob(j) | j < i) <= r <= sum(prob(j) | j <= i)
   */
  @scala.annotation.tailrec
  private def indexForChoiceAccordingToDistribution(r: Double,
    probabilities: Seq[Double],
    currentTotal: Double = 0,
    currentIndex: Int = 0): Int = {

    val cumulativeTotalWithFirst = currentTotal + probabilities.head

    if (currentTotal <= r && r <= cumulativeTotalWithFirst) {
      currentIndex
    } else {
      indexForChoiceAccordingToDistribution(r, probabilities.tail, cumulativeTotalWithFirst, currentIndex + 1)
    }
  }

  /**
   * Estimate the maximum absolute value fo the derivative on the input interval map
   *
   * @param expr      The expression that should be derived
   * @param inputMaps The input intervals for which the current derivative should be estimated
   * @return The absolute value of the maximum partial derivative by each input variable
   */
  private def estimateMaximumAbsDerivative(expr: Expr,
    inputMaps: Map[Identifier, Interval]): Seq[(Identifier, Rational)] = {

    val variables = inputMaps.keys
    val derivatives = variables.map(v => (v, getPartialDerivative(expr, v)))

    derivatives.par
      .map {
        case (v, d) =>
          val (range, _) = evalRange[MPFRAffineForm](d, inputMaps.map(x => (x._1 -> MPFRAffineForm(x._2))), MPFRAffineForm.apply)
          (v, Interval.maxAbs(range.toInterval))
      }.seq.toSeq
  }

  /**
   * Create a regime part from a subinterval map by applying the mixed precision optimization phase to it and using
   * the phase's result
   *
   * @param subIntervals The subinterval map to turn into a regime part
   * @param f            The function definition that the phase should operate on
   * @param errorBound   The provided error bound for the function
   * @return The created regime part
   */
  private def applyMixedPrecisionTuningOnSubInterval(subIntervals: InputDomain,
    f: FunDef, errorBound: Option[Rational], timeOut: Int = 300): RegimePartResult = {

    val availablePrecisions = defaultPrecision match {
      case FixedPrecision(b) => // given precision specifies the upper bound
        (1 to b).map(FixedPrecision(_))
      case _ =>
        //Seq(Float32, Float64, Float128)
        Seq(Float64, Float128)
    }

    val (_, intermediateRanges) = DataflowPhase.computeRange(subIntervals, f.body.get, f.precondition.get, rangeMethod)

    var newDef: FunDef = null
    var typeConfig: Map[Identifier, Precision] = Map()
    val future: Future[Unit] = Future {
      val (nD, _, tC) = MixedPrecisionOptimizationPhase.tuneMixedPrecision(f, intermediateRanges,
        availablePrecisions, defaultPrecision, errorBound, reporter, uniformCostFunction = Some(costFunction))
      newDef = nD
      typeConfig = tC
    }
    try {
       Await.result(future, timeOut.second)

    }

    val returnPrecision = newDef.returnType match {
      case FinitePrecisionType(prec) => prec
    }
    // TODO: clean up the code to only keep around the evaluation body and apply finite precision at the very end?
    RegimePartResult(subIntervals, newDef.body.get, f.body.get, typeConfig, returnPrecision)
  }

  private def applyFPTunerOnSubInterval(subIntervals: InputDomain,
    f: FunDef, errorBound: Option[Rational], timeOut: Int = 300): RegimePartResult = {

    val (typeConfig, newDef) = FPTunerPhase.optimizeWithFPTuner(subIntervals, f, errorBound.get)

    val returnPrecision = newDef.returnType match {
      case FinitePrecisionType(prec) => prec
    }
    // TODO: clean up the code to only keep around the evaluation body and apply finite precision at the very end?
    RegimePartResult(subIntervals, newDef.body.get, f.body.get, typeConfig, returnPrecision)
  }

  // does not need the errorBound, but keep it for consistency with rewriting
  /*private def applyRewritingOnSubInterval(subIntervals: InputDomain, f: FunDef,
    errorBound: Option[Rational]): RegimePartResult = {

    val fitnessFunction: Expr => Rational = {(e: Expr) =>
      Rational.fromString(generalErrorDynamicWithInputRoundoff(e, subIntervals, 256).avrgRelError.toString())
    }

    val newBody = RewritingOptimizationPhase.rewriteExpression(f.body.get, fitnessFunction)

    RegimePartResult(subIntervals, newBody, newBody, Map(), defaultPrecision)
  }*/

  private def applyRewritingOnSubInterval(subIntervals: InputDomain, f: FunDef,
    errorBound: Option[Rational], inputErrors: Map[Identifier, Rational],
    uniformPrecision: Precision): RegimePartResult = {

    val fitnessFunction: Expr => Rational = {(e: Expr) =>
      uniformRoundoff_IA_AA_MPFR(e, subIntervals, inputErrors, uniformPrecision, true, true)._1
    }

    val (_, newBody, newError) = RewritingOptimizationPhase.rewriteExpression(f.body.get, fitnessFunction)

    RegimePartResult(subIntervals, newBody, newBody, Map(), defaultPrecision, Some(newError))
  }

  /**
   * Perform a post-processing step after regime derivation for mixed-precision tuning. This determines the main input
   * value precision as well as result precision and introduces casts for the different regime parts where necessary.
   *
   * @param parameters      The parameters for which to possibly introduce casts
   * @param regime          The regime split (including rewritten bodies, etc.)
   * @param inputPrecisions The initial input precisions
   * @return A sequence of tuples of rewritten regime part results, updated input precisions, and updated return value
   */
  def introduceCastsForInputAndReturnValues(parameters: Seq[Identifier],
    regime: Regime,
    inputPrecisions: Map[Identifier, Precision]): (Regime, Map[Identifier, Precision], ReturnPrecision) = {

    assert(regime.nonEmpty, "The regime should be non-empty!")

    val returnPrecisions = regime.map(_.resultPrecision)
    val returnPrecision = returnPrecisions.max

    val (updated: Regime, updatedPrecs: Map[Identifier, Precision]) = parameters.foldLeft((regime, inputPrecisions)) {
      case ((regime, precs), parameter) =>
        val setPrecisions = regime.map(_.inputPrecisions(parameter)).toSet
        if (setPrecisions.size == 1 && returnPrecisions.toSet.size == 1) {
          // Don't need to change anything because all input precisions for this parameter are the same
          (regime, precs + (parameter -> setPrecisions.head))
        } else {
          // Store new identifiers created for a given precision for the current parameter (since we want to keep
          // equality relations).
          val variableReplacementIdentifiers: MMap[Precision, Identifier] = MMap()

          val maximumPrecision = setPrecisions.max
          val updatedRegime = regime.map { part =>
            val regimePartParameterPrecision = part.inputPrecisions(parameter)

            val parameterUpdated = if (regimePartParameterPrecision == maximumPrecision) {
              // If the parameter already has the maximum precision for this part, we do not need to
              // introduce any casts or new variables.
              part
            } else {

              val newIdentifier = variableReplacementIdentifiers
                .getOrElseUpdate(regimePartParameterPrecision,
                  FreshIdentifier(s"_${parameter.name}", FinitePrecisionType(regimePartParameterPrecision)))

              val replacement = TreeOps.replace({
                case Variable(id) if id == parameter => Variable(newIdentifier)
              }) _

              val newRewrittenBody = Let(
                newIdentifier,
                Cast(Variable(parameter.changeType(FinitePrecisionType(maximumPrecision))), newIdentifier.getType),
                replacement(part.rewrittenBody)
              )

              val newEvaluationBody = Let(newIdentifier, Variable(parameter), replacement(part.evaluationBody))

              val newInputPrecisions = part.inputPrecisions + (newIdentifier -> regimePartParameterPrecision) + (parameter -> maximumPrecision)

              part.copy(rewrittenBody = newRewrittenBody,
                evaluationBody = newEvaluationBody,
                inputPrecisions = newInputPrecisions)
            }

            parameterUpdated
          }

          (updatedRegime, precs + (parameter -> maximumPrecision))
        }
    }

    val returnUpdated = updated.map(part =>
      if (part.resultPrecision == returnPrecision) {
        // If the return value already has the correct return precision, we do not need to introduce a cast
        part
      } else {
        part.copy(
          rewrittenBody = TreeOps.updateLastExpression(
            Cast(_, FinitePrecisionType(returnPrecision))
          )(part.rewrittenBody),
          resultPrecision = returnPrecision
        )
      }
    )

    (returnUpdated, updatedPrecs, returnPrecision)
  }

  /**
   * Compute the mixed precision cost for a regime with full input ranges
   *
   * @param inputRanges The input ranges that all regime parts fall into (and must fill out exactly)
   * @param regime      The regime split
   * @return A the cost (in terms of min, max and avg; all including cost offset) for this regime
   */
  def mixedPrecisionCost(inputRanges: InputDomain, regime: Regime): OffsetAggregateEvaluation = {

    // Offset the regime's weight by the number of regime parts there are (to penalize more branches)
    val offset = offsetFunction(regime)

    val (regimeWithCasts, _, _) = introduceCastsForInputAndReturnValues(inputRanges.keys.toSeq, regime, Map())

    val separateEvaluations: Seq[Rational] = regimeWithCasts.map(p => {
      //println(p.evaluationBody)
      //println(p.inputPrecisions)
      costFunction(p.evaluationBody, p.inputPrecisions)
    })

    val weights = regime.map(r =>
      r.intervalMap.map { case (key, interval) =>
        interval.width / inputRanges(key).width
      }.reduce(_ * _)
    )

    assert(weights.reduce(_ + _) == Rational.one,
      s"Regime weights should sum up to one but sum is ${weights.reduce(_ + _)}")

    val weightedAverage: Rational = separateEvaluations.zip(weights).map { case (a, b) => a * b }.reduce(_ + _)

    OffsetAggregateEvaluation(separateEvaluations.max + offset,
      weightedAverage + offset,
      separateEvaluations.min + offset,
      offset
    )
  }

  // TODO: to improve performance, perhaps use the cache to retrieve the already computed error?
  /* def rewritingCost(inputRanges: InputDomain, regime: Regime): OffsetAggregateEvaluation = {

    //val offset = offsetFunction(regime)
    // TODO: check this
    val offset = defaultPrecision.asInstanceOf[FloatPrecision].machineEpsilon * regime.size

    val separateEvaluations: Seq[Rational] = regime.map(p => {
      val measurer = generalErrorDynamicWithInputRoundoff(p.evaluationBody, p.intervalMap, 256)
      Rational.fromString(measurer.avrgRelError.toString())
    })

    val weights = regime.map(r =>
      r.intervalMap.map { case (key, interval) =>
        interval.width / inputRanges(key).width
      }.reduce(_ * _)
    )
    assert(weights.reduce(_ + _) == Rational.one,
      s"Regime weights should sum up to one but sum is ${weights.reduce(_ + _)}")

    val weightedAverage: Rational = separateEvaluations.zip(weights).map { case (a, b) => a * b }.reduce(_ + _)

    OffsetAggregateEvaluation(separateEvaluations.max + offset,
      weightedAverage + offset,
      separateEvaluations.min + offset,
      offset
    )
  } */

  def rewritingCost(inputRanges: InputDomain, regime: Regime,
    inputErrors: Map[Identifier, Rational], uniformPrecision: Precision): OffsetAggregateEvaluation = {

    //val offset = offsetFunction(regime)
    // TODO: check this
    val offset = 0 //defaultPrecision.asInstanceOf[FloatPrecision].machineEpsilon * regime.size

    val absErrors: Seq[Rational] = regime.map(p => {
      p.bestError match {
        case Some(err) => err
        case None => uniformRoundoff_IA_AA_MPFR(p.evaluationBody, p.intervalMap, inputErrors, uniformPrecision, true, true)._1
      }
    })

    OffsetAggregateEvaluation(absErrors.max + offset,
      absErrors.max + offset,
      absErrors.min + offset,
      offset
    )
  }

  def rewritingCostIndividual(inputRanges: InputDomain, expr: Expr,
    inputErrors: Map[Identifier, Rational], uniformPrecision: Precision): Rational = {

    uniformRoundoff_IA_AA_MPFR(expr, inputRanges, inputErrors, uniformPrecision, true, true)._1

  }



  def rewritingCostWeighted(inputRanges: InputDomain, regime: Regime,
    inputErrors: Map[Identifier, Rational], uniformPrecision: Precision): OffsetAggregateEvaluation = {

    //val offset = offsetFunction(regime)
    // TODO: check this
    val offset = 0 // defaultPrecision.asInstanceOf[FloatPrecision].machineEpsilon * regime.size

    val separateEvaluations: Seq[Rational] = regime.map(p => {
      p.bestError match {
        case Some(err) => err
        case None => uniformRoundoff_IA_AA_MPFR(p.evaluationBody, p.intervalMap, inputErrors, uniformPrecision, true, true)._1
      }
    })

    val weights = regime.map(r =>
      r.intervalMap.map { case (key, interval) =>
        interval.width / inputRanges(key).width
      }.reduce(_ * _)
    )
    assert(weights.reduce(_ + _) == Rational.one,
      s"Regime weights should sum up to one but sum is ${weights.reduce(_ + _)}")

    val weightedAverage: Rational = separateEvaluations.zip(weights).map { case (a, b) => a * b }.reduce(_ + _)

    OffsetAggregateEvaluation(separateEvaluations.max + offset,
      weightedAverage + offset,
      separateEvaluations.min + offset,
      offset
    )
  }

  /**
   * Compute the number of regime parts that result from applying variable-wise nested splits, until there
   * remains only one regime part per branch.
   *
   * @param regime The input regime
   * @return The number of regime parts that will be created. This will be greater or equal to the number of input
   *         regime parts
   */
  private def numberOfRegimePartsForNestedSplit(regime: Seq[Map[Identifier, Interval]]): Int = regime match {
    case Seq(_) => 1
    case _ =>
      val variables = regime.flatMap(_.keys).distinct

      val variableToSplitAt = variables
        .filterNot(v => regime.map(_ (v)).distinct.size == 1)
        .maxBy(v => regime.map(_ (v)).distinct.size)

      val possibleSplitPoints: Seq[Rational] = regime
        .flatMap(p => Seq(p(variableToSplitAt).xlo, p(variableToSplitAt).xhi))
        .sorted
        .distinct

      val splitPoint = possibleSplitPoints(possibleSplitPoints.size / 2)

      val (lower, upper) = regime.foldLeft(
        (Seq[Map[Identifier, Interval]](), Seq[Map[Identifier, Interval]]())) {
        case ((lower, upper), part) =>
          val partInterval = part(variableToSplitAt)
          if (partInterval.xlo < splitPoint && partInterval.xhi > splitPoint) {
            val lowerIntervalMap = part + (variableToSplitAt -> Interval(partInterval.xlo, splitPoint))
            val upperIntervalMap = part + (variableToSplitAt -> Interval(splitPoint, partInterval.xhi))

            (lower :+ lowerIntervalMap, upper :+ upperIntervalMap)
          } else if (partInterval.xlo >= splitPoint) {
            (lower :+ part, upper)
          } else {
            (lower, upper :+ part)
          }
      }

      numberOfRegimePartsForNestedSplit(lower) + numberOfRegimePartsForNestedSplit(upper)
  }
}
