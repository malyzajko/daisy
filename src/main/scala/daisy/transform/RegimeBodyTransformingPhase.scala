package daisy.transform

import java.io.{BufferedWriter, File, FileWriter}
import daisy._
import daisy.opt.RegimeInferencePhase.Regime
import daisy.opt.{RegimeInferencePhase, RegimePartResult, OffsetAggregateEvaluation}
import daisy.analysis.DataflowPhase
import daisy.lang.Identifiers.Identifier
import daisy.lang.TreeOps.postTraversal
import daisy.lang.Trees.{Expr, _}
import daisy.lang.Types.FinitePrecisionType
import daisy.lang.{TreeOps, Trees}
import daisy.opt.CostFunctions
import daisy.opt.MixedPrecisionOptimizationPhase.exprWithoutCasts
import daisy.tools.FinitePrecision.Precision
import daisy.tools._

/**
 * A phase that evaluates the grade of a regime split (based on a given phase) and rewrites the functions based on that
 * regime split.
 *
 */
object RegimeBodyTransformingPhase extends DaisyPhase with Subdivision with CostFunctions
  with RoundoffEvaluators with DynamicEvaluators {

  override val name: String = "regime inference transformation phase"
  override val description: String = "A phase to evaluate costs of inferred regimes"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringOption("csvFolder", "where to put the CSV file with info")
  )

  override implicit val debugSection = DebugSectionTransform

  override def runPhase(ctx: Context, prog: Trees.Program): (Context, Trees.Program) = {

    val regimes = ctx.regimes
    val defaultPrecision = ctx.option[Precision]("precision")

    val csvFolder = ctx.option[Option[String]]("csvFolder") match {
      case Some(name) => name
      case None => "output"
    }

    val newDefs = transformConsideredFunctions(ctx, prog) { fnc =>
      val regime = regimes(fnc.id)

      val newReturnType = ctx.specResultPrecisions.get(fnc.id).map(FinitePrecisionType).getOrElse(fnc.returnType)
      val newParams = fnc.params.map(p =>
        ctx.specInputPrecisions(fnc.id)
          .get(p.id)
          .map(t => p.copy(id = p.id.changeType(FinitePrecisionType(t))))
          .getOrElse(p)
      )

      val newBody = createIfThenElseConditions(regime, ctx.specInputPrecisions(fnc.id))

      if (ctx.hasFlag("regime-rewriting")) {
        /*ctx.reporter.info("evaluating the error of final program")

        val overallErrorOld = generalErrorDynamicWithInputRoundoff(fnc.body.get,
          ctx.specInputRanges(fnc.id), 100000).avrgRelError
        val overallErrorNew = generalErrorDynamicWithInputRoundoff(newBody,
          ctx.specInputRanges(fnc.id), 100000).avrgRelError
        ctx.reporter.result(s"error before: $overallErrorOld\nerror after:  $overallErrorNew")
        val o = new FileWriter(new File(csvFolder, "regime-rewriting.csv"), true)

        // run several trials with different seeds to get reliable data
        val overallErrorOld2 = generalErrorDynamicWithInputRoundoff(fnc.body.get, ctx.specInputRanges(fnc.id), 100000, 274583).avrgRelError
        val overallErrorNew2 = generalErrorDynamicWithInputRoundoff(newBody, ctx.specInputRanges(fnc.id), 100000, 274583).avrgRelError
        ctx.reporter.result(s"error before: $overallErrorOld2\nerror after:  $overallErrorNew2")

        val overallErrorOld3 = generalErrorDynamicWithInputRoundoff(fnc.body.get, ctx.specInputRanges(fnc.id), 100000, 997245).avrgRelError
        val overallErrorNew3 = generalErrorDynamicWithInputRoundoff(newBody, ctx.specInputRanges(fnc.id), 100000, 997245).avrgRelError
        ctx.reporter.result(s"error before: $overallErrorOld3\nerror after:  $overallErrorNew3")

        o.write(s"${fnc.id}, ${overallErrorOld}, ${overallErrorNew}, ${overallErrorOld2}, ${overallErrorNew2} ${overallErrorOld3}, ${overallErrorNew3}\n")
        o.close*/

        val costNew = RegimeInferencePhase.rewritingCostWeighted(ctx.specInputRanges(fnc.id), regimes(fnc.id),
          ctx.specInputErrors(fnc.id), defaultPrecision)
        ctx.reporter.result(s"# regimes: ${regime.size}, with max error: $costNew")
        val o = new FileWriter(new File(csvFolder, "regime-rewriting.csv"), true)
        o.write(s"${fnc.id}, ${regime.size}, ${costNew.toCSV}\n")
        o.close

      } else { //mixed-tuning

        val costNew = RegimeInferencePhase.mixedPrecisionCost(ctx.specInputRanges(fnc.id), regime)
        ctx.reporter.result(s"# regimes: ${regime.size}, with cost: $costNew")
        val o = new FileWriter(new File(csvFolder, "regime-tuning.csv"), true)
        o.write(s"${fnc.id}, ${regime.size}, ${costNew.toCSV}\n")
        o.close

      }

      fnc.copy(params = newParams, body = Some(newBody), returnType = newReturnType)
    }

    (ctx, Program(prog.id, newDefs))

  }


  def createIfThenElseConditions(regime: Seq[RegimePartResult], precisions: Map[Identifier, Precision]): Expr =

    regime match {
      case Seq(part) => part.rewrittenBody

      case _ =>
        val variables = regime.flatMap(_.intervalMap.keys).distinct

        // Split the variable with the greatest number of distinct intervals
        val variableToSplitAt = variables
          .filterNot(v => regime.map(_.intervalMap(v)).distinct.size == 1)
          .maxBy(v => regime.map(_.intervalMap(v)).distinct.size)

        // We will split at one of the start or end points of the intervals.
        val possibleSplitPoints = regime
          .flatMap(p => Seq(p.intervalMap(variableToSplitAt).xlo, p.intervalMap(variableToSplitAt).xhi))
          .sorted
          .distinct

        val splitPoint = possibleSplitPoints(possibleSplitPoints.size / 2)

        // Partition all regime parts by whether the variable interval lies fully above or below the split point
        // If the interval contains the split point (and it is not one of the boundaries), we need to split the interval
        // into two parts at the split point.
        val (lower, lowerTranslationMap, upper, upperTranslationMap) =
        regime.foldLeft(
          (Seq[RegimePartResult](),
            Map[RegimePartResult, RegimePartResult](),
            Seq[RegimePartResult](),
            Map[RegimePartResult, RegimePartResult]())
        ) {
          case ((lower, lowerTranslationMap, upper, upperTranslationMap), part) =>
            val partInterval = part.intervalMap(variableToSplitAt)
            if (partInterval.xlo < splitPoint && partInterval.xhi > splitPoint) {
              val lowerIntervalMap = part.intervalMap + (variableToSplitAt -> Interval(partInterval.xlo, splitPoint))
              val upperIntervalMap = part.intervalMap + (variableToSplitAt -> Interval(splitPoint, partInterval.xhi))

              val lowerPart = part.copy(intervalMap = lowerIntervalMap)
              val upperPart = part.copy(intervalMap = upperIntervalMap)

              (
                lower :+ lowerPart,
                lowerTranslationMap + (part -> lowerPart),
                upper :+ upperPart,
                upperTranslationMap + (part -> upperPart)
              )
            } else if (partInterval.xhi <= splitPoint) {
              (lower :+ part, lowerTranslationMap, upper, upperTranslationMap)
            } else {
              (lower, lowerTranslationMap, upper :+ part, upperTranslationMap)
            }
        }

        val condition = LessEquals(Variable(variableToSplitAt),
          precisions
            .get(variableToSplitAt)
            .map(FinitePrecisionLiteral(splitPoint, _, splitPoint.toString))
            .getOrElse(RealLiteral(splitPoint))
        )

        def replaceKeyByValueIn[K, V](replacementMap: Map[K, K])(keyValue: (K, V)): (K, V) = {
          (replacementMap.getOrElse(keyValue._1, keyValue._1), keyValue._2)
        }

        val thenBody = createIfThenElseConditions(lower, precisions)
        val elseBody = createIfThenElseConditions(upper, precisions)

        def updatePathsWithCondition[T](cond: Expr, value: ((Expr, Seq[Expr]), T)): ((Expr, Seq[Expr]), T) = {
          ((value._1._1, cond +: value._1._2), value._2)
        }

        IfExpr(cond = condition,
          thenn = thenBody,
          elze = elseBody)
    }
}
