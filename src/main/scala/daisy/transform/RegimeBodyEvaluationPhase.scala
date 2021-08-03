package daisy.transform

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
object RegimeBodyEvaluationPhase extends DaisyPhase with Subdivision with CostFunctions with RoundoffEvaluators {

  override val name: String = "regime inference evaluation phase"
  override val description: String = "A phase to evaluate costs of inferred regimes"

  private type IntermediateRanges = Map[(Expr, PathCond), Interval]
  private type IntermediateErrors = Map[(Expr, PathCond), Rational]

  override implicit val debugSection = DebugSectionTransform

  private var defaultPrecision: Precision = _
  private var rangeMethod: String = ""

  override def runPhase(ctx: Context, prog: Trees.Program): (Context, Trees.Program) = {

    defaultPrecision = ctx.option[Precision]("precision")
    rangeMethod = ctx.option[String]("rangeMethod")

    DataflowPhase.errorMethod = ctx.option[String]("errorMethod")

    val regimes = ctx.regimes

    val evaluationResult: Map[Identifier, OffsetAggregateEvaluation] = analyzeConsideredFunctions(ctx, prog) {
      f =>
        if (ctx.hasFlag("regime-rewriting")) {
          RegimeInferencePhase.rewritingCost(ctx.specInputRanges(f.id), regimes(f.id), ctx.specInputErrors(f.id), defaultPrecision)
        } else {
          RegimeInferencePhase.mixedPrecisionCost(ctx.specInputRanges(f.id), regimes(f.id))
        }
    }

    for ((id, eval) <- evaluationResult) {
      ctx.reporter.info(s"[$id] Final regime-inferred evaluation: $eval")
    }

    val zipped = analyzeConsideredFunctions(ctx, prog) { f =>
      if (f.body.isEmpty) {
        (f, Interval(Rational.zero, Rational.zero), Rational.zero,
          Map[(Expr, Seq[Expr]), Interval](), Map[(Expr, Seq[Expr]), Rational]())
      } else {
        val regime = regimes(f.id)

        val newReturnType = ctx.specResultPrecisions.get(f.id).map(FinitePrecisionType).getOrElse(f.returnType)
        val newParams = f.params.map(p =>
          ctx.specInputPrecisions(f.id)
            .get(p.id)
            .map(t => p.copy(id = p.id.changeType(FinitePrecisionType(t))))
            .getOrElse(p)
        )

        val (resultRange, resultError, intermediateRanges, intermediateErrors) =
          computeIndividualIntermediateRangesAndErrors(regime, f.precondition.get)

        val (newBody, newRanges, newErrors) = if (regime.size > 1) {
          val (newBody, newRanges, newErrors) =
            createIfThenElseConditions(regime, ctx.specInputPrecisions(f.id))(intermediateRanges, intermediateErrors)

          (newBody,
            newRanges ++ newParams.map(p => (Variable(p.id), Seq()) -> ctx.specInputRanges(f.id)(p.id)),
            newErrors ++ newParams.map(p => (Variable(p.id), Seq()) -> ctx.specInputErrors(f.id)(p.id))
          )
        } else {
          (regime.head.rewrittenBody, intermediateRanges(regime.head), intermediateErrors(regime.head))
        }

        (f.copy(params = newParams,
          body = Some(newBody), returnType = newReturnType),
          resultRange,
          resultError,
          newRanges,
          newErrors)
      }
    }

    val rewrittenBodies = zipped.map {
      case (_, (bodies, _, _, _, _)) => bodies
    }.toSeq

    val newIntermediateRanges = zipped.transform {
      case (_, (_, _, _, newRanges, _)) => newRanges
    }

    val newIntermediateErrors = zipped.transform {
      case (_, (_, _, _, _, newErrors)) => newErrors
    }

    val newResultRanges = zipped.transform {
      case (_, (_, resultRange, _, _, _)) => resultRange
    }

    val newResultErrors = zipped.transform {
      case (_, (_, _, resultError, _, _)) => resultError
    }

    val rewrittenProgram = Program(prog.id, rewrittenBodies)
    (ctx.copy(intermediateRanges = newIntermediateRanges,
      intermediateAbsErrors = newIntermediateErrors,
      resultRealRanges = newResultRanges,
      resultAbsoluteErrors = newResultErrors), rewrittenProgram)
  }

  private def computeIndividualIntermediateRangesAndErrors(regime: Regime,
    precondition: Expr): (Interval, Rational, Map[RegimePartResult, IntermediateRanges], Map[RegimePartResult, IntermediateErrors]) = {

    val rangesAndErrors = regime.map(part => {
      val (resultRange, intermediateRanges) = DataflowPhase.computeRange(part.intervalMap,
        part.evaluationBody, precondition, rangeMethod)

      var intermediateRangesWithCasts = Map[(Expr, Seq[Expr]), Interval]()

      postTraversal(e => {
        intermediateRangesWithCasts = intermediateRangesWithCasts +
          ((e, Seq()) -> intermediateRanges((exprWithoutCasts(e), Seq())))
      })(part.rewrittenBody)

      val inputErrors = part.inputPrecisions.transform {
        case (id, prec) => part.intervalMap.get(id).map(i => prec.absRoundoff(i)).orNull
      }

      val (resultError, intermediateErrorsWithCasts) = DataflowPhase.computeErrors(intermediateRangesWithCasts,
        inputErrors, part.inputPrecisions, part.rewrittenBody, defaultPrecision)

      part -> (resultRange, resultError, intermediateRangesWithCasts, intermediateErrorsWithCasts)
    }).toMap

    val intermediateRanges = rangesAndErrors.transform { case (_, (_, _, intermediateRanges, _)) => intermediateRanges }
    val intermediateErrors = rangesAndErrors.transform { case (_, (_, _, _, intermediateErrors)) => intermediateErrors }

    val resultRanges = rangesAndErrors.map({ case (_, (r: Interval, _, _, _)) => r })
    val resultRange = resultRanges.tail.foldLeft(resultRanges.head)({ case (a, b) => a.union(b) })

    val resultErrors = rangesAndErrors.map({ case (_, (_, e: Rational, _, _)) => e })
    val resultError = resultErrors.foldLeft(resultErrors.head)({ case (a, b) => Rational.max(a, b) })

    (resultRange, resultError, intermediateRanges, intermediateErrors)
  }

  def createIfThenElseConditions(regime: Seq[RegimePartResult],
    precisions: Map[Identifier, Precision])(implicit intermediateRanges: Map[RegimePartResult, IntermediateRanges],
    intermediateErrors: Map[RegimePartResult, IntermediateErrors]): (Expr, IntermediateRanges, IntermediateErrors) =

    regime match {
      case Seq(part) => (part.rewrittenBody, intermediateRanges(part), intermediateErrors(part))
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

        val lowerIntermediateRanges = intermediateRanges.map(replaceKeyByValueIn(lowerTranslationMap))
        val lowerIntermediateErrors = intermediateErrors.map(replaceKeyByValueIn(lowerTranslationMap))

        val upperIntermediateRanges = intermediateRanges.map(replaceKeyByValueIn(upperTranslationMap))
        val upperIntermediateErrors = intermediateErrors.map(replaceKeyByValueIn(upperTranslationMap))

        val (thenBody, thenRanges, thenErrors) = createIfThenElseConditions(lower, precisions)(lowerIntermediateRanges, lowerIntermediateErrors)
        val (elseBody, elseRanges, elseErrors) = createIfThenElseConditions(upper, precisions)(upperIntermediateRanges, upperIntermediateErrors)

        def updatePathsWithCondition[T](cond: Expr, value: ((Expr, Seq[Expr]), T)): ((Expr, Seq[Expr]), T) = {
          ((value._1._1, cond +: value._1._2), value._2)
        }

        val updatedRanges = (thenRanges.toList.map(updatePathsWithCondition(condition, _)) ++
          elseRanges.toList.map(updatePathsWithCondition(TreeOps.negate(condition), _))).toMap

        val updatedErrors = (thenErrors.toList.map(updatePathsWithCondition(condition, _)) ++
          elseErrors.toList.map(updatePathsWithCondition(TreeOps.negate(condition), _))).toMap

        (IfExpr(cond = condition,
          thenn = thenBody,
          elze = elseBody), updatedRanges, updatedErrors)
    }
}
