// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.TreeOps.allVariablesOf
import lang.Identifiers._
import tools.{AffineForm, Interval, Rational}
import tools.FinitePrecision._
import tools.Interval._

/**
  This phase computes roundoff errors for a given precision and data type
  and attaches the information to the trees.

  Note:
    - if we want to use separation of errors, then the range computation needs
    to compute the actual (but real-valued) ranges, as
    |f(\tl{x} - \tl{f}(\tl{x}) | is over \tl{x} and hence the actual range.
    - For now, we use affine arithmetic to track initial as well as roundoff
    errors. Via options one can switch off tracking of either.
    - if we are tracking initial errors, and one is present, do NOT add an
    additional roundoff error

  Prerequisites:
    - SpecsProcessingPhase
    - RangePhase (check you are computing the correct ranges)
 */
object AbsErrorPhase extends DaisyPhase with tools.RoundoffEvaluators {
  override val name = "Roundoff"
  override val shortName = "roundoff"
  override val description = "Computes roundoff errors"

  implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val trackInitialErrs = !ctx.hasFlag("noInitialErrors")
    val trackRoundoffErrs = !ctx.hasFlag("noRoundoff")
    val uniformPrecision = ctx.option[Precision]("precision")

    val res: Map[Identifier, (Rational, Map[Expr, Rational])] =
      functionsToConsider(ctx, prg).map(fnc => {

      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val inputErrorMap: Map[Identifier, Rational] =
        if (trackInitialErrs && trackRoundoffErrs) {

          val inputErrs = ctx.specInputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map(id => (id -> uniformPrecision.absRoundoff(inputValMap(id))))

        } else if (trackInitialErrs) {

          val inputErrs = ctx.specInputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map(id => (id -> Rational.zero))

        } else if (trackRoundoffErrs) {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))).toMap

        } else {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> Rational.zero)).toMap

        }

      val fncBody = fnc.body.get
      val intermediateRanges = ctx.intermediateRanges(fnc.id)

      val (resRoundoff, allErrors) = evalRoundoff[AffineForm](fncBody, intermediateRanges,
        Map.empty.withDefaultValue(uniformPrecision),
        inputErrorMap.mapValues(AffineForm.+/-),
        zeroError = AffineForm.zero,
        fromError = AffineForm.+/-,
        interval2T = AffineForm.apply,
        constantsPrecision = uniformPrecision,
        trackRoundoffErrs)

      // computeAbsError(fnc.body.get, Map.empty, trackInitialErrs, trackRoundoffErrs)
      fnc.id -> (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(aa => Interval.maxAbs(aa.toInterval)))
    }).toMap

    (ctx.copy(
      resultAbsoluteErrors = res.mapValues(_._1),
      intermediateAbsErrors = res.mapValues(_._2)),
    prg)
  }

}
