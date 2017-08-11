// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.TreeOps.allVariablesOf
import lang.Identifiers._
import tools.{AffineForm, Interval, Rational}
import tools.FinitePrecision._
import tools.Interval._
import Rational._

/**
  This phase computes roundoff errors for a given precision and data type
  and attaches the information to the trees.

  TODO: make parametric in data type

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

  override val name = "roundoff phase"
  override val description = "Computes roundoff errors"
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.roundoff.start

    var trackInitialErrs = true
    var trackRoundoffErrs = true
    val uniformPrecision = Float64

    /* Process relevant options */
    for (opt <- ctx.options) opt match {
      case FlagOption("noInitialErrors") => trackInitialErrs = false
      case FlagOption("noRoundoff") => trackRoundoffErrs = false
      case _ => ;
    }

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)

    val res: Map[Identifier, (Rational, Map[Expr, Rational])] = prg.defs.filter(fnc =>
      !fnc.precondition.isEmpty &&
      !fnc.body.isEmpty &&
      fncsToConsider.contains(fnc.id.toString)).map(fnc => {

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
          inputErrs ++ missingIDs.map(id => (id -> zero))

        } else if (trackRoundoffErrs) {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))).toMap

        } else {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> zero)).toMap

        }

      val fncBody = fnc.body.get
      val intermediateRanges = ctx.intermediateRanges(fnc.id)

      val (resRoundoff, allErrors) = evalRoundoff[AffineForm](fncBody, intermediateRanges,
        allVariablesOf(fncBody).map(id => (id -> uniformPrecision)).toMap,
        inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
        zeroError = AffineForm.zero,
        fromError = AffineForm.fromError,
        interval2T = AffineForm.apply,
        constantsPrecision = uniformPrecision,
        trackRoundoffErrs)

      // computeAbsError(fnc.body.get, Map.empty, trackInitialErrs, trackRoundoffErrs)
      (fnc.id -> (Interval.maxAbs(resRoundoff.toInterval),
        allErrors.map({
          case (e, aa) => (e -> Interval.maxAbs(aa.toInterval))
        })))
    }).toMap

    timer.stop
    // ctx.reporter.info(s"Finished $name")
    (ctx.copy(resultAbsoluteErrors = res.mapValues(_._1),
      intermediateAbsErrors = res.mapValues(_._2)), prg)
  }

}

