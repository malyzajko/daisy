// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.TreeOps.allVariablesOf
import lang.Identifiers._
import tools._
import FinitePrecision._


/**
  ??? Description goes here

  Computes and stores intermediate ranges.

  Prerequisites:
    - SpecsProcessingPhase
 */
object DataflowPhase extends PhaseComponent {
  override val name = "Dataflow Error"
  override val description = "Computes ranges and absolute errors via dataflow analysis"

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "errorMethod",
      Set("affine", "interval"),
      "affine",
      "Method for error analysis")
  )
  override def apply(cfg: Config) = new DataflowPhase(cfg, name, "analysis")
}

class DataflowPhase(val cfg: Config, val name: String, val shortName: String) extends DaisyPhase
    with RoundoffEvaluators with IntervalSubdivision {
  implicit val debugSection = DebugSectionAnalysis

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    startRun()

    val rangeMethod = cfg.option[String]("rangeMethod")
    val errorMethod = cfg.option[String]("errorMethod")

    val trackInitialErrs = !cfg.hasFlag("noInitialErrors")
    val trackRoundoffErrs = !cfg.hasFlag("noRoundoff")

    val mixedPrecision = cfg.option[Option[String]]("mixed-precision").isDefined
    val uniformPrecision = cfg.option[Precision]("precision")

    cfg.reporter.info(s"using $rangeMethod for ranges, $errorMethod for errors")
    if (!mixedPrecision) {
      cfg.reporter.info(s"error analysis for uniform $uniformPrecision precision")
    }


    // returns (abs error, result range, interm. errors, interm. ranges)
    val res: Map[Identifier, (Rational, Interval, Map[Expr, Rational], Map[Expr, Interval])] =
      functionsToConsider(prg).map(fnc => {

      cfg.reporter.info("analyzing fnc: " + fnc.id)
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val precisionMap: Map[Identifier, Precision] = if (mixedPrecision) {
        ctx.specMixedPrecisions(fnc.id)
      } else {
        allVariablesOf(fnc.body.get).map(id => (id -> uniformPrecision)).toMap
      }

      // If we track both input and roundoff errors, then we pre-compute
      // the roundoff errors for those variables that do not have a user-defined
      // error, in order to keep correlations.
      val inputErrorMap: Map[Identifier, Rational] =
        if (trackInitialErrs && trackRoundoffErrs) {

          val inputErrs = ctx.specInputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map(id => (id -> precisionMap(id).absRoundoff(inputValMap(id))))

        } else if (trackInitialErrs) {

          val inputErrs = ctx.specInputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map(id => (id -> Rational.zero))

        } else if (trackRoundoffErrs) {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> precisionMap(id).absRoundoff(inputValMap(id)))).toMap

        } else {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> Rational.zero)).toMap

        }

      val fncBody = fnc.body.get

      val (resRange, intermediateRanges) = (rangeMethod: @unchecked) match {
        case "interval" =>
          evalRange[Interval](fncBody, inputValMap, Interval.apply)

        case "affine" =>
          val (rng, intrmdRange) = evalRange[AffineForm](fncBody,
            inputValMap.mapValues(AffineForm(_)), AffineForm.apply)
          (rng.toInterval, intrmdRange.mapValues(_.toInterval))

        case "smt" =>
          // the SMT can take into account additional constraints
          val additionalConstr = ctx.specAdditionalConstraints(fnc.id)
          additionalConstr match {
            case BooleanLiteral(_) =>
            case c => cfg.reporter.info("taking into account additional constraint: " + c)
          }
          val (rng, intrmdRange) = evalRange[SMTRange](fncBody,
            inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, additionalConstr)) }),
            SMTRange.apply)
          (rng.toInterval, intrmdRange.mapValues(_.toInterval))
      }

      val (resError, intermediateErrors) = (errorMethod: @unchecked) match {
        case "interval" =>
          val (resRoundoff, allErrors) = evalRoundoff[Interval](fncBody, intermediateRanges,
            precisionMap,
            inputErrorMap.mapValues(Interval.+/-),
            zeroError = Interval.zero,
            fromError = Interval.+/-,
            interval2T = Interval.apply,
            constantsPrecision = uniformPrecision,
            trackRoundoffErrs)

          (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(Interval.maxAbs))

        case "affine" =>

          val (resRoundoff, allErrors) = evalRoundoff[AffineForm](fncBody, intermediateRanges,
            precisionMap,
            inputErrorMap.mapValues(AffineForm.+/-),
            zeroError = AffineForm.zero,
            fromError = AffineForm.+/-,
            interval2T = AffineForm.apply,
            constantsPrecision = uniformPrecision,
            trackRoundoffErrs)

          (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)))
      }

      (fnc.id -> ((resError, resRange, intermediateErrors, intermediateRanges)))

    }).toMap

    finishRun(
      ctx.copy(
        resultAbsoluteErrors = res.mapValues(_._1),
        resultRealRanges = res.mapValues(_._2),
        intermediateAbsErrors = res.mapValues(_._3),
        intermediateRanges = res.mapValues(_._4)),
      prg)
  }


}
