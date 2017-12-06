// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.Identifiers._
import tools._
import FinitePrecision._

/**
  Computes and stores intermediate ranges.

  Prerequisites:
    - SpecsProcessingPhase
 */
object DataflowPhase extends DaisyPhase with RoundoffEvaluators with IntervalSubdivision {
  override val name = "Dataflow Error"
  override val shortName = "analysis"
  override val description = "Computes ranges and absolute errors via dataflow analysis"

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "errorMethod",
      Set("affine", "interval"),
      "affine",
      "Method for error analysis")
  )

  implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val rangeMethod = ctx.option[String]("rangeMethod")
    val errorMethod = ctx.option[String]("errorMethod")

    val trackRoundoffErrs = !ctx.hasFlag("noRoundoff")

    val mixedPrecision = ctx.option[Option[String]]("mixed-precision").isDefined
    val uniformPrecision = ctx.option[Precision]("precision")

    ctx.reporter.info(s"using $rangeMethod for ranges, $errorMethod for errors")
    if (!mixedPrecision) {
      ctx.reporter.info(s"error analysis for uniform $uniformPrecision precision")
    }


    // returns (abs error, result range, interm. errors, interm. ranges)
    val res: Map[Identifier, (Rational, Interval, Map[Expr, Rational], Map[Expr, Interval])] =
      analyzeConsideredFunctions(ctx, prg){ fnc =>

      ctx.reporter.info("analyzing fnc: " + fnc.id)
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id)

      val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

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
            case c => ctx.reporter.info("taking into account additional constraint: " + c)
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

      (resError, resRange, intermediateErrors, intermediateRanges)

    }

    (ctx.copy(
      resultAbsoluteErrors = res.mapValues(_._1),
      resultRealRanges = res.mapValues(_._2),
      intermediateAbsErrors = res.mapValues(_._3),
      intermediateRanges = res.mapValues(_._4)),
    prg)
  }


}
