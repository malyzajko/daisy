// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.Identifiers._
import tools._
import FinitePrecision._
import lang.TreeOps.allVariablesOf

/**
  Computes and stores intermediate ranges.

  Prerequisites:
    - SpecsProcessingPhase
 */
object DataflowPhase extends DaisyPhase with RoundoffEvaluators with IntervalSubdivision with opt.CostFunctions {
  override val name = "Dataflow Error"
  override val shortName = "analysis"
  override val description = "Computes ranges and absolute errors via dataflow analysis"

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "errorMethod",
      Set("affine", "interval"),
      "affine",
      "Method for error analysis"),
    StringChoiceOption(
      "choosePrecision",
      Set("no", "fixed", "float"),
      "no",
      "choose the fixed/floating-point precision which satisfies error bound")
  )

  implicit val debugSection = DebugSectionAnalysis

  var rangeMethod = ""
  var errorMethod = ""
  var trackRoundoffErrs = true

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    rangeMethod = ctx.option[String]("rangeMethod")
    errorMethod = ctx.option[String]("errorMethod")
    trackRoundoffErrs = !ctx.hasFlag("noRoundoff")

    val choosePrecision = ctx.option[String]("choosePrecision")

    val mixedPrecision = ctx.option[Option[String]]("mixed-precision").isDefined
    val uniformPrecision = ctx.option[Precision]("precision")
    val reporter = ctx.reporter

    ctx.reporter.info(s"using $rangeMethod for ranges, $errorMethod for errors")

    var uniformPrecisions = Map[Identifier, Precision]()

    // returns (abs error, result range, interm. errors, interm. ranges)
    val res: Map[Identifier, (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval])] =
      analyzeConsideredFunctions(ctx, prg){ fnc =>

      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val fncBody = fnc.body.get

      if (choosePrecision != "no") {
        reporter.info("analyzing fnc: " + fnc.id)

        // the max tolerated error
        val targetError = ctx.specResultErrorBounds(fnc.id)

        val availablePrecisions = choosePrecision match {
          case "fixed" =>
            // the max available amount of bits
            val maxBits = uniformPrecision match {
              case FixedPrecision(b) => b
              case _ => 32 // TODO put default elsewhere
            }
            (1 to maxBits).map(x => FixedPrecision(x))
          case "float" => List(Float16, Float32, Float64, DoubleDouble, QuadDouble)
          case s => throw new Exception(s"Unknown choice value for choosePrecision: $s. Stopping now")
        }

        reporter.info(s"choosing among $choosePrecision precisions")

        // save the intermediate result
        var res: (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval]) = null

        // find precision which is sufficient
        availablePrecisions.find( prec => {
          try {
            reporter.info(s"trying precision $prec")
            val allIDs = fnc.params.map(_.id)
            val inputErrorMap = allIDs.map(id => (id -> prec.absRoundoff(inputValMap(id)))).toMap
            val precisionMap: Map[Identifier, Precision] = allVariablesOf(fnc.body.get).map(id => (id -> prec)).toMap

            res = computeRoundoff(inputValMap, inputErrorMap, precisionMap, fncBody,
              prec, ctx.specAdditionalConstraints(fnc.id))

            res._1 <= targetError
          } catch {
            case OverflowException(_) => false
          }
        }) match {

          case None =>
            val lastPrec = availablePrecisions.last
            reporter.warning(s"Highest available precision ${lastPrec} " +
              "is not sufficient. Using it anyway.")
            uniformPrecisions = uniformPrecisions + (fnc.id -> lastPrec)

          case Some(prec) =>
            uniformPrecisions = uniformPrecisions + (fnc.id -> prec)
        }
        res

      } else {
        ctx.reporter.info("analyzing fnc: " + fnc.id)

        if (!mixedPrecision) {
          ctx.reporter.info(s"error analysis for uniform $uniformPrecision precision")
        }
        val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

        val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id)
        uniformPrecisions = uniformPrecisions + (fnc.id -> uniformPrecision) // so that this info is available in codegen

        computeRoundoff(inputValMap, inputErrorMap, precisionMap, fncBody,
          uniformPrecision, ctx.specAdditionalConstraints(fnc.id))
      }
    }

    (ctx.copy(uniformPrecisions = uniformPrecisions,
      resultAbsoluteErrors = res.mapValues(_._1),
      resultRealRanges = res.mapValues(_._2),
      intermediateAbsErrors = res.mapValues(_._3),
      intermediateRanges = res.mapValues(_._4)), prg)
  }

  def computeRoundoff(inputValMap: Map[Identifier, Interval], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], expr: Expr,
    constPrecision: Precision, additionalConstr: Expr = BooleanLiteral(true)):
    (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval]) = {

    val (resRange, intermediateRanges) = (rangeMethod: @unchecked) match {
      case "interval" =>
        evalRange[Interval](expr, inputValMap, Interval.apply)

      case "affine" =>
        val (rng, intrmdRange) = evalRange[AffineForm](expr,
          inputValMap.mapValues(AffineForm(_)), AffineForm.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval))

      case "smt" =>
        // SMT can take into account additional constraints
        val (rng, intrmdRange) = evalRange[SMTRange](expr,
          inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, additionalConstr)) }),
          SMTRange.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval))
    }

    val (resError, intermediateErrors) = (errorMethod: @unchecked) match {
    case "interval" =>
      val (resRoundoff, allErrors) = evalRoundoff[Interval](expr, intermediateRanges,
        precisionMap,
        inputErrorMap.mapValues(Interval.+/-),
        zeroError = Interval.zero,
        fromError = Interval.+/-,
        interval2T = Interval.apply,
        constantsPrecision = constPrecision,
        trackRoundoffErrs)

      (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(Interval.maxAbs))

    case "affine" =>

      val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr, intermediateRanges,
        precisionMap,
        inputErrorMap.mapValues(AffineForm.+/-),
        zeroError = AffineForm.zero,
        fromError = AffineForm.+/-,
        interval2T = AffineForm.apply,
        constantsPrecision = constPrecision,
        trackRoundoffErrs)

      (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)))
    }
    (resError, resRange, intermediateErrors, intermediateRanges)
  }
}
