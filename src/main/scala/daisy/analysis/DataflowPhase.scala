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
  override val name = "Dataflow error"
  override val description = "Computes ranges and absolute errors via dataflow analysis"

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "errorMethod",
      Set("affine", "interval", "intervalMPFR", "affineMPFR"),
      "affineMPFR",
      "Method for error analysis"),
    StringChoiceOption(
      "choosePrecision",
      Set("no", "fixed", "float"),
      "no",
      "choose the fixed/floating-point precision which satisfies error bound")
  )
  override implicit val debugSection = DebugSectionAnalysis

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

            res = computeRoundoff(inputValMap, inputErrorMap, precisionMap, fncBody, prec, fnc.precondition.get)

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

        val precond = fnc.precondition.get

        computeRoundoff(inputValMap, inputErrorMap, precisionMap, fncBody,
          uniformPrecision, precond)
      }
    }

    (ctx.copy(uniformPrecisions = uniformPrecisions,
      resultAbsoluteErrors = res.mapValues(_._1).toMap,
      resultRealRanges = res.mapValues(_._2).toMap,
      intermediateAbsErrors = res.mapValues(_._3).toMap,
      intermediateRanges = res.mapValues(_._4).toMap), prg)
  }

  def computeRange(inputValMap: Map[Identifier, Interval], expr: Expr, precond: Expr):
  (Interval, Map[(Expr, PathCond), Interval]) = {

    (rangeMethod: @unchecked) match {
      case "interval" =>
        evalRange[Interval](expr, inputValMap, Interval.apply)

      case "affine" =>
        val (rng, intrmdRange) = evalRange[AffineForm](expr,
          inputValMap.mapValues(AffineForm(_)).toMap, AffineForm.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)

      case "smt" =>
        // SMT can take into account additional constraints
        val (rng, intrmdRange) = evalRange[SMTRange](expr,
          inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, precond)) }),
          SMTRange.apply(_, precond))
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)

      case "intervalMPFR" =>
        val (rng, intrmdRange) = evalRange[MPFRInterval](expr,
          inputValMap.mapValues(MPFRInterval(_)).toMap, MPFRInterval.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)

      case "affineMPFR" =>
        val (rng, intrmdRange) = evalRange[MPFRAffineForm](expr,
          inputValMap.mapValues(MPFRAffineForm(_)).toMap, MPFRAffineForm.apply)
        (rng.toInterval, intrmdRange.mapValues(_.toInterval).toMap)
    }
  }

  def computeErrors(intermediateRanges: Map[(Expr, PathCond), Interval], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], expr: Expr, constPrecision: Precision):
  (Rational, Map[(Expr, PathCond), Rational]) = {

    (errorMethod: @unchecked) match {
      case "interval" =>
        val (resRoundoff, allErrors) = evalRoundoff[Interval](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(Interval.+/-).toMap,
          zeroError = Interval.zero,
          fromError = Interval.+/-,
          interval2T = Interval.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(Interval.maxAbs).toMap)

      case "affine" =>

        val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(AffineForm.+/-).toMap,
          zeroError = AffineForm.zero,
          fromError = AffineForm.+/-,
          interval2T = AffineForm.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)).toMap)

      case "intervalMPFR" =>

        val (resRoundoff, allErrors) = evalRoundoff[MPFRInterval](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(MPFRInterval.+/-).toMap,
          zeroError = MPFRInterval.zero,
          fromError = MPFRInterval.+/-,
          interval2T = MPFRInterval.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)).toMap)

      case "affineMPFR" =>

        val (resRoundoff, allErrors) = evalRoundoff[MPFRAffineForm](expr, intermediateRanges,
          precisionMap,
          inputErrorMap.mapValues(MPFRAffineForm.+/-).toMap,
          zeroError = MPFRAffineForm.zero,
          fromError = MPFRAffineForm.+/-,
          interval2T = MPFRAffineForm.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs)

        (Interval.maxAbs(resRoundoff.toInterval), allErrors.mapValues(e => Interval.maxAbs(e.toInterval)).toMap)
    }
  }

  def computeRoundoff(inputValMap: Map[Identifier, Interval], inputErrorMap: Map[Identifier, Rational],
    precisionMap: Map[Identifier, Precision], expr: Expr, constPrecision: Precision, precond: Expr):
    (Rational, Interval, Map[(Expr, PathCond), Rational], Map[(Expr, PathCond), Interval]) = {

    val (resRange, intermediateRanges) = computeRange(inputValMap, expr, precond)

    val (resError, intermediateErrors) = computeErrors(intermediateRanges, inputErrorMap, precisionMap, expr,
      constPrecision)

    (resError, resRange, intermediateErrors, intermediateRanges)
  }
}
