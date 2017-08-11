// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.TreeOps.allVariablesOf
import lang.Identifiers._
import tools._
import FinitePrecision._
import Rational._


/**
  ??? Description goes here

  Computes and stores intermediate ranges.

  Prerequisites:
    - SpecsProcessingPhase
 */
object RangeErrorPhase extends DaisyPhase with RoundoffEvaluators with IntervalSubdivision {

  override val name = "dataflow error phase"
  override val description = "Computes ranges and absolute errors via dataflow analysis"

  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.analysis.start

    // default range method: intervals
    var rangeMethod = "interval"
    var errorMethod = "affine"   // only one supported so far

    var trackInitialErrs = true
    var trackRoundoffErrs = true

    var mixedPrecision: Boolean = false
    var uniformPrecision: Precision = Float64

    // process relevant options
    for (opt <- ctx.options) opt match {
      case ChoiceOption("rangeMethod", s) => s match {
        case "interval" | "affine" | "smt" | "subdiv" =>
          rangeMethod = s
          reporter.info(s"using $s")
        case _ =>
          reporter.warning(s"Unknown range method: $s, choosing default (interval)!")
      }
      case FlagOption("noInitialErrors") => trackInitialErrs = false
      case FlagOption("noRoundoff") => trackRoundoffErrs = false
      case ChoiceOption("precision", s) => s match {
        case "Float32" =>
          uniformPrecision = Float32
          reporter.info(s"using $s")
        case "Float64" =>
          uniformPrecision = Float64
          reporter.info(s"using $s")
        case "DoubleDouble" =>
          uniformPrecision = DoubleDouble
          reporter.info(s"using $s")
        case "QuadDouble" =>
          uniformPrecision = QuadDouble
          reporter.info(s"using $s")
        case "Fixed16" =>
          uniformPrecision = Fixed(16)
        case "Fixed32" =>
          uniformPrecision = Fixed(32)
        case _ =>
          reporter.warning(s"Unknown precision specified: $s, choosing default ($uniformPrecision)!")
      }
      case ParamOption("mixed-precision", _) => mixedPrecision = true
      case _ =>
    }



    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)

    // returns (abs error, result range, interm. errors, interm. ranges)
    val res: Map[Identifier, (Rational, Interval, Map[Expr, Rational], Map[Expr, Interval])] = prg.defs.filter(fnc =>
      !fnc.precondition.isEmpty &&
      !fnc.body.isEmpty &&
      fncsToConsider.contains(fnc.id.toString)).map(fnc => {

      reporter.info("analyzing fnc: " + fnc.id)
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
          inputErrs ++ missingIDs.map(id => (id -> zero))

        } else if (trackRoundoffErrs) {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> precisionMap(id).absRoundoff(inputValMap(id)))).toMap

        } else {

          val allIDs = fnc.params.map(_.id)
          allIDs.map(id => (id -> zero)).toMap

        }

      // TODO: Interval-only based error estimation; should be a very quick fix

      val fncBody = fnc.body.get

      // val (resError: Rational, resRange: Interval) =
      // returns (abs error, result range, interm. errors, interm. ranges)
      (rangeMethod, errorMethod) match {
        case ("interval", "affine") =>
          val (resRange, intermediateRanges) = evalRange[Interval](fncBody, inputValMap, Interval.apply)

          val (resRoundoff, allErrors) = evalRoundoff[AffineForm](fncBody, intermediateRanges,
            precisionMap,
            inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
            zeroError = AffineForm.zero,
            fromError = AffineForm.fromError,
            interval2T = AffineForm.apply,
            constantsPrecision = uniformPrecision,
            trackRoundoffErrs)

          val resError = Interval.maxAbs(resRoundoff.toInterval)

          (fnc.id -> ((resError, resRange,
            allErrors.map({
              case (e, aa) => (e -> Interval.maxAbs(aa.toInterval))
            }),
            intermediateRanges)))

        case ("affine", "affine") =>
          val (resRange, intermediateRanges) = evalRange[AffineForm](fncBody,
            inputValMap.map(x => (x._1 -> AffineForm(x._2))), AffineForm.apply)

          val (resRoundoff, allErrors) = evalRoundoff[AffineForm](fncBody,
            intermediateRanges.map(x => (x._1 -> x._2.toInterval)),
            precisionMap,
            inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
            zeroError = AffineForm.zero,
            fromError = AffineForm.fromError,
            interval2T = AffineForm.apply,
            constantsPrecision = uniformPrecision,
            trackRoundoffErrs)

          val resError = Interval.maxAbs(resRoundoff.toInterval)

          (fnc.id -> ((resError, resRange.toInterval,
            allErrors.map({
              case (e, aa) => (e -> Interval.maxAbs(aa.toInterval))
            }),
            intermediateRanges.mapValues(_.toInterval))))

        case ("smt", "affine") =>
          val (resRange, intermediateRanges) = evalRange[SMTRange](fncBody,
            inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) }),
            SMTRange.apply)

          val (resRoundoff, allErrors) = evalRoundoff[AffineForm](fncBody,
            intermediateRanges.map(x => (x._1 -> x._2.toInterval)),
            precisionMap,
            inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
            zeroError = AffineForm.zero,
            fromError = AffineForm.fromError,
            interval2T = AffineForm.apply,
            constantsPrecision = uniformPrecision,
            trackRoundoffErrs)

          val resError = Interval.maxAbs(resRoundoff.toInterval)

          (fnc.id -> ((resError, resRange.toInterval,
            allErrors.map({
              case (e, aa) => (e -> Interval.maxAbs(aa.toInterval))
            }),
            intermediateRanges.mapValues(_.toInterval))))

        // TODO: interval subdivision; compare with what Anastasiia did
        // case ("subdiv", _) =>
        //   val tmp = doIntervalSubdivision( //evaluateSubdiv(
        //       fnc.body.get, lang.TreeOps.freeVariablesOf(fnc.body.get),
        //       inputValMap,
        //       inputErrorMap,
        //       trackRoundoffErrs,
        //       uniformPrecision)
        //   (tmp._2, tmp._1)

        //   (fnc.id -> (resError, resRange))

        case _ =>
          reporter.fatalError(s"Your combination of $rangeMethod and $errorMethod" +
            "for computing ranges and errors is not supported.")
          null
      }
    }).toMap

    timer.stop
    // ctx.reporter.info(s"Finished $name")
    (ctx.copy(resultAbsoluteErrors = res.mapValues(_._1),
      resultRealRanges = res.mapValues(_._2),
      intermediateAbsErrors = res.mapValues(_._3),
      intermediateRanges = res.mapValues(_._4)), prg)
  }


}
