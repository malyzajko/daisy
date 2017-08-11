// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees.{Expr, _}
import tools.Rational.{zero, abs, max}
import lang.Identifiers.Identifier
import lang.Types.RealType
import tools._
import Interval._
import lang.Constructors._
import solvers.{Solver, Z3Solver}
import tools.FinitePrecision._
import lang.TreeOps._
import smtlib.parser.Commands.{AttributeOption, SetOption}
import smtlib.parser.Terms.{Attribute, SKeyword, SNumeral, SSymbol}

import scala.collection.immutable.Map
import scala.collection.parallel.{ParSeq, ParSet}
import scala.util.control.Breaks._


/**
 * Compute relative errors through absolute, i.e. not through first computing
 * absolute errors.
 *
 * Uses the (1 + delta) abstraction for floating-point computations.
 *
 *
 * Prerequisites:
 * - SpecsProcessingPhase
 */
object DataflowSubdivisionPhase extends DaisyPhase with Subdivision with RoundoffEvaluators {

  override val name = "forward dataflow with subdivision"
  override val description = "computes relative errors directly"

  // default parameters for the complete run
  var divLimit = 3

  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ParamOptionDef("divLimit", "Max amount of interval divisions", divLimit.toString),
    ParamOptionDef("totalOpt", "Max total amount of analysis runs", totalOpt.toString)
    // ChoiceOptionDef("subdiv", "Method to subdivide intervals", Set("simple", "model"), "simple"),
  )

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  var rangeMethod = "interval"
  var errorMethod = "affine"   // only one supported so far

  var subdiv = "simple"
  var approach = "taylor"
  var uniformPrecision: Precision = Float64

  var trackInitialErrs = true
  trackRoundoffErrs = true

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.relAbs.start

    // process relevant options
    for (opt <- ctx.options) opt match {
      case ChoiceOption("rangeMethod", s) => s match {
        case "interval" | "affine" | "smt" =>
          rangeMethod = s
          reporter.info(s"using $s")
        case _ =>
          reporter.warning(s"Unknown range method: $s, choosing default (interval)!")
      }
      case ParamOption("divLimit", value) => divLimit = value.toInt
      case ParamOption("totalOpt", value) => totalOpt = value.toInt
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
      case _ =>
    }

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)

    // for each function, returns (abs error, rel error, result interval)
    val res: Map[Identifier, (Rational, Option[Rational], Interval)] = prg.defs.filter(fnc =>
      !fnc.precondition.isEmpty &&
      !fnc.body.isEmpty &&
      fncsToConsider.contains(fnc.id.toString)).map(fnc => {

        reporter.info("analyzing fnc: " + fnc.id)
        val startTime = System.currentTimeMillis
        val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

        val bodyReal = fnc.body.get

        //  Subdivide input ranges
        val subIntervals: Seq[Map[Identifier, Interval]] =
          getEqualSubintervals(inputValMap, divLimit)

        // Evaluate each input range
        val errors: Seq[(Rational, Option[Rational], Interval)] = subIntervals.map(subInt => {

          // If we track both input and roundoff errors, then we pre-compute
          // the roundoff errors for those variables that do not have a user-defined
          // error, in order to keep correlations.
          val inputErrorMap: Map[Identifier, Rational] = if (trackInitialErrs && trackRoundoffErrs){

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

          val (absError, realRange) = evalError(bodyReal, subInt, inputErrorMap)

          // var failIntervals: List[(Map[Identifier, Interval], Rational)] = List.empty

          val relError: Option[Rational] = if (realRange.xlo <= Rational.zero && Rational.zero <= realRange.xhi) {
            // failIntervals = failIntervals :+ (x, absError)
            None
          } else {
            Some(max(abs(absError / realRange.xlo), abs(absError / realRange.xhi)))
          }

          (absError, relError, realRange)
        }) // end subIntervals.map

        // Merge results
        val (absErrors, relErrors, ranges) = errors.unzip3

        val totalAbsError = absErrors.tail.fold(absErrors.head)({
          case (x, y) => max(x, y)
          })
        val totalRelError = relErrors.tail.fold(relErrors.head)({
          case (None, _) | (_, None) => None
          case (Some(x), Some(y)) => Some(max(x, y))
          })

        val totalRange = ranges.tail.fold(ranges.head)({
          case (x, y) => x.union(y)
          })

        (fnc.id -> (totalAbsError, totalRelError, totalRange))
        // val relError = errors.max(optionAbsOrdering).getOrElse("NaN")

        // if (failIntervals.nonEmpty)
        //   reporter.info("For several sub-intervals it was not possible to compute relative error")
        // for(x <- failIntervals){
        //   val (m, er) = x
        //   reporter.info(s"absErr: $er on $m")
        // }
        // reporter.info(s"relError: $relError, time: " +
        //   (System.currentTimeMillis - startTime))
      }).toMap

    timer.stop
    reporter.info(s"Finished $name")

    (ctx.copy(resultAbsoluteErrors = res.mapValues(_._1),
      resultRelativeErrors = res.mapValues(_._2),
      resultRealRanges = res.mapValues(_._3)), prg)
  }

  def evalError(expr: Expr, inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational], errorMethod: String = "affine"): (Rational, Interval) = {

    (rangeMethod, errorMethod) match {
      case ("interval", "affine") =>
        uniformRoundoff_IA_AA(expr, inputValMap, inputErrorMap, uniformPrecision,
          trackRoundoffErrors = true)

      case ("affine", "affine") =>
        uniformRoundoff_AA_AA(expr, inputValMap, inputErrorMap, uniformPrecision,
          trackRoundoffErrors = true)

      case ("smt", "affine") =>
        uniformRoundoff_SMT_AA(expr, inputValMap, inputErrorMap, uniformPrecision,
          trackRoundoffErrors = true)

      case _ =>
        reporter.fatalError(s"Your combination of $rangeMethod and $errorMethod" +
          "for computing ranges and errors is not supported.")
    }
  }

}

