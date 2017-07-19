

package daisy
package analysis

//import java.io.FileWriter
//import java.io.BufferedWriter

import lang.Trees.{Expr, _}
import daisy.utils.Rational.{zero, apply => _, _}
import lang.Identifiers.{Identifier, _}
import lang.Types.RealType
import utils.{SMTRange, _}
import Interval._
import daisy.analysis.RangeErrorPhase.{errorAffineAffine, errorIntervalAffine, errorSMTAffine, evaluateSubdiv, functionsToConsider, trackRoundoffErrs}
import daisy.analysis.RelativeErrorPhase.{denormals, reporter}
import daisy.lang.Constructors._
import daisy.lang.NumAnnotation
import daisy.solvers.{Solver, Z3Solver}
import utils.FinitePrecision._
import lang.TreeOps._
import smtlib.parser.Commands.{AttributeOption, SetOption}
import smtlib.parser.Terms.{Attribute, SKeyword, SNumeral, SSymbol}

import scala.collection.immutable.Map
import scala.collection.parallel.{ParSeq, ParSet}
import scala.util.control.Breaks._


/**
  * Compute relative errors through absolute, i.e. not through first computing
  * absolute errors.
  **
  *Uses the (1 + delta) abstraction for floating-point computations.
  **
  *
  *Prerequisites:
  *- SpecsProcessingPhase
 */
object RelThroughAbsPhase extends DaisyPhase with Subdivision with ErrorFunctions{

  // default parameters for the complete run
  var divLimit = 3
  var rangeMethod = "interval"
  var subdiv = "simple"
  var approach = "taylor"
  var uniformPrecision: Precision = Float64
  override var denormals = false

  override val name = "relative error phase"
  override val description = "computes relative errors directly"
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ChoiceOptionDef("rel-rangeMethod", "Method to use for range analysis",
    Set("affine", "interval", "smtreuse", "smtredo"), "interval"),
    ParamOptionDef("divLimit", "Max amount of interval divisions", divLimit.toString),
    ParamOptionDef("totalOpt", "Max total amount of analysis runs", totalOpt.toString),
    ChoiceOptionDef("subdiv", "Method to subdivide intervals", Set("simple", "model"), "simple"),
    ChoiceOptionDef("approach", "Approach for expressions", Set("taylor", "naive"), "taylor"),
    // fixme change name to not overlap with RangeErrorPhase or put into Main
    FlagOptionDef("noRoundoff", "No initial roundoff errors"),
    FlagOptionDef("denormals","Include parameter for denormals in the FP abstraction"))
  val deltaName = "delta"
  val epsilonName = "eps"

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.relAbs.start

    // default range method: intervals
    var rangeMethod = "interval"
    var errorMethod = "affine"   // only one supported so far

    var trackInitialErrs = true

    // setting trait variables
    trackRoundoffErrs = true
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
      case FlagOption("denormals") =>
        reporter.info("Include parameter for denormals")
        denormals = true
      case _ =>
    }

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)

    for (fnc <- prg.defs)
      if (!fnc.precondition.isEmpty && !fnc.body.isEmpty && fncsToConsider.contains(fnc.id.toString)){

        reporter.info("analyzing fnc: " + fnc.id)
        val startTime = System.currentTimeMillis
        val inputValMap: Map[Identifier, Interval] = ctx.inputRanges(fnc.id)

        // If we track both input and roundoff errors, then we pre-compute
        // the roundoff errors for those variables that do not have a user-defined
        // error, in order to keep correlations.
        val inputErrorMap: Map[Identifier, Rational] =
        if (trackInitialErrs && trackRoundoffErrs){

          val inputErrs = ctx.inputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map( id => (id -> uniformPrecision.absRoundoff(inputValMap(id))))

        } else if(trackInitialErrs) {

          val inputErrs = ctx.inputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map( id => (id -> Rational.zero))

        } else if (trackRoundoffErrs) {

          val allIDs = fnc.params.map(_.id)
          allIDs.map( id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))).toMap

        } else {

          val allIDs = fnc.params.map(_.id)
          allIDs.map( id => (id -> Rational.zero)).toMap

        }

        val bodyReal = fnc.body.get
        var failIntervals: List[(Map[Identifier, Interval], Rational)] = List.empty
        //  Subdivide and eval
        val subIntervals = getSubintervals(inputValMap, bodyReal, ctx, subdiv, divLimit)

        val errors = subIntervals.map(x => {
          // TODO: same inputErrorMap?
            val absError = evalError(bodyReal, x, inputErrorMap)
            val range = evalRange(bodyReal, x)
            if (range.xlo <= Rational.zero && Rational.zero <= range.xhi) {
              failIntervals = failIntervals :+ (x, absError)
              None
            } else {
                Some(max(abs(absError / range.xlo), abs(absError / range.xhi)))
              }
            })
        val relError = errors.max(optionAbsOrdering).getOrElse("NaN")

        if (failIntervals.nonEmpty)
          reporter.info("For several sub-intervals it was not possible to compute relative error")
        for(x <- failIntervals){
          val (m, er) = x
          reporter.info(s"absErr: $er on $m")
        }
        reporter.info(s"relError: $relError, time: " +
          (System.currentTimeMillis - startTime))
      }

    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, prg)
  }

  def evalError(expr:Expr,
           inputValMap: Map[Identifier, Interval],
           inputErrorMap:  Map[Identifier, Rational],
           errorMethod:String = "affine"): Rational = {
    (rangeMethod, errorMethod) match {
      case ("interval", "affine") =>
        errorIntervalAffine(expr, inputValMap, inputErrorMap, uniformPrecision)

      case ("affine", "affine") =>
        errorAffineAffine(expr, inputValMap, inputErrorMap, uniformPrecision)

      case ("smt", "affine") =>
        errorSMTAffine(expr, inputValMap, inputErrorMap, uniformPrecision)

//      // default is to use the method that attaches the info to trees.
//      case ("subdiv", _) =>
//        evaluateSubdiv(
//          expr,
//          inputValMap,
//          inputErrorMap,
//          trackRoundoffErrs,
//          uniformPrecision)

      case _ =>
        reporter.fatalError(s"Your combination of $rangeMethod and $errorMethod" +
          "for computing ranges and errors is not supported.")
    }
  }

  def evalRange(expr:Expr,
               inputValMap: Map[Identifier, Interval]): Interval = {
    rangeMethod match {
      case "interval" =>
        Evaluators.evalInterval(expr, inputValMap)

      case "affine" =>
        Evaluators.evalAffine(
          expr, inputValMap.map(x => (x._1 -> AffineForm(x._2)))).toInterval

      case "smt" =>
        Evaluators.evalSMT(
          expr, inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval

      case _ =>
        reporter.fatalError(s"The range method $rangeMethod is not supported.")
    }
  }

}

