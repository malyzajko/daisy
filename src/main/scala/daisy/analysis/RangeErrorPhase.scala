
package daisy
package analysis

import lang.NumAnnotation
import lang.Trees._
import utils._
import FinitePrecision._
import Rational._
import lang.Identifiers._

/**
  ??? Description goes here


  Prerequisites:
    - SpecsProcessingPhase
 */
object RangeErrorPhase extends DaisyPhase with RoundoffEvaluators with IntervalSubdivision {

  override val name = "range-error phase"
  override val description = "Computes ranges and absolute errors"

  val optionPrecision = ChoiceOptionDef("precision", "Type of precision to use",
      Set("Float32", "Float64", "DoubleDouble", "QuadDouble",
        "Fixed16", "Fixed32"), "Float64")

  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ChoiceOptionDef("rangeMethod", "Method to use for range analysis",
      Set("affine", "interval", "smt", "subdiv"), "interval"),
    FlagOptionDef("noInitialErrors", "do not track initial errors specified by user"),
    FlagOptionDef("noRoundoff", "do not track roundoff errors"),
    optionPrecision
    )

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
      case _ =>
    }

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)

    val res: Map[Identifier, (Rational, Interval)] = prg.defs.filter(fnc =>
      !fnc.precondition.isEmpty &&
      !fnc.body.isEmpty &&
      fncsToConsider.contains(fnc.id.toString)).map(fnc => {

      reporter.info("analyzing fnc: " + fnc.id)
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      // If we track both input and roundoff errors, then we pre-compute
      // the roundoff errors for those variables that do not have a user-defined
      // error, in order to keep correlations.
      val inputErrorMap: Map[Identifier, Rational] =
        if (trackInitialErrs && trackRoundoffErrs){

          val inputErrs = ctx.specInputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map( id => (id -> uniformPrecision.absRoundoff(inputValMap(id))))

        } else if(trackInitialErrs) {

          val inputErrs = ctx.specInputErrors(fnc.id)
          val allIDs = fnc.params.map(_.id).toSet
          val missingIDs = allIDs -- inputErrs.keySet
          inputErrs ++ missingIDs.map( id => (id -> zero))

        } else if (trackRoundoffErrs) {

          val allIDs = fnc.params.map(_.id)
          allIDs.map( id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))).toMap

        } else {

          val allIDs = fnc.params.map(_.id)
          allIDs.map( id => (id -> zero)).toMap

        }

      // TODO: Interval-only based error estimation; should be a very quick fix


      val (resError: Rational, resRange: Interval) = (rangeMethod, errorMethod) match {
        case ("interval", "affine") =>
          uniformRoundoff_IA_AA(fnc.body.get, inputValMap, inputErrorMap, uniformPrecision, trackRoundoffErrs)

        case ("affine", "affine") =>
          uniformRoundoff_AA_AA(fnc.body.get, inputValMap, inputErrorMap, uniformPrecision, trackRoundoffErrs)

        case ("smt", "affine") =>
          uniformRoundoff_SMT_AA(fnc.body.get, inputValMap, inputErrorMap, uniformPrecision, trackRoundoffErrs)

        // default is to use the method that attaches the info to trees.
        case ("subdiv", _) =>
          val tmp = doIntervalSubdivision( //evaluateSubdiv(
              fnc.body.get, lang.TreeOps.freeVariablesOf(fnc.body.get),
              inputValMap,
              inputErrorMap,
              trackRoundoffErrs,
              uniformPrecision)
          (tmp._2, tmp._1)

        case _ =>
          reporter.fatalError(s"Your combination of $rangeMethod and $errorMethod" +
            "for computing ranges and errors is not supported.")
          null
      }
      (fnc.id -> (resError, resRange))
    }).toMap

    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx.copy(resultAbsoluteErrors = res.map(x => (x._1 -> x._2._1)),
      resultRealRanges = res.map(x => (x._1 -> x._2._2))), prg)
  }


}
