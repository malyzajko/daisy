// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Identifiers._
import lang.Trees._
import tools.{Interval, AffineForm, SMTRange}


/**
  This phase annotates every numerical expression with an interval
  of the real values the expression can take, according to the contract.
  Several options for how to perform this analysis are available:
  - classic intervals with rational bounds
  - classic affine arithmetic over rationals

  Note:
    - assumes the input variables have ranges annotated already, or else
      have already been assigned
    - does not support sqrt (for now)
    - assumes the body is only an arithmetic expression with let stmts,
      but without if-then-else, loops, etc.
    - uses the same method for all expressions


  Prerequisite:
    - SpecsProcessingPhase
 */
object RangePhase extends DaisyPhase with tools.RangeEvaluators {

  override val name = "Range phase"
  override val description = "Computes the ranges of intermediate expressions."
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info("\nStarting range phase")
    val timer = ctx.timers.ranges.start

    // default
    var rangeMethod: String = "interval"

    // Process relevant options.
    for (opt <- ctx.options) opt match {
      case ChoiceOption("rangeMethod", s) => s match {
        case "interval" | "affine" | "smt" | "subdiv" =>
          rangeMethod = s
          reporter.info(s"using $s")
        case _ =>
          reporter.warning(s"Unknown range method: $s, choosing default (interval)!")
      }
      case _ =>
    }

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)

    val res: Map[Identifier, (Interval, Map[Expr, Interval])] = prg.defs.filter(fnc =>
      !fnc.precondition.isEmpty &&
      !fnc.body.isEmpty &&
      fncsToConsider.contains(fnc.id.toString)).map(fnc => {

      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      rangeMethod match {
        case "interval" =>
          val (resRange, intermediateRanges) =
            evalRange[Interval](fnc.body.get, inputValMap, Interval.apply)

          (fnc.id -> (resRange, intermediateRanges))

        case "affine" =>
          val (resRange, intermediateRanges) = evalRange[AffineForm](fnc.body.get,
            inputValMap.map(x => (x._1 -> AffineForm(x._2))), AffineForm.apply)

          (fnc.id -> (resRange.toInterval, intermediateRanges.mapValues(_.toInterval)))

        case "smt" =>
          val (resRange, intermediateRanges) = evalRange[SMTRange](fnc.body.get,
            inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) }),
            SMTRange.apply)

          (fnc.id -> (resRange.toInterval, intermediateRanges.mapValues(_.toInterval)))


        // case "subdiv" =>
        //   evaluateSubdiv(fnc.body.get, ctx.specInputRanges(fnc.id), Map.empty)

      }

    }).toMap

    timer.stop
    // ctx.reporter.info("Finished range phase\n")

    (ctx.copy(resultRealRanges = res.mapValues(_._1),
      intermediateRanges = res.mapValues(_._2)),
      prg)
  }

}