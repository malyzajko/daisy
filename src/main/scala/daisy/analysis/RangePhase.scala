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
object RangePhase extends PhaseComponent {
  override val name = "Range"
  override val description = "Computes the ranges of intermediate expressions."
  override def apply(cfg: Config) = new RangePhase(cfg, name, "range")
}

class RangePhase(val cfg: Config, val name: String, val shortName: String) extends DaisyPhase
    with tools.RangeEvaluators {
  implicit val debugSection = DebugSectionAnalysis

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    startRun()

    val rangeMethod = cfg.option[String]("rangeMethod")

    val res: Map[Identifier, (Interval, Map[Expr, Interval])] = functionsToConsider(prg).map(fnc => {

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

    finishRun(
      ctx.copy(
        resultRealRanges = res.mapValues(_._1),
        intermediateRanges = res.mapValues(_._2)),
      prg)
  }

}
