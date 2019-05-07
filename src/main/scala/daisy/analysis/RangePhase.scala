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
  override val name = "Range"
  override val shortName = "range"
  override val description = "Computes the ranges of intermediate expressions."

  implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val rangeMethod = ctx.option[String]("rangeMethod")

    val res: Map[Identifier, (Interval, Map[(Expr, PathCond), Interval])] = analyzeConsideredFunctions(ctx, prg){ fnc =>

      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      rangeMethod match {
        case "interval" =>
          val (resRange, intermediateRanges) =
            evalRange[Interval](fnc.body.get, inputValMap, Interval.apply)

          (resRange, intermediateRanges)

        case "affine" =>
          val (resRange, intermediateRanges) = evalRange[AffineForm](fnc.body.get,
            inputValMap.map(x => (x._1 -> AffineForm(x._2))), AffineForm.apply)

          (resRange.toInterval, intermediateRanges.mapValues(_.toInterval))

          case "smt" =>
            val precond = fnc.precondition.get
            val (resRange, intermediateRanges) = evalRange[SMTRange](fnc.body.get,
              inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, precond)) }),
              SMTRange.apply(_, precond))

          (resRange.toInterval, intermediateRanges.mapValues(_.toInterval))


        // case "subdiv" =>
        //   evaluateSubdiv(fnc.body.get, ctx.specInputRanges(fnc.id), Map.empty)

      }

    }

    (ctx.copy(resultRealRanges = res.mapValues(_._1),
      intermediateRanges = res.mapValues(_._2)),
      prg)
  }

}
