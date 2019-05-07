// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.collection.immutable.Map
import scala.collection.parallel.{ParSeq}

import daisy.solvers.Solver
import lang.Trees._
import tools.Rational.max
import lang.Identifiers.Identifier
import lang.Constructors.and
import tools._
import tools.FinitePrecision._

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
  override val name = "Forward Dataflow with Subdivision"
  override val shortName = "subdiv"
  override val description = "Forward dataflow with subdivision"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("divLimit", 3, "Max amount of interval divisions"),
    NumOption("totalOpt", 32, "Max total amount of analysis runs")
  )

  implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val divLimit = ctx.option[Long]("divLimit").toInt
    val totalOpt = ctx.option[Long]("totalOpt").toInt
    
    // for each function, returns (abs error, rel error, result interval)
    val res = analyzeConsideredFunctions(ctx, prg){ fnc =>

      ctx.reporter.info("analyzing fnc: " + fnc.id)
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val bodyReal = fnc.body.get

      //  Subdivide input ranges
      val subIntervals: ParSeq[Map[Identifier, Interval]] =
        getEqualSubintervals(inputValMap, divLimit, -1, totalOpt).par

      val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id)
      val uniformPrecision = ctx.option[Precision]("precision")

      // TODO: remove this mutable state
      DataflowPhase.rangeMethod = ctx.option[String]("rangeMethod")
      DataflowPhase.errorMethod = ctx.option[String]("errorMethod")

      val addConstr = ctx.specAdditionalConstraints(fnc.id)

      // Remove unfeasible intervals from the list of intervals using SMT solvers.
      val parSubInt = cleanSubIntervals(subIntervals, addConstr)

      uniformPrecision match {
        case FixedPrecision(_) =>
          val _ranges = parSubInt.par.map(subInt => {

            val subIntExpr = subInt.foldLeft(Seq[Expr]())({ case (pre, (id, intv)) =>
              pre :+ LessEquals(RealLiteral(intv.xlo), Variable(id)) :+ LessEquals(Variable(id), RealLiteral(intv.xhi))
            })
            val precond = and(subIntExpr :+ addConstr: _*)
            val (realRange, intermRanges) = DataflowPhase.computeRange(subInt, bodyReal, precond)

            (realRange, intermRanges)
          })

          val (ranges, intermRanges) = _ranges.unzip

          val totalRange = ranges.tail.fold(ranges.head)({
            case (x, y) => x.union(y)
          })

          val totalIntermRanges = intermRanges.tail.fold(intermRanges.head)({
            case (acc, m) =>
              acc.keys.map(key => (key -> acc(key).union(m(key)))).toMap
          })

          val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

          val (totalAbsError, totalIntermErrors) = DataflowPhase.computeErrors(totalIntermRanges, inputErrorMap,
            precisionMap, bodyReal, uniformPrecision)

          val totalRelError: Option[Rational] = if (totalRange.includes(Rational.zero)) {
            None
          } else {
            Some(totalAbsError / Interval.minAbs(totalRange))
          }

          (totalAbsError, totalRelError, totalRange, totalIntermRanges, totalIntermErrors)

        case _ =>
          val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

          val results = parSubInt.par.map(subInt => {
            val subIntExpr = subInt.foldLeft(Seq[Expr]())({ case (pre, (id, intv)) =>
              pre :+ LessEquals(RealLiteral(intv.xlo), Variable(id)) :+ LessEquals(Variable(id), RealLiteral(intv.xhi))
            })
            val precond = and(subIntExpr :+ addConstr: _*)
            val (absError, realRange, _intermErrors, _intermRanges) =
              DataflowPhase.computeRoundoff(subInt, inputErrorMap, precisionMap, bodyReal, uniformPrecision, precond)

            val relError: Option[Rational] = if (realRange.includes(Rational.zero)) {
              None
            } else {
              Some(absError / Interval.minAbs(realRange))
            }

            (absError, relError, realRange, _intermRanges, _intermErrors)
          })

          val absErrors = results.map(_._1)
          val relErrors = results.map(_._2)
          val ranges = results.map(_._3)
          val intermRanges = results.map(_._4)
          val intermErrors = results.map(_._5)

          // Merge results
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

          val totalIntermRanges = intermRanges.tail.fold(intermRanges.head)({
            case (acc, m) =>
              acc.keys.map(key => (key -> acc(key).union(m(key)))).toMap
          })


          val totalIntermErrors = intermErrors.tail.fold(intermErrors.head)({
            case (acc, m) =>
              acc.keys.map(key => (key -> max(acc(key), m(key)))).toMap
          })

          (totalAbsError, totalRelError, totalRange, totalIntermRanges, totalIntermErrors)

          // val relError = errors.max(optionAbsOrdering).getOrElse("NaN")

          // if (failIntervals.nonEmpty)
          //   ctx.reporter.info("For several sub-intervals it was not possible to compute relative error")
          // for(x <- failIntervals){
          //   val (m, er) = x
          //   ctx.reporter.info(s"absErr: $er on $m")
          // }
          // ctx.reporter.info(s"relError: $relError, time: " +
          //   (System.currentTimeMillis - startTime))
      }
    }

    (ctx.copy(
      resultAbsoluteErrors = res.mapValues(_._1),
      resultRelativeErrors = res.mapValues(_._2),
      resultRealRanges = res.mapValues(_._3),
      intermediateRanges = res.mapValues(_._4),
      intermediateAbsErrors = res.mapValues(_._5)),
    prg)
  }

  /**
   *
   * @param intervals list of intervals after the subdivision of input ranges
   * @param addConstr additional constraints given in the precondition
   * @return only the intervals satisfying the preconditions
   */
  def cleanSubIntervals(intervals: ParSeq[Map[Identifier, Interval]], addConstr: Expr):
  ParSeq[Map[Identifier, Interval]] = {

    val tempIntervals = intervals.filter{ interval =>

      val subIntExpr = interval.foldLeft(Seq[Expr]())({ case (pre, (id, intv)) =>
        pre :+ LessEquals(RealLiteral(intv.xlo), Variable(id)) :+ LessEquals(Variable(id), RealLiteral(intv.xhi))
      })
      val solverQuery = and(subIntExpr :+ addConstr: _*)
      val res = Solver.checkSat(solverQuery)

      res match {
        case Some(false) => false // UNSAT
        case Some(true) => true // SAT
        case _ => true // In case you don't know you can't discard the square
      }
    }

    tempIntervals
  }

}

