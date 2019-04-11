// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import tools.Rational.max
import lang.Identifiers.Identifier
import lang.Constructors.and
import tools._
import tools.FinitePrecision._

import scala.collection.immutable.Map

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
    NumOption(
      "divLimit",
      3,
      "Max amount of interval divisions"),
    NumOption(
      "totalOpt",
      32,
      "Max total amount of analysis runs")
//    StringChoiceOption(
//      "subdiv",
//      Set("simple", "model"),
//      "simple",
//      "Method to subdivide intervals"),
  )

  implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val divLimit = ctx.option[Long]("divLimit").toInt
    val totalOpt = ctx.option[Long]("totalOpt").toInt
    //  val subdiv: String = ctx.option("subdiv")

    // for each function, returns (abs error, rel error, result interval)
    val res = analyzeConsideredFunctions(ctx, prg){ fnc =>

      ctx.reporter.info("analyzing fnc: " + fnc.id)
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)

      val bodyReal = fnc.body.get

      //  Subdivide input ranges
      val subIntervals: Seq[Map[Identifier, Interval]] =
        getEqualSubintervals(inputValMap, divLimit, totalOpt)

      val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id)
      val uniformPrecision = ctx.option[Precision]("precision")

      // TODO: remove this mutable state
      DataflowPhase.rangeMethod = ctx.option[String]("rangeMethod")
      DataflowPhase.errorMethod = ctx.option[String]("errorMethod")

      // Evaluate each input range
      val results = subIntervals.par.map(subInt => {

        val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

        // val (absError, realRange) = evalError(bodyReal, subInt, inputErrorMap,
        //   ctx.option[String]("rangeMethod"), ctx.option[String]("errorMethod"),
        //   ctx.option[Precision]("precision"))

        // TODO fix ugly hack
        val (rangesPrecond, errorsPrecond, addCond) = SpecsProcessingPhase.extractPreCondition(fnc.precondition.get)
        val subIntExpr = subInt.foldLeft(Seq[Expr]())({ case (pre, (id, intv)) =>
          pre :+ LessEquals(RealLiteral(intv.xlo), Variable(id)) :+ LessEquals(Variable(id), RealLiteral(intv.xhi)) })
        val errorsExpr = errorsPrecond.foldLeft(Seq[Expr]())({ case (errs, (id, error)) =>
          errs :+ AbsError(Variable(id), RealLiteral(error)) })
        val precond = and(subIntExpr ++ errorsExpr ++ addCond: _*)
        val (absError, realRange, intermErrors, intermRanges, intermQueries) = DataflowPhase.computeRoundoff(
          subInt, inputErrorMap, precisionMap, bodyReal, uniformPrecision, precond)

        // var failIntervals: List[(Map[Identifier, Interval], Rational)] = List.empty

        val relError: Option[Rational] = if (realRange.includes(Rational.zero)) {
          // failIntervals = failIntervals :+ (x, absError)
          None
        } else {
          Some(absError / Interval.minAbs(realRange))
        }

        // We return the corresponding subdivision 'subInt' here because we need it for the certificate generation.
        ((absError, relError, realRange), (intermErrors, intermRanges, intermQueries), subInt)
      }) // end subIntervals.map

      val (endResults, intermResults, subIntvs) = results.seq.unzip3
      val (absErrors, relErrors, ranges) = endResults.unzip3
      val (intermErrors, intermRanges, intermQueries) = intermResults.unzip3

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

      (totalAbsError, totalRelError, totalRange, totalIntermRanges, totalIntermErrors, intermResults, subIntvs)
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

    (ctx.copy(
      resultAbsoluteErrors = res.mapValues(_._1),
      resultRelativeErrors = res.mapValues(_._2),
      resultRealRanges = res.mapValues(_._3),
      intermediateRanges = res.mapValues(_._4),
      intermediateAbsErrors = res.mapValues(_._5),
      subdivResults = res.mapValues(_._6),
      subdivIntervals = res.mapValues(_._7)),
    prg)
  }

  def evalError(expr: Expr, inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational], precondition: Expr, rangeMethod: String, errorMethod: String,
    precision: Precision): (Rational, Interval) = {

    (rangeMethod, errorMethod) match {
      case ("interval", "affine") =>
        uniformRoundoff_IA_AA(expr, inputValMap, inputErrorMap, precision,
          trackRoundoffErrors = true)

      case ("affine", "affine") =>
        uniformRoundoff_AA_AA(expr, inputValMap, inputErrorMap, precision,
          trackRoundoffErrors = true)

      case ("smt", "affine") =>
        uniformRoundoff_SMT_AA(expr, inputValMap, inputErrorMap, precondition, precision,
          trackRoundoffErrors = true)
    }
  }

}

