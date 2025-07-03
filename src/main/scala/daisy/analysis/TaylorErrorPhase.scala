// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.Identifiers.Identifier
import lang.TreeOps._
import tools.FinitePrecision._
import tools.{Rational, Interval, AffineForm, SMTRange}
import Rational._
import Interval._

import scala.collection.immutable.Map
import scala.collection.parallel.CollectionConverters._

/**
 * Computes absolute errors same way as in RangeErrorPhase,
 * but evaluates taylor simplifications instead
 */
object TaylorErrorPhase extends DaisyPhase with tools.Subdivision with tools.Taylor with tools.RangeEvaluators {
  override val name: String = "Taylor error"
  override val description: String = "Computes absolute error using taylor simplifications."
  override implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    // default range method: intervals
    val rangeMethod: String = ctx.option[String]("rangeMethod")
    val subdiv = ctx.hasFlag("subdiv")

    val res: Map[Identifier, (Rational, Interval)] = analyzeConsideredFunctions(ctx, prg){ fnc =>

      val body = daisy.lang.TreeOps.inline(fnc.body.get)
      ctx.reporter.info("analyzing fnc: " + fnc.id)
      val startTime = System.currentTimeMillis

      if (subdiv) {
        val subIntervals: Seq[Map[Identifier, Interval]] =
           getEqualSubintervals(ctx.specInputRanges(fnc.id), 3)

        val errors = subIntervals.par.map(subInt =>
          evalTaylor(ctx, body, subInt, fnc.precondition.get, rangeMethod)._1
        )
        val totalAbsError = errors.tail.fold(errors.head)({
          case (x, y) => max(x, y)
        })
        // TODO: also do this for ranges
        (totalAbsError, Interval(Rational.zero))
      } else {
        val (error, interval) = evalTaylor(ctx, body, ctx.specInputRanges(fnc.id), fnc.precondition.get, rangeMethod)
        ctx.reporter.debug("absError: " + error.toString + ", time: " +
          (System.currentTimeMillis - startTime))

        (error, interval)
      }
    }

    (ctx.copy(resultAbsoluteErrors = res.mapValues(_._1).toMap,
      resultRealRanges = res.mapValues(_._2).toMap),
      prg)
  }


  def evalTaylor(ctx: Context, bodyReal: Expr, inputRanges: Map[Identifier, Interval],
    precondition: Expr, rangeMethod: String): (Rational, Interval) = {

    // derive f~(x)
    val (bodyDelta, transEpsilons , _, _) = epsilonDeltaAbstract(bodyReal, ctx.hasFlag("denormals"))
    //println(transDeltas)

    // get set of partial derivatives wrt epsilons
    val taylor = getDerivative(bodyDelta)
    //println("derivative: " + taylor)

    // add constraints for deltas and epsilons
    val epsilons = epsilonsOf(bodyDelta)
    var epsilonIntervalMap: Map[Identifier, Interval] = Map.empty
    for (epsilon <- epsilons if !transEpsilons.contains(epsilon.id)){
      epsilonIntervalMap = epsilonIntervalMap +
        (epsilon.id -> Interval(-Float64.machineEpsilon, Float64.machineEpsilon))
    }
    for (epsilon <- epsilons if transEpsilons.contains(epsilon.id)){
      epsilonIntervalMap = epsilonIntervalMap +
        (epsilon.id -> Interval(-Float64.machineEpsilon*2, Float64.machineEpsilon*2))
    }
    val delta = deltasOf(bodyDelta)
    epsilonIntervalMap = epsilonIntervalMap ++ delta.map(d => (d.id -> deltaIntervalFloat64))

    val inputValMap: Map[Identifier, Interval] = inputRanges ++ epsilonIntervalMap
    //println(inputValMap)

    val (tmpErr, interval) = rangeMethod match {
      case "interval" =>
        // evaluate each partial derivative
        val err = taylor.map(x => {
          // replace all deltas with zeros to get f~(x,0)
          val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Epsilon(x._2)))
          //println("simplified: " + tmp)
          //maxAbs(Evaluators.evalInterval(tmp, inputValMap))
          maxAbs(evalRange[Interval](tmp, inputValMap, Interval.apply)._1)
        })
        val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
        //(finalErr, Evaluators.evalInterval(bodyReal, inputValMap))
        (finalErr, evalRange[Interval](bodyReal, inputValMap, Interval.apply)._1)

      case "affine" =>
        val err = taylor.map(x => {
          // replace all deltas and epsilons with zeros to get f~(x,0)
          val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Epsilon(x._2)))
          // maxAbs(Evaluators.evalAffine(
          //   tmp, inputValMap.map(y => (y._1 -> AffineForm(y._2)))).toInterval)
          maxAbs(evalRange[AffineForm](tmp, inputValMap.map(y => (y._1 -> AffineForm(y._2))),
            AffineForm.apply)._1.toInterval)

        })

        val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
        //(finalErr, Evaluators.evalAffine(bodyReal, inputValMap.map(x => (x._1 -> AffineForm(x._2)))).toInterval)
        (finalErr, evalRange[AffineForm](bodyReal, inputValMap.map(x => (x._1 -> AffineForm(x._2))),
          AffineForm.apply)._1.toInterval)


      case "smt" =>
        val err = taylor.map(x => {
          // replace all deltas and epsilons with zeros to get f~(x,0)
          val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Epsilon(x._2)))
          // evaluate the term and take the upper bound of resulting interval
          // maxAbs(Evaluators.evalSMT(tmp, inputValMap.map({
          //   case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)
          maxAbs(evalRange[SMTRange](tmp, inputValMap.map({
            case (id, int) => (id -> SMTRange(Variable(id), int, precondition)) }),
            SMTRange.apply(_, precondition))._1.toInterval)
        })
        val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
        // (finalErr, Evaluators.evalSMT(
        //   bodyReal, inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)
        (finalErr, evalRange[SMTRange](
          bodyReal, inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, precondition)) }),
          SMTRange.apply(_, precondition))._1.toInterval)

      case _ =>
        ctx.reporter.fatalError(s"$rangeMethod is not supported.")
    }

    // compute the remainder term for taylor series
    val taylorRemainder = getTaylorRemainder(bodyDelta, Seq(inputValMap))
    ctx.reporter.debug(s"The taylor remainder value is $taylorRemainder")

    // add the remainder to the error
    // TODO: shouldn't this fail, if the remainder cannot be computed?
    val error = tmpErr + taylorRemainder.getOrElse(Rational.zero)


    (error, interval)
  }

}
