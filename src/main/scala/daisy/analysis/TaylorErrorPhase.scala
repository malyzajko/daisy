// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import lang.Identifiers.{FreshIdentifier, Identifier}
import lang.TreeOps._
import lang.Types.RealType
import tools.FinitePrecision._
import tools.{Rational, Evaluators, Interval, AffineForm, SMTRange}
import Rational._
import Interval._

import scala.collection.immutable.Map
import scala.collection.parallel.ParSeq

/**
 * Computes absolute errors same way as in RangeErrorPhase,
 * but evaluates taylor simplifications instead
 */
object TaylorErrorPhase extends DaisyPhase with tools.Subdivision with tools.Taylor with tools.RangeEvaluators {
  override val name: String = "Taylor Absolute Error"
  override val shortName: String = "taylor"
  override val description: String = "Computes abssolut error using taylor simplifications."

  implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    // default range method: intervals
    val rangeMethod: String = ctx.option[String]("rangeMethod")
    var subdiv = ctx.hasFlag("subdiv")

    val res: Map[Identifier, (Rational, Interval)] =
      functionsToConsider(ctx, prg).map(fnc => {

      ctx.reporter.info("analyzing fnc: " + fnc.id)
      val startTime = System.currentTimeMillis

      if (subdiv) {
        val subIntervals: Seq[Map[Identifier, Interval]] =
           getEqualSubintervals(ctx.specInputRanges(fnc.id), 3)

        val errors = subIntervals.par.map(subInt =>
          evalTaylor(ctx, fnc.body.get, subInt, rangeMethod)._1
        )
        val totalAbsError = errors.tail.fold(errors.head)({
          case (x, y) => max(x, y)
        })
        // TODO: also do this for ranges
        (fnc.id -> (totalAbsError, Interval(Rational.zero)))
      } else {
        val (error, interval) = evalTaylor(ctx, fnc.body.get, ctx.specInputRanges(fnc.id), rangeMethod)
        ctx.reporter.debug("absError: " + error.toString + ", time: " +
          (System.currentTimeMillis - startTime))

        (fnc.id -> (error, interval))
      }
    }).toMap

    (ctx.copy(resultAbsoluteErrors = res.mapValues(_._1),
      resultRealRanges = res.mapValues(_._2)),
      prg)
  }


  def evalTaylor(ctx: Context, bodyReal: Expr, inputRanges: Map[Identifier, Interval],
    rangeMethod: String): (Rational, Interval) = {
    val containsLet = lang.TreeOps.exists { case Let(_,_,_) => true }(bodyReal)
    if (containsLet) {
      ctx.reporter.error("The Taylor approach currently does not support Let definitions.")
    }

    val deltaVarMap = mapDeltasToVars(bodyReal)
    val epsVarMap = mapEpsilonsToVars(bodyReal)

    // derive f~(x)
    val (bodyDelta, transDeltas) = deltaAbstract(bodyReal, deltaVarMap, epsVarMap, ctx.hasFlag("denormals"))
    //println(transDeltas)

    // get set of partial derivatives wrt deltas
    val taylor = getDerivative(bodyDelta)
    //println("derivative: " + taylor)

    // add constraints for deltas
    val deltas = deltasOf(bodyDelta)
    var deltaIntervalMap: Map[Identifier, Interval] = Map.empty
    for (delta <- deltas if !transDeltas.contains(delta.id)){
      deltaIntervalMap = deltaIntervalMap +
        (delta.id -> Interval(-Float64.machineEpsilon, Float64.machineEpsilon))
    }
    for (delta <- deltas if transDeltas.contains(delta.id)){
      deltaIntervalMap = deltaIntervalMap +
        (delta.id -> Interval(-Float64.machineEpsilon*2, Float64.machineEpsilon*2))
    }
    val eps = epsilonsOf(bodyDelta)
    deltaIntervalMap = deltaIntervalMap ++ eps.map(e => (e.id -> epsilonIntervalFloat64))

    val inputValMap: Map[Identifier, Interval] = inputRanges ++ deltaIntervalMap
    //println(inputValMap)

    val (tmpErr, interval) = rangeMethod match {
      case "interval" =>
        // evaluate each partial derivative
        val err = taylor.map(x => {
          // replace all deltas with zeros to get f~(x,0)
          val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
          //println("simplified: " + tmp)
          //maxAbs(Evaluators.evalInterval(tmp, inputValMap))
          maxAbs(evalRange[Interval](tmp, inputValMap, Interval.apply)._1)
        })
        val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
        //(finalErr, Evaluators.evalInterval(bodyReal, inputValMap))
        (finalErr, evalRange[Interval](bodyReal, inputValMap, Interval.apply)._1)

      case "affine" =>
        val err = taylor.map(x => {
          // replace all deltas with zeros to get f~(x,0)
          val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
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
          // replace all deltas with zeros to get f~(x,0)
          val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
          // evaluate the term and take the upper bound of resulting interval
          // maxAbs(Evaluators.evalSMT(tmp, inputValMap.map({
          //   case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)
          maxAbs(evalRange[SMTRange](tmp, inputValMap.map({
            case (id, int) => (id -> SMTRange(Variable(id), int)) }), SMTRange.apply)._1.toInterval)
        })
        val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
        // (finalErr, Evaluators.evalSMT(
        //   bodyReal, inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)
        (finalErr, evalRange[SMTRange](
          bodyReal, inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) }),
          SMTRange.apply)._1.toInterval)

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
