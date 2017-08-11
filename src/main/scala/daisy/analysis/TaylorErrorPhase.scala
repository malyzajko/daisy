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
object TaylorErrorPhase extends DaisyPhase with tools.Taylor with tools.RangeEvaluators {

  override var reporter: Reporter = null
  override val name: String = "taylor abs error phase"
  override val description: String = "computes abs error using taylor simplifications"

  implicit val debugSection = DebugSectionAnalysis

  val zeroNum = Rational.zero
  denormals = false

  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  override def run(ctx: Context, prg: Program): (Context, Program) = {

    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")

    val timer = ctx.timers.taylor.start

    // default range method: intervals
    var rangeMethod = "interval"

    // process relevant options
    for (opt <- ctx.options) opt match {
      case ChoiceOption("rangeMethod", s) => s match {
        case "interval" | "affine" | "smt" | "subdiv" =>
          rangeMethod = s
          reporter.info(s"using $s")
        case _ =>
          reporter.warning(s"Unknown range method: $s, choosing default (interval)!")
      }

      case FlagOption("denormals") =>
        reporter.info("Include parameter for denormals")
        denormals = true
      case _ =>
    }

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)

    val res: Map[Identifier, (Rational, Interval)] = prg.defs.filter(fnc =>
      !fnc.precondition.isEmpty &&
      !fnc.body.isEmpty &&
      fncsToConsider.contains(fnc.id.toString)).map(fnc => {

      reporter.info("analyzing fnc: " + fnc.id)
      val startTime = System.currentTimeMillis

      val bodyReal = fnc.body.get

      val containsLet = lang.TreeOps.exists {
        case (l: Let) => true
        case _ => false
      }(bodyReal)
      if (containsLet) {
        reporter.error("The Taylor approach currently does not support Let definitions.")
      }


      val deltaVarMap = mapDeltasToVars(bodyReal)
      val epsVarMap = mapEpsilonsToVars(bodyReal)

      // derive f~(x)
      val bodyDelta = deltaAbstract(bodyReal, deltaVarMap, epsVarMap)

      // get set of partial derivatives wrt deltas
      val taylor = getDerivative(bodyDelta)

      // add constraints for deltas
      val deltas = deltasOf(bodyDelta)
      var deltaIntervalMap: Map[Identifier, Interval] = Map.empty
      for (delta <- deltas){
        deltaIntervalMap = deltaIntervalMap +
          (delta.id -> Interval(-Float64.machineEpsilon, Float64.machineEpsilon))
      }
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id) ++ deltaIntervalMap

      val (tmpErr, interval) = rangeMethod match {
        case "interval" =>
          // evaluate each partial derivative
          val err = taylor.map(x => {
            // replace all deltas with zeros to get f~(x,0)
            val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))

            maxAbs(Evaluators.evalInterval(tmp, inputValMap))
          })
          val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
          (finalErr, Evaluators.evalInterval(fnc.body.get, inputValMap))

        case "affine" =>
          val err = taylor.map(x => {
            // replace all deltas with zeros to get f~(x,0)
            val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
            maxAbs(Evaluators.evalAffine(
              tmp, inputValMap.map(y => (y._1 -> AffineForm(y._2)))).toInterval)
          })

          val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
          (finalErr, Evaluators.evalAffine(
            fnc.body.get, inputValMap.map(x => (x._1 -> AffineForm(x._2)))).toInterval)

        case "smt" =>
          val err = taylor.map(x => {
            // replace all deltas with zeros to get f~(x,0)
            val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
            // evaluate the term and take the upper bound of resulting interval
            maxAbs(Evaluators.evalSMT(tmp, inputValMap.map({
              case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)
          })
          val finalErr = err.fold(Rational.zero)({case (x, y) => x + y})
          (finalErr, Evaluators.evalSMT(
            fnc.body.get, inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)

        case _ =>
          reporter.fatalError(s"$rangeMethod is not supported.")
      }

      // compute the remainder term for taylor series
      val taylorRemainder = getTaylorRemainder(bodyDelta, Seq(inputValMap))
      reporter.debug(s"The taylor remainder value is $taylorRemainder")

      // add the remainder to the error
      // TODO: shouldn't this fail, if the remainder cannot be computed?
      val error = tmpErr + taylorRemainder.getOrElse(Rational.zero)

      reporter.debug("absError: " + error.toString + ", time: " +
        (System.currentTimeMillis - startTime))
      (fnc.id -> (error, interval))
    }).toMap

    timer.stop
    ctx.reporter.info(s"Finished $name")

    (ctx.copy(resultAbsoluteErrors = res.mapValues(_._1),
      resultRealRanges = res.mapValues(_._2)), prg)
  }

}
