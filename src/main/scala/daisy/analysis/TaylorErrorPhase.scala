package daisy.analysis

import daisy.lang.Trees._
import daisy._
import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.TreeOps._
import daisy.lang.Types.RealType
import daisy.utils.FinitePrecision._
import daisy.utils._
import Interval._
import daisy.analysis.RelativeErrorPhase.{denormals, reporter}
import daisy.lang.NumAnnotation

import scala.collection.immutable.Map
import scala.collection.parallel.ParSeq

/**
  * Computes absolute errors same way as in RangeErrorPhase,
  * but evaluates taylor simplifications instead
  */
object TaylorErrorPhase extends DaisyPhase with Taylor with ErrorFunctions {
  override var reporter: Reporter = null
  override val name: String = "taylor abs error phase"
  override val description: String = "computes abs error using taylor simplifications"

  val zeroNum = Rational.zero
  override var denormals: Boolean = false

  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ChoiceOptionDef("rangeMethod", "Method to use for range analysis",
      Set("affine", "interval", "smt", "subdiv"), "interval"),

    FlagOptionDef("noRoundoff", "do not track roundoff errors"),
    FlagOptionDef("denormals","Include parameter for denormals in the FP abstraction")
  )

  // Var for error functions
  // trackRoundoffErrs and uniformPrecision assigned below in run
  attachToTree = true

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.taylor.start
    // default range method: intervals
    var rangeMethod = "interval"
    var errorMethod = "affine"   // only one supported so far

    // setting trait variables
    trackRoundoffErrs = true

    // process relevant options
    for (opt <- ctx.options) opt match {
      case ChoiceOption("rangeMethod", s) => s match {
        case "interval" | "affine" | "smt" | "subdiv" =>
          rangeMethod = s
          reporter.info(s"using $s")
        case _ =>
          reporter.warning(s"Unknown range method: $s, choosing default (interval)!")
      }
      case FlagOption("noRoundoff") => trackRoundoffErrs = false

      case FlagOption("denormals") =>
        reporter.info("Include parameter for denormals")
        denormals = true
      case _ =>
    }

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)
    for (fnc <- prg.defs) if (fnc.precondition.isDefined && fnc.body.isDefined && fncsToConsider.contains(fnc.id.toString)) {
      reporter.info("analyzing fnc: " + fnc.id)
      val startTime = System.currentTimeMillis

      val bodyReal = fnc.body.get
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
      val inputValMap: Map[Identifier, Interval] = ctx.inputRanges(fnc.id) ++ deltaIntervalMap


      val (tmpErr, interval) = (rangeMethod, errorMethod) match {
        case ("interval", "affine") =>
          // evaluate each partial derivative
          val err = taylor.map(x =>{
            // replace all deltas with zeros to get f~(x,0)
            val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
//            reporter.debug(s"expr before " + x._1)
//            reporter.debug(s"expr after $tmp")
            // evaluate the term and take the upper bound of resulting interval
            maxAbs(Evaluators.evalInterval(tmp, inputValMap))})
          reporter.debug(s"errors $err")
          var finalErr = Rational.zero
          // sum the values for each partial derivative term
          err.foreach(x => {finalErr = finalErr.+(x)})

          (finalErr, Evaluators.evalInterval(fnc.body.get, inputValMap))

        case ("affine", "affine") =>
          val err = taylor.map(x =>{
            // replace all deltas with zeros to get f~(x,0)
            val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
            reporter.debug(s"expr before " + x._1)
            reporter.debug(s"expr after $tmp")
            // evaluate the term and take the upper bound of resulting interval
            maxAbs(Evaluators.evalAffine(
              tmp, inputValMap.map(y => (y._1 -> AffineForm(y._2)))).toInterval)})
          var finalErr = Rational.zero
          // sum the values for each partial derivative term
          err.foreach(x => {finalErr = finalErr.+(x)})

          (finalErr, Evaluators.evalAffine(
            fnc.body.get, inputValMap.map(x => (x._1 -> AffineForm(x._2)))).toInterval)

        case ("smt", "affine") =>
          val err = taylor.map(x =>{// replace all deltas with zeros to get f~(x,0)
            val tmp = easySimplify(Times(replaceDeltasWithZeros(x._1), Delta(x._2)))
              // evaluate the term and take the upper bound of resulting interval
              maxAbs(Evaluators.evalSMT(
                tmp, inputValMap.map({
                  case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)})
          var finalErr = Rational.zero
          // sum the values for each partial derivative term
          err.foreach(x => {finalErr = finalErr.+(x)})

          (finalErr, Evaluators.evalSMT(
            fnc.body.get, inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)

        case _ =>
          reporter.fatalError(s"Your combination of $rangeMethod and $errorMethod" +
            "for computing ranges and errors is not supported.")
      }

      // compute the remainder term for taylor series
      val taylorRemainder = getTaylorRemainder(bodyDelta, Seq(inputValMap))
      reporter.debug(s"The taylor remainder value is $taylorRemainder")
      // add the remainder to the error
      val error = tmpErr.+(taylorRemainder.getOrElse(Rational.zero))

      fnc.body match {
        case y @ Some(w) => w match {
            // FIXME cases when it's not NUmAnnotation? error is computed, but not shown in info phase
          case x: NumAnnotation => x.absError = error
            x.interval = interval
          case _ => reporter.debug("") // mock up to make match exhaustive
        }
        case None => reporter.internalError(s"Function has no body")
      }
        reporter.debug("absError: " + error.toString + ", time: " +
        (System.currentTimeMillis - startTime))
    }

    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, prg)
  }

}
