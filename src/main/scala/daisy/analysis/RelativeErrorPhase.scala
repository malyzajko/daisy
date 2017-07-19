

package daisy
package analysis

//import java.io.FileWriter
//import java.io.BufferedWriter

import lang.Trees.{Expr, _}
import daisy.utils.Rational.{apply => _, _}
import lang.Identifiers.{Identifier, _}
import lang.Types.RealType
import utils.{SMTRange, _}
import Interval._
import daisy.lang.Constructors._
import daisy.solvers.{Solver, Z3Solver}
import utils.FinitePrecision._
import lang.TreeOps._
import smtlib.parser.Commands.{AttributeOption, SetOption}
import smtlib.parser.Terms.{Attribute, SKeyword, SNumeral, SSymbol}

import scala.collection.immutable.Map
import scala.collection.parallel.{ParSeq, ParSet}
import scala.util.control.Breaks._


/**
  * Compute relative errors directly, i.e. not through first computing
  * absolute errors.
  **
  *Uses the (1 + delta) abstraction for floating-point computations.
  **
  *
  *Prerequisites:
  *- SpecsProcessingPhase
 */
object RelativeErrorPhase extends DaisyPhase with Subdivision with ErrorFunctions{

  // default parameters for the complete run
  var divLimit = 3
  var divRemainder = 0
  var rangeMethod = "interval"
  var subdiv = "simple"
  var approach = "taylor"
  var uniformPrecision: Precision = Float64

  override val name = "relative error phase"
  override val description = "computes relative errors directly"
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ChoiceOptionDef("rel-rangeMethod", "Method to use for range analysis",
    Set("affine", "interval", "smtreuse", "smtredo", "smtcomplete"), "interval"),
  ParamOptionDef("divLimit", "Max amount of interval divisions", divLimit.toString),
  ParamOptionDef("divRemainder", "Max amount of interval divisions for remainder term", divRemainder.toString),
  ParamOptionDef("totalOpt", "Max total amount of analysis runs", totalOpt.toString),
    ChoiceOptionDef("subdiv", "Method to subdivide intervals", Set("simple", "model"), "simple"),
    ChoiceOptionDef("approach", "Approach for expressions", Set("taylor", "naive"), "taylor"),
    // fixme change name to not overlap with RangeErrorPhase or put into Main
    FlagOptionDef("noRoundoff", "No initial roundoff errors"),
    FlagOptionDef("denormals","Include parameter for denormals in the FP abstraction"))
  val deltaName = "delta"
  val epsilonName = "eps"

  var reporter: Reporter = null
  override var denormals: Boolean = false

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.relError.start

    val fncsToConsider: Seq[String] = functionsToConsider(ctx, prg)
    for (fnc <- prg.defs) if (fnc.precondition.isDefined && fnc.body.isDefined && fncsToConsider.contains(fnc.id.toString)) {

      // process relevant options
      for (opt <- ctx.options) opt match {
        case ParamOption("divLimit", value) => divLimit = value.toInt
        case ParamOption("divRemainder", value) => divRemainder = value.toInt
        case ParamOption("totalOpt", value) => totalOpt = value.toInt
        case ChoiceOption("rel-rangeMethod", s) => s match {
          case "interval" | "affine" | "smtreuse" | "smtredo" | "smtcomplete" =>
            rangeMethod = s
            reporter.info(s"using $s")
          case _ =>
            reporter.warning(s"Unknown range method: $s, choosing default (interval)!")
        }
        case ChoiceOption("subdiv", s) => s match {
          case "simple" | "model" =>
            subdiv = s
            reporter.info(s"using $s subdivision method")
          case _ =>
            reporter.warning(s"Unknown subdivision method: $s, choosing default (simple)!")
        }
        case ChoiceOption("approach", s) => s match {
          case "taylor" | "naive" =>
            approach = s
            reporter.info(s"using $s approach")
          case _ =>
            reporter.warning(s"Unknown approach: $s, choosing default (taylor)!")
        }
        case FlagOption("noRoundoff") =>
          reporter.info("No roundoff")
          trackRoundoffErrs = false
        case FlagOption("denormals") =>
          reporter.info("Include parameter for denormals")
          denormals = true
        case _ =>
      }

      val bodyReal = fnc.body.get
      val deltaVarMap = mapDeltasToVars(bodyReal)
      val epsVarMap = mapEpsilonsToVars(bodyReal)
      val bodyDeltaAbs = if (denormals)
          deltaAbstract(bodyReal, deltaVarMap, epsVarMap)
        else
             deltaAbstract(bodyReal, deltaVarMap, Map.empty)
//      reporter.warning(s"bodyDelta $bodyDeltaAbs")
      // Step 1: disregard initial errors for now
      // (f(x) - fl(x))/ f(x)

      val relErrorExpr = Division(Minus(bodyReal, bodyDeltaAbs), bodyReal)

      reporter.info("\n" + fnc.id + ", bodyReal: " + bodyReal)

      val startTime = System.currentTimeMillis
      // adding constraints on deltas here
      val deltas = deltasOf(relErrorExpr)
      var deltaIntervalMap: Map[Identifier, Interval] = Map.empty
      for (delta <- deltas){
          deltaIntervalMap = deltaIntervalMap + (delta.id -> delta.interval)
      }
      val eps = epsilonsOf(relErrorExpr)
      for (e <- eps){
        deltaIntervalMap = deltaIntervalMap + (e.id -> e.interval)
      }
      val inputValMap: Map[Identifier, Interval] = ctx.inputRanges(fnc.id) ++ deltaIntervalMap
      // no initial errors
      val allIDs = fnc.params.map(_.id)
      val inputErrorMap: Map[Identifier, Rational] = allIDs.map( id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))).toMap
      try {
        reporter.info("Evaluating " + fnc.id + "...")
        val (relError, tmpList) = approach match {
          case "taylor" => getRelErrorTaylorApprox(relErrorExpr, inputValMap, bodyReal, ctx)
          case "naive" => getRelErrorNaive(relErrorExpr, inputValMap, bodyReal, ctx)
        }
        reporter.warning("Failed on " + tmpList.distinct.size + " sub-domain(s)")
        val list = mergeIntervals(tmpList, inputValMap)
        // if returned None or too many subintervals where failed to compute relError,
        // say it is not possible to compute
        if (relError.isDefined && (list.size < 30)) {
            val time = System.currentTimeMillis
            reporter.info("relError: " + relError.get.toString + ", time: " + (time - startTime))
            if (list.nonEmpty) {
              reporter.info(s"On several sub-intervals relative error cannot be computed.")
              reporter.info("Computing absolute error on these sub-intervals.")
              for (mapEntry <- list) {
                  // here we compute the abs error for intervals where rel error is not possible
                  val absError = getAbsError(bodyReal, mapEntry, inputErrorMap, uniformPrecision)
                  reporter.info(s"For intervals $mapEntry, absError: $absError, time: " +
                      (System.currentTimeMillis - time))
              }
            }
        } else {
          reporter.info("Not possible to get relative error, compute the absolute instead, time:" +
            (System.currentTimeMillis - startTime))
          val time = System.currentTimeMillis
          // fixme for JetEngine DivByZeroException is thrown
          val absError = getAbsError(bodyReal, inputValMap, inputErrorMap, uniformPrecision)
          reporter.info(s"absError: $absError, time: " +
              (System.currentTimeMillis - time))
        }
      }
      catch {
        case e: Throwable => {
          reporter.info("Something went wrong while computing the relative error.")
          reporter.info(e.printStackTrace())}
      }

    }
    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, prg)
  }

  /**
    * Evaluates the relative error for the taylor approximation for relErrorExpr on the set of subintervals
    * @param relErrorExpr - expression to evaluate, i.e. |f(x) - f~(x)|/f(x)
    * @param inputValMap - input ranges for variables plus deltas
    * @return
    */
  private def getRelErrorTaylorApprox(relErrorExpr: Expr,
                                      inputValMap: Map[Identifier, Interval],
                                      bodyReal: Expr,
                                      ctx: Context ): (Option[Rational], Seq[Map[Identifier, Interval]]) = {
    var listFailInterval: Seq[Map[Identifier, Interval]] = Seq.empty
    var finalErr: Option[Rational] = None
    // get intervals subdivision for the complete divLimit
    val newSet = getSubintervals(inputValMap, bodyReal, ctx, subdiv, divLimit)

    reporter.debug(s"EXPRESSION is $relErrorExpr")
    reporter.debug("The set we got")
    // output subintervals without deltas
    for (entry <- newSet){
      reporter.debug("============================================")
      for (mapEntry <- entry if !(mapEntry._1.isDeltaId|| mapEntry._1.isEpsilonId))
        reporter.debug(mapEntry._1 + " -> " + mapEntry._2)
    }
    reporter.debug(s"We need to evaluate expression on "+newSet.size+" intervals")
    reporter.debug("there are " + deltasOf(relErrorExpr).size+ " deltas")
    val taylorFirst = getDerivative(relErrorExpr)
//    reporter.warning(s"the taylor expression we got is ")
//    taylorFirst.foreach(x=>{reporter.debug(s"term is $x")})
    reporter.info("Computing the error ...")

    reporter.info(s"subdiv for remainder $divRemainder")
    // separate timer for remainder
    val remainderTime = System.currentTimeMillis
    val remainderMap = getEqualSubintervals(inputValMap, divLimit, divRemainder)
    val taylorRemainder = getTaylorRemainder(relErrorExpr, remainderMap)
    reporter.info(s"The taylor remainder value is $taylorRemainder, time: " + (System.currentTimeMillis - remainderTime))
    if (taylorRemainder.isDefined) {
      val errForSum = taylorFirst.map(x => {
        val (expr, wrt) = x
        val tmpExpr = moreSimplify(
          Times(replaceDeltasWithZeros(expr), Delta(wrt)))
        reporter.debug(s"Evaluate the term $tmpExpr")
        // do not call evaluation function on all subintervals
        // if simplified expression is delta or RealLiteral
        val tmpForMax = tmpExpr match {
          case x @ Delta(id) => List(evaluateOpt(tmpExpr, inputValMap, rangeMethod))
          case x @ Epsilon(id) => List(evaluateOpt(tmpExpr, inputValMap, rangeMethod))
          case x @ Variable(id) => List(evaluateOpt(tmpExpr, inputValMap, rangeMethod))
          case x @ RealLiteral(r) => List(evaluateOpt(tmpExpr, inputValMap, rangeMethod))
          case _ => newSet.map(interval => {
            val tmp = evaluateOpt(tmpExpr, interval, rangeMethod)
            reporter.debug("err on " + removeDeltasFromMap(interval) + s" is $tmp")
            if (tmp.isEmpty)
              if (!listFailInterval.contains(interval) && !listFailed.contains(interval)) listFailInterval = listFailInterval :+ interval
            tmp
          })
        }
        tmpForMax.max(optionAbsOrdering)
      })
      reporter.debug(s"we need to sum $errForSum")

      errForSum.foreach(x => {
        if (finalErr.isDefined)
          finalErr = Some(finalErr.get.+(x.getOrElse(Rational.zero)))
        else
          finalErr = x
      })
      if (finalErr.isDefined)
        finalErr = Some(finalErr.get.+(taylorRemainder.getOrElse(Rational.zero)))
    }

    listFailInterval = (listFailInterval ++ listFailed).toSet.toList
    reporter.debug("print what is ACTUALLY in ListFailed " + listFailed.map(removeDeltasFromMap).map(_.keySet.map(_.globalId)))
    (finalErr, listFailInterval)
  }

  /**
    * Evaluates the relative error for the original relErrorExpr on the set of subintervals
    * @param relErrorExpr - expression to evaluate, i.e. |f(x) - f~(x)|/f(x)
    * @param inputValMap - input ranges for variables plus deltas
    * @return
    */
  private def getRelErrorNaive(relErrorExpr: Expr,
                               inputValMap: Map[Identifier, Interval],
                               bodyReal: Expr,
                               ctx: Context): (Option[Rational], Seq[Map[Identifier, Interval]]) = {
    var listFailInterval: Seq[Map[Identifier, Interval]] = Seq.empty

    // get intervals subdivision for the complete divLimit
    val newSet = getSubintervals(inputValMap, bodyReal, ctx, subdiv, divLimit).par

    reporter.debug("The set we got")
    // output subintervals without deltas
    for (entry <- newSet){
      for (mapEntry <- entry if !(mapEntry._1.isDeltaId|| mapEntry._1.isEpsilonId))
        reporter.debug(mapEntry._1 + " -> " + mapEntry._2)
    }

    reporter.info("Computing the error ...")
    val errors = newSet.map(x => {
      val tmp = evaluateOpt(relErrorExpr, x, rangeMethod)
      if (tmp.isEmpty) listFailInterval = listFailInterval :+ x
      tmp
    })
    reporter.debug(errors)
    (errors.max, listFailInterval)
  }

  private def evaluateOpt(relErrorExpr: Expr,
                          inputValMap: Map[Identifier, Interval],
                          rangeMethod: String):Option[Rational] = {
    try {
      rangeMethod match {
        case ("interval") => Some(maxAbs(Evaluators.evalInterval(relErrorExpr, inputValMap)))
        case ("affine") => Some(maxAbs(Evaluators.evalAffine(relErrorExpr,
          inputValMap.map(x => (x._1 -> AffineForm(x._2)))).toInterval))
        case ("smtreuse") => Some(maxAbs(evaluateSMTReuse(relErrorExpr,
          inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval))
        case ("smtredo") => Some(maxAbs(Evaluators.evalSMT(relErrorExpr,
          inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval))
        case("smtcomplete") => Some(maxAbs(evaluateSMTComplete(relErrorExpr, inputValMap).toInterval))
        //case _ => reporter.error("Something went wrong. Unknown range method")
      }
    }
    catch{
      case z0: DivisionByZeroException => None
    }
  }

  private def getAbsError(bodyReal: Expr, inputValMap: Map[Identifier, Interval],
                          inputErrorMap: Map[Identifier, Rational],
                          uniformPrecision: Precision): Rational ={
    rangeMethod match {
      case "interval" =>
        errorIntervalAffine(bodyReal, inputValMap, inputErrorMap, uniformPrecision)

      case "affine" =>
        errorAffineAffine(bodyReal, inputValMap, inputErrorMap, uniformPrecision)

      case "smtreuse" | "smtredo" | "smtcomplete" =>
        errorSMTAffine(bodyReal, inputValMap, inputErrorMap, uniformPrecision)

      case _ =>
        reporter.fatalError(s"Range method $rangeMethod is not supported.")
    }
  }

  val precisionLower = Rational.fromReal(0.01)
  val precisionDefault = Rational.fromReal(0.000000000000000001)
  val loopDefault = 100
  val loopLower = 75


  def evaluateSMTComplete(expr:Expr,
                          _intMap: Map[Identifier, Interval]): SMTRange = {
    val intMap = _intMap
    val interval = Evaluators.evalInterval(expr, intMap)
    var constrs: Set[Expr] = Set.empty
    val deltas = deltasOf(expr)
    val eps = epsilonsOf(expr)
    val vars = variablesOf(expr)
    intMap.foreach(x => {
      val (id, interval) = x
      if (deltas.contains(Delta(id)) || vars.contains(id) || eps.contains(Epsilon(id))) {
        constrs = constrs ++ SMTRange.toConstraints(Variable(id), interval)
      }
    })
//    reporter.debug(s"preconditions $constrs")
    val tmp = SMTRange(expr, interval, constrs)
//    reporter.debug(s"AFTER $tmp")
    tmp
  }

  /**
    *This version records the already seen intervals (from identical, repeated subtrees)
    *and does not recompute the range.
  */
  def evaluateSMTReuse(expr: Expr, _intMap: collection.immutable.Map[Identifier, SMTRange] = Map.empty): SMTRange = {

    var intMap = _intMap
    // TODO check whether the copy is the best solution here
    val exprCopy = expr.deepCopy
    var smtRangeMap: collection.immutable.Map[Expr, SMTRange] = Map.empty

    def evalSMT(e: Expr): SMTRange = e match {

      case x @ Delta(id) =>
        if (x.hasInterval) {
          SMTRange(x.toVariable, x.interval)
        } else {
          val smtRange = intMap(id)
          x.interval = smtRange.toInterval
          smtRange
        }

      case x @ Epsilon(id) =>
        if (x.hasInterval) {
          SMTRange(x.toVariable, x.interval)
        } else {
          val smtRange = intMap(id)
          x.interval = smtRange.toInterval
          smtRange
        }

      case x @ Variable(id) =>
        if (x.hasInterval) {
            SMTRange(x, x.interval)
        } else {
          val smtRange = intMap(id)
          x.interval = smtRange.toInterval
          smtRange
        }

      case x @ RealLiteral(r) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = SMTRange(r)
          x.interval = smtRange.toInterval
          smtRangeMap = smtRangeMap + (x -> smtRange)
          smtRange
        }

      case x @ Plus(lhs, rhs) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = evalSMT(lhs).+(evalSMT(rhs), precisionDefault, loopLower)
          x.interval = smtRange.toInterval
          smtRangeMap = smtRangeMap + (x -> smtRange)
          smtRange
        }

      case x @ Minus(lhs, rhs) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = evalSMT(lhs).-(evalSMT(rhs), precisionDefault, loopLower)
          x.interval = smtRange.toInterval
          smtRangeMap = smtRangeMap + (x -> smtRange)
          smtRange
        }

      case x @ Times(lhs, rhs) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = evalSMT(lhs).*(evalSMT(rhs), precisionDefault, loopLower)
          x.interval = smtRange.toInterval
          smtRangeMap = smtRangeMap + (x -> smtRange)
          smtRange
        }

      case x @ Division(lhs, rhs) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = evalSMT(lhs)./(evalSMT(rhs), precisionDefault, loopLower)
          smtRangeMap = smtRangeMap + (x -> smtRange)
          x.interval = smtRange.toInterval
          smtRange
        }

      case x @ Pow(lhs, rhs) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = evalSMT(lhs).^(evalSMT(rhs), precisionDefault, loopLower)
          smtRangeMap = smtRangeMap + (x -> smtRange)
          x.interval = smtRange.toInterval
          smtRange
        }

      case x @ UMinus(t) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = - evalSMT(t)
          x.interval = smtRange.toInterval
          smtRangeMap = smtRangeMap + (x -> smtRange)
          smtRange
        }

      case x @ Sqrt(t) =>
        if (x.hasInterval && smtRangeMap.contains(x)) {
          smtRangeMap(x)
        } else {
          val smtRange = evalSMT(t).squareRoot(precisionDefault, loopDefault)
          x.interval = smtRange.toInterval
          smtRangeMap = smtRangeMap + (x -> smtRange)
          smtRange
        }

      case Let(id, value, body) =>
        if (intMap.contains(id)) {
          intMap(id)
        } else {
          val smtRange = evalSMT(value)
          intMap += (id -> smtRange)
          evalSMT(body)
        }

      case _ =>
        throw new IllegalArgumentException("Unknown expression. Evaluation failed")
    }
    evalSMT(exprCopy)
  }

  def compareMaps(first: Map[Identifier, Interval], second: Map[Identifier, Interval]): Boolean = {
    // if less than yes

    def compareIntervals(x: Interval, y: Interval): Int = {
      // [0; 1] and [1; 2]
      if (x.xlo < y.xlo) -1
        // [0; 1] and [-1; 0]
      else if (x.xlo > y.xlo) 1
        // [0; 1] and [0; 2]
        else if (x.xhi < y.xhi) -1
          // [0; 1] and [0; 0.5]
          else if (x.xhi > y.xhi) 1
          // equal
          else 0

    }

    var tmp = 0
    val iterator = first.iterator

    while (tmp == 0 && iterator.hasNext){
      val (id, interval) = iterator.next()
      if (!(id.isDeltaId|| id.isEpsilonId)) {
        tmp = compareIntervals(interval, second(id))
      }
    }
    tmp <= 0
  }


  private def mergeIntervals(listFailIntervals: Seq[Map[Identifier, Interval]],
                             inputMap: Map[Identifier, Interval]):
                              Seq[Map[Identifier, Interval]] = {
    if (listFailIntervals.nonEmpty) {
      // sort the maps
      val tmpList = listFailIntervals.map(removeDeltasFromMap).sortWith(compareMaps)
      reporter.debug("===== sorted maps ======")
      tmpList.foreach(x => {reporter.debug(s"map: $x")})

      // merge all the possible maps in the list
      var ready = false
      var tmpMerged = tmpList
      while (!ready){
        // merge maps
        val tmpSrc = mergeSortedMaps(tmpMerged)
        // check if maps have changed after merging
        ready = tmpMerged.equals(tmpSrc)
        tmpMerged = tmpSrc
      }
      reporter.debug("===== merged ======")
      tmpMerged.foreach(x => {reporter.debug(s"map: $x")})
      tmpMerged
    }
    else Seq()
  }

  def mergeSortedMaps(in: Seq[Map[Identifier,Interval]]): Seq[Map[Identifier,Interval]] = in match {
    case x:: y::tail =>
      val merged = mergeMaps(x, y)
      if (merged.size > 1)
        x +: mergeSortedMaps(y +: tail)
      else
        merged ++ mergeSortedMaps(tail)
    case x::Nil => Seq(x)
    case Nil => Seq()
  }

  def mergeMaps(x: Map[Identifier,Interval], y: Map[Identifier,Interval]):Seq[Map[Identifier,Interval]] = {
    var abort: Boolean = false
    var res: Map[Identifier, Interval] = Map.empty
    breakable(
    for((id, int) <- x){
      (int, y(id)) match {
        case (a, b) if a.equals(b) =>
          res = res + (id -> a)
        case (a, b) if a.xhi == b.xlo =>
          res = res + (id -> Interval(a.xlo, b.xhi))
//          // TODO does it over-approximate too much?
//        case (a, b) if (a.xlo >= b.xlo) && (a.xhi <= b.xhi) =>
//          // a is contained in b
//          res = res + (id -> b)
//        case (a, b) if (b.xlo >= a.xlo) && (b.xhi <= a.xhi) =>
//          // b is contained in a
//          res = res + (id -> a)
        case _ =>
          abort = true
          break
      }
    })
    if (abort)
    // if at least one element of the map cannot be merged,
    // return original maps
      Seq(x, y)
    else
      Seq(res)
  }

}

