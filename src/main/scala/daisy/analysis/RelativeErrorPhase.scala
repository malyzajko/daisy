// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._

import lang.Identifiers.Identifier
import lang.TreeOps._

import tools.{SMTRange, RangeEvaluators, Interval, Rational, AffineForm, DivisionByZeroException}
import tools.FinitePrecision._
import Interval._

import scala.collection.immutable.Map
import scala.util.control.Breaks._
import scala.collection.parallel.CollectionConverters._

/**
 * Compute relative errors directly, i.e. not through first computing
 * absolute errors.
 *
 * Uses the (1 + delta) abstraction for floating-point computations.
 *
 *
 * Prerequisites:
 * - SpecsProcessingPhase
 */
object RelativeErrorPhase extends DaisyPhase with tools.Taylor with tools.Subdivision
  with tools.RoundoffEvaluators with RangeEvaluators {

  override val name = "Relative error"
  override val description = "Computes relative errors directly."
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "rel-rangeMethod",
      Set("affine", "interval", "smtreuse", "smtredo", "smtcomplete"),
      "interval",
      "Method to use for range analysis"),
    NumOption(
      "rel-divLimit",
      3,
      "Max amount of interval divisions"),
    NumOption(
      "rel-divRemainder",
      0,
      "Max amount of interval divisions for remainder term"),
    StringChoiceOption(
      "approach",
      Set("taylor", "naive"),
      "taylor",
      "Approach for expressions")
  )

  override implicit val debugSection = DebugSectionAnalysis

  var divLimit: Int = 0
  var divRemainder: Int = 0
  var rangeMethod: String = null
  var approach: String = null
  var uniformPrecision: Precision = null

  var ctx: Context = null

  override def runPhase(_ctx: Context, prg: Program): (Context, Program) = {
    ctx = _ctx
    divLimit = ctx.option[Long]("rel-divLimit").toInt
    divRemainder = ctx.option[Long]("rel-divRemainder").toInt
    rangeMethod = ctx.option[String]("rel-rangeMethod")
    approach = ctx.option[String]("approach")
    uniformPrecision = ctx.option[Precision]("precision")

    val res: Map[Identifier, Option[Rational]] = analyzeConsideredFunctions(ctx, prg){ fnc =>

      ctx.reporter.info("Evaluating " + fnc.id + "...")
      val bodyReal = fnc.body.get
      val bodyDeltaAbs = epsilonDeltaAbstract(bodyReal, ctx.hasFlag("denormals"))._1
      // cfg.reporter.warning(s"bodyDelta $bodyDeltaAbs")
      // Step 1: disregard initial errors for now
      // (f(x) - fl(x))/ f(x)

      val relErrorExpr = Division(Minus(bodyReal, bodyDeltaAbs), bodyReal)

      ctx.reporter.info("\n" + fnc.id + ", bodyReal: " + bodyReal)

      val startTime = System.currentTimeMillis

      // adding constraints on deltas here
      val epsilons = epsilonsOf(relErrorExpr)
      var epsilonIntervalMap: Map[Identifier, Interval] =
        epsilons.map(eps => (eps.id -> epsilonIntervalFloat64)).toMap

      val deltas = deltasOf(relErrorExpr)
      epsilonIntervalMap = epsilonIntervalMap ++ deltas.map(e => (e.id -> deltaIntervalFloat64))

      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id) ++ epsilonIntervalMap

      // no initial errors
      val allIDs = fnc.params.map(_.id)
      val inputErrorMap: Map[Identifier, Rational] =
        allIDs.map(id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))).toMap

      try {
        val (relError, tmpList) = approach match {
          case "taylor" => getRelErrorTaylorApprox(relErrorExpr, inputValMap, fnc.precondition.get, bodyReal)
          case "naive" => getRelErrorNaive(relErrorExpr, inputValMap, fnc.precondition.get, bodyReal)
        }
        if (tmpList.distinct.size > 0) {
          ctx.reporter.warning("Failed on " + tmpList.distinct.size + " sub-domain(s)")
        }

        val list = mergeIntervals(tmpList, inputValMap)

        // if returned None or too many subintervals where failed to compute relError,
        // say it is not possible to compute
        if (relError.isDefined && (list.size < 30)) {
          val time = System.currentTimeMillis
          ctx.reporter.info("relError: " + relError.get.toString + ", time: " + (time - startTime))
          if (list.nonEmpty) {
            ctx.reporter.info("On several sub-intervals relative error cannot be computed.")
            ctx.reporter.info("Computing absolute error on these sub-intervals.")
            for (mapEntry <- list) {
              // here we compute the abs error for intervals where rel error is not possible
              val absError = getAbsError(bodyReal, mapEntry, inputErrorMap, fnc.precondition.get, uniformPrecision)
              ctx.reporter.info(s"For intervals $mapEntry, absError: $absError, time: " +
                (System.currentTimeMillis - time))
            }
          }
          relError
        } else {
          ctx.reporter.info("Not possible to get relative error, compute the absolute instead, time:" +
            (System.currentTimeMillis - startTime))
          val time = System.currentTimeMillis
          // fixme for JetEngine DivByZeroException is thrown
          val absError = getAbsError(bodyReal, inputValMap, inputErrorMap, fnc.precondition.get, uniformPrecision)
          ctx.reporter.info(s"absError: $absError, time: " +
            (System.currentTimeMillis - time))
          None
        }
      }
      catch {
        case e: Throwable => {
          ctx.reporter.info("Something went wrong while computing the relative error.")
          ctx.reporter.info(e.printStackTrace())}
          None
      }

    }
    (ctx.copy(resultRelativeErrors = res), prg)
  }

  /**
   * Evaluates the relative error for the taylor approximation for relErrorExpr on the set of subintervals
   * @param relErrorExpr - expression to evaluate, i.e. |f(x) - f~(x)|/f(x)
   * @param inputValMap - input ranges for variables plus deltas
   * @return
   */
  private def getRelErrorTaylorApprox(relErrorExpr: Expr, inputValMap: Map[Identifier, Interval],
    precondition: Expr, bodyReal: Expr): (Option[Rational], Seq[Map[Identifier, Interval]]) = {

    var listFailInterval: Seq[Map[Identifier, Interval]] = Seq.empty
    var finalErr: Option[Rational] = None

    // get intervals subdivision for the complete divLimit
    // val newSet = getSubintervals(inputValMap, bodyReal, ctx, subdiv, divLimit)
    val newSet = getEqualSubintervals(inputValMap, divLimit)

    ctx.reporter.ifDebug { debug =>
      ctx.reporter.debug(s"EXPRESSION is $relErrorExpr")
      ctx.reporter.debug("The set we got")

      // output subintervals without deltas
      for (entry <- newSet){
        ctx.reporter.debug("============================================")
        for (mapEntry <- entry if !(mapEntry._1.isDeltaId|| mapEntry._1.isEpsilonId)) {
          ctx.reporter.debug(mapEntry._1 + " -> " + mapEntry._2)
        }
      }
      ctx.reporter.debug(s"We need to evaluate expression on " + newSet.size + " intervals")
      ctx.reporter.debug("there are " + epsilonsOf(relErrorExpr).size + " epsilons")
    }

    val taylorFirst = getDerivative(relErrorExpr)

    // ctx.reporter.warning(s"the taylor expression we got is ")
    // taylorFirst.foreach(x=>{ctx.reporter.debug(s"term is $x")})
    ctx.reporter.info("Computing the error ...")

    ctx.reporter.info(s"subdiv for remainder $divRemainder")
    // separate timer for remainder
    val remainderTime = System.currentTimeMillis
    val remainderMap = getEqualSubintervals(inputValMap, divLimit, divRemainder)
    val taylorRemainder = getTaylorRemainder(relErrorExpr, remainderMap)
    ctx.reporter.info(s"The taylor remainder value is $taylorRemainder, time: " +
      (System.currentTimeMillis - remainderTime))
    if (taylorRemainder.isDefined) {
      val errForSum = taylorFirst.map(x => {
        val (expr, wrt) = x
        val tmpExpr = moreSimplify(Times(replaceDeltasWithZeros(expr), Epsilon(wrt)))
        ctx.reporter.debug(s"Evaluate the term $tmpExpr")
        // do not call evaluation function on all subintervals
        // if simplified expression is delta or RealLiteral
        val tmpForMax = tmpExpr match {
          case x @ Variable(id) => List(evaluateOpt(tmpExpr, inputValMap, precondition, rangeMethod))
          case x @ RealLiteral(r) => List(evaluateOpt(tmpExpr, inputValMap, precondition, rangeMethod))
          case _ => newSet.map(interval => {
            val tmp = evaluateOpt(tmpExpr, interval, precondition, rangeMethod)
            ctx.reporter.debug("err on " + removeDeltasFromMap(interval) + s" is $tmp")
            if (tmp.isEmpty && !listFailInterval.contains(interval) && !listFailed.contains(interval)) {
              listFailInterval = listFailInterval :+ interval
            }
            tmp
          })
        }
        tmpForMax.max(optionAbsOrdering)
      })
      ctx.reporter.debug(s"we need to sum $errForSum")

      errForSum.foreach(x => {
        if (finalErr.isDefined) {
          finalErr = Some(finalErr.get + x.getOrElse(Rational.zero))
        } else {
          finalErr = x
        }
      })
      if (finalErr.isDefined) {
        // TODO: why is this getOrElse?
        finalErr = Some(finalErr.get + taylorRemainder.getOrElse(Rational.zero))
      }
    }

    listFailInterval = (listFailInterval ++ listFailed).toSet.toList
    ctx.reporter.debug("print what is ACTUALLY in ListFailed " +
      listFailed.map(removeDeltasFromMap).map(_.keySet.map(_.globalId)))
    (finalErr, listFailInterval)
  }

  /**
   * Evaluates the relative error for the original relErrorExpr on the set of subintervals
   * @param relErrorExpr - expression to evaluate, i.e. |f(x) - f~(x)|/f(x)
   * @param inputValMap - input ranges for variables plus deltas
   * @return
   */
  private def getRelErrorNaive(relErrorExpr: Expr, inputValMap: Map[Identifier, Interval],
    precondition: Expr, bodyReal: Expr): (Option[Rational], Seq[Map[Identifier, Interval]]) = {

    var listFailInterval: Seq[Map[Identifier, Interval]] = Seq.empty

    // get intervals subdivision for the complete divLimit
    val newSet = getEqualSubintervals(inputValMap, divLimit).par

    ctx.reporter.ifDebug { debug =>
      ctx.reporter.debug("The set we got")
      // output subintervals without deltas
      for (entry <- newSet) {
        for (mapEntry <- entry if !(mapEntry._1.isDeltaId|| mapEntry._1.isEpsilonId))
          ctx.reporter.debug(mapEntry._1 + " -> " + mapEntry._2)
      }
    }

    ctx.reporter.info("Computing the error ...")
    val errors = newSet.map(x => {
      val tmp = evaluateOpt(relErrorExpr, x, precondition, rangeMethod)
      if (tmp.isEmpty) listFailInterval = listFailInterval :+ x
      tmp
    })
    ctx.reporter.debug(errors)
    (errors.max, listFailInterval)
  }

  private def evaluateOpt(relErrorExpr: Expr, inputValMap: Map[Identifier, Interval],
    precondition: Expr, rangeMethod: String): Option[Rational] = {
    try {
      rangeMethod match {
        case ("interval") =>
          Some(maxAbs(evalRange[Interval](relErrorExpr, inputValMap, Interval.apply)._1))

        case ("affine") =>
          Some(maxAbs(evalRange[AffineForm](relErrorExpr,
            inputValMap.mapValues(AffineForm(_)).toMap, AffineForm.apply)._1.toInterval))

        case ("smtreuse") =>
          Some(maxAbs(evalRange[SMTRange](relErrorExpr,
            inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, precondition)) }),
            SMTRange.apply(_, precondition))._1.toInterval))

        case ("smtredo") =>
          Some(maxAbs(evaluateSMTRedo(relErrorExpr, precondition,
            inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int, precondition)) })).toInterval))

        case("smtcomplete") =>
          Some(maxAbs(evaluateSMTComplete(relErrorExpr, inputValMap).toInterval))

        // case _ => ctx.reporter.error("Something went wrong. Unknown range method")
      }
    }
    catch{
      case z0: DivisionByZeroException => None
    }
  }

  private def getAbsError(bodyReal: Expr, inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational], precondition: Expr, uniformPrecision: Precision): Rational = rangeMethod match {
    case "interval" =>
      uniformRoundoff_IA_AA(bodyReal, inputValMap, inputErrorMap, uniformPrecision,
        trackRoundoffErrors = true)._1

    case "affine" =>
      uniformRoundoff_AA_AA(bodyReal, inputValMap, inputErrorMap, uniformPrecision,
        trackRoundoffErrors = true)._1

    case "smtreuse" | "smtredo" | "smtcomplete" =>
      uniformRoundoff_SMT_AA(bodyReal, inputValMap, inputErrorMap, precondition, uniformPrecision,
        trackRoundoffErrors = true)._1

    case _ =>
      ctx.reporter.fatalError(s"Range method $rangeMethod is not supported.")
  }

  val precisionLower = Rational.fromReal(0.01)
  val precisionDefault = Rational.fromReal(0.000000000000000001)
  val loopDefault = 100
  val loopLower = 75


  def evaluateSMTComplete(expr: Expr, _intMap: Map[Identifier, Interval]): SMTRange = {

    val intMap = _intMap
    val interval = evalRange[Interval](expr, intMap, Interval.apply)._1
    var constrs: Set[Expr] = Set.empty
    val deltas = deltasOf(expr)
    val eps = epsilonsOf(expr)
    val vars = freeVariablesOf(expr)
    intMap.foreach(x => {
      val (id, interval) = x
      if (deltas.contains(Delta(id)) || vars.contains(id) || eps.contains(Epsilon(id))) {
        constrs = constrs ++ SMTRange.toConstraints(Variable(id), interval)
      }
    })
    SMTRange(expr, interval, constrs)
  }

  /**
   * This version does not record the already seen intervals (from identical, repeated subtrees)
   * and does recompute the range.
   */
  def evaluateSMTRedo(expr: Expr, precondition: Expr, _intMap: Map[Identifier, SMTRange] = Map.empty): SMTRange = {

    var valMap: Map[Identifier, SMTRange] = _intMap

    def eval(e: Expr): SMTRange = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case RealLiteral(r) => SMTRange(r, precondition)
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      // case Pow(x, n) => eval(x) ^ eval(n)
      case IntPow(x, n) => eval(x) ^ n
      case UMinus(x) => - eval(x)
      case Let(id, v, b) =>
        val temp = eval(v)
        valMap += (id -> temp)
        eval(b)
    }
    eval(expr)
  }

  def compareMaps(first: Map[Identifier, Interval], second: Map[Identifier, Interval]): Boolean = {
    // if less than yes

    def compareIntervals(x: Interval, y: Interval): Int = {
      // [0; 1] and [1; 2]
      if (x.xlo < y.xlo) {
        -1
      // [0; 1] and [-1; 0]
      } else if (x.xlo > y.xlo) {
        1
      // [0; 1] and [0; 2]
      } else if (x.xhi < y.xhi) {
        -1
      // [0; 1] and [0; 0.5]
      } else if (x.xhi > y.xhi) {
        1
      // equal
      } else {
        0
      }
    }

    var tmp = 0
    val iterator = first.iterator

    // TODO: this can be probably done nicer
    while (tmp == 0 && iterator.hasNext) {
      val (id, interval) = iterator.next()
      if (!(id.isDeltaId|| id.isEpsilonId)) {
        tmp = compareIntervals(interval, second(id))
      }
    }
    tmp <= 0
  }


  private def mergeIntervals(listFailIntervals: Seq[Map[Identifier, Interval]],
    inputMap: Map[Identifier, Interval]): Seq[Map[Identifier, Interval]] = {

    if (listFailIntervals.nonEmpty) {
      // sort the maps
      val tmpList = listFailIntervals.map(removeDeltasFromMap).sortWith(compareMaps)
      ctx.reporter.debug("===== sorted maps ======")
      tmpList.foreach(x => {ctx.reporter.debug(s"map: $x")})

      // merge all the possible maps in the list
      var ready = false
      var tmpMerged = tmpList
      while (!ready) {
        // merge maps
        val tmpSrc = mergeSortedMaps(tmpMerged)
        // check if maps have changed after merging
        ready = tmpMerged.equals(tmpSrc)
        tmpMerged = tmpSrc
      }
      ctx.reporter.debug("===== merged ======")
      tmpMerged.foreach(x => {ctx.reporter.debug(s"map: $x")})
      tmpMerged

    } else {
      Seq()
    }
  }

  def mergeSortedMaps(in: Seq[Map[Identifier,Interval]]): Seq[Map[Identifier,Interval]] = in match {
    case x :: y :: tail =>
      val merged = mergeMaps(x, y)
      if (merged.size > 1) {
        x +: mergeSortedMaps(y +: tail)
      } else {
        merged ++ mergeSortedMaps(tail)
      }
    case x :: Nil => Seq(x)
    case Nil => Seq()
  }

  def mergeMaps(x: Map[Identifier,Interval], y: Map[Identifier,Interval]): Seq[Map[Identifier,Interval]] = {

    var abort: Boolean = false
    var res: Map[Identifier, Interval] = Map.empty
    breakable(
    for((id, int) <- x){
      (int, y(id)) match {
        case (a, b) if a.equals(b) =>
          res = res + (id -> a)
        case (a, b) if a.xhi == b.xlo =>
          res = res + (id -> Interval(a.xlo, b.xhi))
        //  // TODO does it over-approximate too much?
        // case (a, b) if (a.xlo >= b.xlo) && (a.xhi <= b.xhi) =>
        //  // a is contained in b
        //  res = res + (id -> b)
        // case (a, b) if (b.xlo >= a.xlo) && (b.xhi <= a.xhi) =>
        //  // b is contained in a
        //  res = res + (id -> a)
        case _ =>
          abort = true
          break()
      }
    })
    if (abort) {
      // if at least one element of the map cannot be merged,
      // return original maps
      Seq(x, y)
    } else {
      Seq(res)
    }
  }

}
