package daisy
package opt

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.immutable.Seq
import scala.collection.mutable.{Set => MSet}
import util.Random
import lang.Trees._
import lang.Types._
import lang.Identifiers._
import tools.FinitePrecision._
import lang.TreeOps._
import tools.{MPFRAffineForm, Interval, Rational}
import Rational._
import lang.Extractors.ArithOperator

/**
 * This phase optimizes and determines a suitable mixed-precision type assignment.
 *
 * It only considers roundoff errors during the optimization, i.e. no additional
 * roundoff errors (for now). If we want to take into account initial errors too,
 * an additional analysis step is required before this phase (to determine how large
 * the roundoff errors can be), but we will leave this for later.
 *
 * The phase transforms the initial real-valued program into a finite-precision one
 * (by changing the types of the arithmetic nodes).
 * The final error computation (and certification at some point) will be done
 * by the AbsErrorPhase, which should follow this phase.
 *
 * Prerequisites:
 * - SpecsProcessingPhase
 * - RangePhase (assumes that real-valued ranges are attached to the tree)
 */
object MixedPrecisionOptimizationPhase extends DaisyPhase with CostFunctions
  with search.GeneticSearch[Map[Identifier, Precision]] with tools.RoundoffEvaluators {

  override val name = "Mixed-precision optimization"
  override val description = "determines a suitable mixed-precision type assignment"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption("mixed-opt-method", Set("random", "delta", "genetic"), "delta",
      "Algorithm to use for mixed-precision optimization"),
    StringChoiceOption("mixed-cost-function", Set("simple", "area", "benchmark", "benchmarkC", "default"), "default",
      "Cost function to use for mixed-precision optimization. 'default' chooses the function based on available precisions.")
  )

  override implicit val debugSection = DebugSectionOptimization

  type TypeConfig = Map[Identifier, Precision]

  var optimizationMethod: String = ""

  val timeOut: Int = 300

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val reporter = ctx.reporter

    val defaultPrecision = ctx.option[Precision]("precision")
    optimizationMethod = ctx.option[String]("mixed-opt-method")
    val uniformCostFunction = ctx.option[String]("mixed-cost-function") match {
      case "default" => None
      case "simple" => Some(simpleMixedPrecisionCost _)
      case "area" => Some(areaBasedCostFunction _)
      case "benchmark" => Some(benchmarkedMixedPrecisionCost _)
      case "benchmarkC" => Some(benchmarkedMixedPrecisionCostForC _)
    }

    val availablePrecisions = defaultPrecision match {
      case FixedPrecision(b) => // given precision specifies the upper bound
        (1 to b).map(FixedPrecision(_))
      case _ =>
        //Seq(Float32, Float64, Float128)
        Seq(Float64, Float128)
    }

    reporter.info(s"Optimisation method: $optimizationMethod")

    // store computed absolute errors for each function
    var resAbsoluteErrors: Map[Identifier, Rational] = Map()
    var precisionMap: Map[Identifier, Map[Identifier, Precision]] = Map()

    val newDefs: Seq[FunDef] = transformConsideredFunctions(ctx, prg) { fnc =>
      val rangeMap = ctx.intermediateRanges(fnc.id)
      reporter.info(s"Optimizing mixed-precision for ${fnc.id}...")
      // val (newDef, resultError, typeConfig) = tuneMixedPrecision(fnc, rangeMap, availablePrecisions,
      //   defaultPrecision, ctx.specResultErrorBounds.get(fnc.id), reporter, uniformCostFunction)

      var newDef: FunDef = null
      var typeConfig: Map[Identifier, Precision] = Map()
      var resultError: Rational = zero
      val future: Future[Unit] = Future {
        val (nD, rE, tC) = tuneMixedPrecision(fnc, rangeMap, availablePrecisions,
          defaultPrecision, ctx.specResultErrorBounds.get(fnc.id), reporter, uniformCostFunction)
        newDef = nD
        typeConfig = tC
        resultError = rE
      }

      try {
       Await.result(future, timeOut.second)
      }

      resAbsoluteErrors += (fnc.id -> resultError)
      precisionMap += (fnc.id -> typeConfig)

      newDef
    }

    // update rangeMap with trees with casts
    val newIntermediateRanges = newDefs.map(fnc => {
      var newRanges = Map[(Expr, Seq[Expr]), Interval]()
      val currRanges = ctx.intermediateRanges(fnc.id)

      postTraversal(e => {

        newRanges = newRanges + ((e, Seq()) -> currRanges((exprWithoutCasts(e), Seq())))

      })(fnc.body.get)

      (fnc.id -> newRanges)
    }).toMap

    val newResultPrecisions = newDefs.map(f => (f.id, f.returnType))
      .map({ case (id, FinitePrecisionType(prec)) => (id, prec) }).toMap

    val newInputErrors = newDefs.map(fnc => {
      val inputRanges = ctx.specInputRanges(fnc.id)
      val precisions = precisionMap(fnc.id)

      fnc.id -> fnc.params.map(p => p.id -> precisions(p.id).absRoundoff(inputRanges(p.id))).toMap
    }).toMap

    (ctx.copy(resultAbsoluteErrors = resAbsoluteErrors,
      intermediateRanges = newIntermediateRanges,
      specInputPrecisions = precisionMap,
      specInputErrors = newInputErrors,
      specResultPrecisions = newResultPrecisions), Program(prg.id, newDefs))
  }

  def tuneMixedPrecision(fnc: FunDef,
    rangeMap: Map[(Expr, Seq[Expr]), Interval],
    availablePrecisions: Seq[Precision],
    defaultPrecision: Precision,
    errorBound: Option[Rational],
    reporter: Reporter,
    uniformCostFunction: Option[(Expr, TypeConfig) => Rational] = None): (FunDef, Rational, Map[Identifier, Precision]) = {

    val body = fnc.body.get

    // TODO: check the errorbound elsewhere
    val (typeConfig, constPrec) = if (errorBound.nonEmpty) { // there is an output error to optimize for
      val targetError = errorBound.get

      // Step 1: find the smallest available precision which satisfies the error bound
      availablePrecisions.find(prec => {
        try {
          val rndoff = computeAbsError(body, allVariablesOf(body).map(id => (id -> prec)).toMap,
            prec, rangeMap)._1
          rndoff <= targetError
        } catch { // e.g. overflow when precision is not enough
          case _: Throwable => false
        }
      }) match {
        case None =>
          reporter.warning(s"Highest precision is *not enough* for ${fnc.id}; " +
            s"generating the highest-precision (${availablePrecisions.last}) version anyway.")
          (allVariablesOf(body).map(v => (v -> availablePrecisions.last)).toMap,
            availablePrecisions.last)

        case Some(prec) if prec == availablePrecisions.head =>
          reporter.info(s"No mixed-precision optimization needed for ${fnc.id} - " +
            s"lowest (${availablePrecisions.head}) is sufficient")
          (allVariablesOf(body).map(v => (v -> prec)).toMap, prec)

        case Some(lowestUniformPrec) =>
          val indexLowestUniformPrec = availablePrecisions.indexOf(lowestUniformPrec)

          // Step 2: optimize mixed-precision
          val consideredPrecisions = availablePrecisions.take(indexLowestUniformPrec + 1)
          reporter.debug(s"  precisions: $consideredPrecisions")

          val costFnc = uniformCostFunction.getOrElse(
            consideredPrecisions.last match {
              case Float128 | Float256 =>
                simpleMixedPrecisionCost _

              case FixedPrecision(_) =>
                areaBasedCostFunction _

              case _ =>
                benchmarkedMixedPrecisionCost _
            }
          )

          optimizationMethod match {
            case "delta" =>

              (deltaDebuggingSearch(body, targetError, fnc.params, costFnc,
                computeAbsError(body, _, _, rangeMap, approximate = true),
                consideredPrecisions, reporter),
                lowestUniformPrec)

            case "random" =>

              (randomSearch(body, targetError, costFnc,
                computeAbsError(body, _, _, rangeMap, approximate = true),
                consideredPrecisions, reporter, maxTries = 1000),
                lowestUniformPrec)

            case "genetic" =>

              (geneticSearch(body, targetError, benchmarkedMixedPrecisionCost,
                computeAbsError(body, _, _, rangeMap, approximate = true),
                consideredPrecisions, reporter),
                lowestUniformPrec)

          }
      }
    }
    else {
      // If no error is given in the postcondition, assign default precision
      reporter.warning(s"No target error bound for ${fnc.id}, " +
        s"assigning default uniform precision $defaultPrecision.")

      (allVariablesOf(body).map(v => (v -> defaultPrecision)).toMap,
        defaultPrecision)
    }

    // compute the roundoff error and return precision
    // this can be ideally cached, but recomputing is cheap enough
    val (resError, resPrecision) = computeAbsError(body, typeConfig, constPrec, rangeMap)

    if (uniformCostFunction.isDefined) {
      val finalSimpleCost = uniformCostFunction.get(body, typeConfig)
      reporter.debug(s"[${fnc.id}] Final uniform mixed precision cost: $finalSimpleCost")
    }

    // final step: apply found type config
    val updatedBody = applyFinitePrecision(body, typeConfig)(resPrecision)

    val updatedParams = fnc.params.map(valDef =>
      ValDef(valDef.id.changeType(FinitePrecisionType(typeConfig(valDef.id)))))

    (fnc.copy(returnType = FinitePrecisionType(resPrecision), params = updatedParams,
      body = Some(updatedBody)), resError, typeConfig)
  }

  def exprWithoutCasts(e: Expr): Expr = e match {
    case Cast(expr, tpe) => exprWithoutCasts(expr)
    case Variable(id) =>  e
    case FinitePrecisionLiteral(r, _, _) => RealLiteral(r)
    case t @ ArithOperator(args, recons) =>
      recons(args.map(exprWithoutCasts))
    case Let(id, value, body) =>
      Let(id, exprWithoutCasts(value), exprWithoutCasts(body))
  }

  /**
   * Performs Precimonious' delta-debugging search to find a valid type configuration.
   *
   * @param expr expression to be optimized
   * @param errorSpec target error, i.e. maximum abs. error tolerated
   * @param params free variable/function parameters of expr
   * @param errorFnc error function to use for evaluating the error (this is not fixed for testing purposes)
   * @param availablePrecisions which precisions are available for assigning
   * @return (type configuration, return precision)
   */
  def deltaDebuggingSearch(expr: Expr, errorSpec: Rational, params: Seq[ValDef],
    costFnc: (Expr, Map[Identifier, Precision]) => Rational,
    errorFnc: (Map[Identifier, Precision], Precision) => (Rational, Precision),
    availablePrecisions: Seq[Precision],
    reporter: Reporter): TypeConfig = {

    val highestPrecision = availablePrecisions.last
    val lowestPrecision = availablePrecisions.head

    // map from each precision to its index in the availablePrecisions list
    val precIndexMap: Map[Precision, Int] = availablePrecisions.zipWithIndex.toMap

    def lowerVariables(vars: Seq[Identifier], typeConfig: Map[Identifier, Precision]): Map[Identifier, Precision] = {
      typeConfig.map({
        case (id, prec) =>
          if (vars.contains(id)) {
            // lower the variable's precision
            (id -> availablePrecisions(precIndexMap(prec) - 1))
          } else {
            (id -> prec)
          }
      })
    }

    // constants that are representable in lowest precision and thus need not be optimised
    // used during experiments
    val constants = Seq[Identifier]()

    // all variables appearing in the expr **in order of appearance**
    val allVars: Seq[Identifier] = {
      val inputParams: Seq[Identifier] = params.map(_.id)

      val letDefs = lang.TreeOps.fold[Seq[Identifier]] {
        case (e, subs) =>
          val subvs = subs.flatten.toList
          e match {
            //case Variable(i) => subvs :+ i
            case Let(i, RealLiteral(r), _) if (lowestPrecision.canRepresent(r)) =>
              //constants :+= i; subvs
              subvs :+ i
            case Let(i, _, _) => subvs :+ i
            case _ => subvs
          }
      }(expr)
      inputParams ++ letDefs.reverse
    }
    assert(constants.size == 0)

    var numValidConfigs = 0
    val candidateTypeConfigs = MSet[TypeConfig]()   // for info purposes only

    // factor out so that it can be changed easily
    val lessThanCost: (Rational, Rational) => Boolean = lessThanByRationalCost

    // @param currentVars are those variables that we want to lower
    // @param typeConfig is the current Type configuration (of which we know that it is a valid one)
    // @returns the 'optimal' type config satisfying the spec and has smallest cost
    def deltaDebug(currentVars: Seq[Identifier], typeConfig: Map[Identifier, Precision], depth: Int): Map[Identifier, Precision] = {
      if (depth >= 1000) {
        reporter.warning("!! delta debugging, max depth level reached !!")
        typeConfig
      } else {

        val typeConfigCost = costFnc(expr, typeConfig)

        // 1: lower all variables in varsToOpt
        val loweredTypeConfig = lowerVariables(currentVars, typeConfig)
        candidateTypeConfigs += loweredTypeConfig

        // 2: evaluate current config
        val (currentError, _) = errorFnc(loweredTypeConfig, highestPrecision)

        // 3a: if the error is below threshold, we are done recursing
        if (currentError <= errorSpec) {
          numValidConfigs = numValidConfigs + 1
          val fixedVars = allVars.diff(currentVars)

          if (fixedVars.isEmpty) { // nothing to optimize further
            loweredTypeConfig
          } else {
            val (fixedVarsLeft, fixedVarsRight) = fixedVars.splitAt(fixedVars.length / 2)
            val loweredLeft = lowerVariables(fixedVarsLeft, loweredTypeConfig)
            val loweredRight = lowerVariables(fixedVarsRight, loweredTypeConfig)
            candidateTypeConfigs += loweredLeft; candidateTypeConfigs += loweredRight

            val (errorLeft, _) = errorFnc(loweredLeft, highestPrecision)
            val (errorRight, _) = errorFnc(loweredRight, highestPrecision)

            val loweredCost = costFnc(expr, loweredTypeConfig)

            // choose the one with lowest cost
            var (currentMinCost, currentBestConfig) = if (lessThanCost(typeConfigCost, loweredCost)) {
              (typeConfigCost, typeConfig)
            } else {
              (loweredCost, loweredTypeConfig)
            }

            if (errorLeft < errorSpec) {
              numValidConfigs = numValidConfigs + 1
              val costLeft = costFnc(expr, loweredLeft)
              if (lessThanCost(costLeft, currentMinCost)) {
                currentBestConfig = loweredLeft
                currentMinCost = costLeft
              }
            }

            if (errorRight < errorSpec) {
              numValidConfigs = numValidConfigs + 1
              val costRight = costFnc(expr, loweredRight)
              if (lessThanCost(costRight, currentMinCost)) {
                currentBestConfig = loweredRight
                currentMinCost = costRight
              }
            }

            currentBestConfig
          }

        } else {
          // 3b: if the error is not below, we need to continue to subdivide

          if (currentVars.length <= 1) {
            typeConfig
          } else {

            // subdivide variables, divide by 2 should be integer division
            val (currVarsLeft, currVarsRight) = currentVars.splitAt(currentVars.length / 2)

            val configLeft = deltaDebug(currVarsLeft, typeConfig, depth + 1)
            val configRight = deltaDebug(currVarsRight, typeConfig, depth + 1)

            val costLeft = costFnc(expr, configLeft)
            val costRight = costFnc(expr, configRight)

            if (lessThanCost(typeConfigCost, costLeft) && lessThanCost(typeConfigCost, costRight)) {
              typeConfig
            } else if (lessThanCost(costLeft, costRight)) {
              configLeft
            } else {
              configRight
            }
          }
        }
      }
    }


    // experiment, strawmans version of shuffle
    //allVars = allVars.toSet.toList

    // possible optimization: integer constants and contants representable in the lowest precision
    // are fixed to have lowest precision and are not optimized
    val initialTypeConfig = allVars.map(i => (i -> highestPrecision)).toMap ++
      constants.map(i => (i -> lowestPrecision)).toMap

    val originalCost = costFnc(expr, initialTypeConfig)

    var currentTypeConfig = initialTypeConfig
    var currentVars = allVars  // variables which can be lowered

    // initially the low precision is the second last precision
    var currentLowPrecIndex = availablePrecisions.length - 2

    var continue = true
    while (continue && currentLowPrecIndex >= 0) {
      // do delta debugging, which will lower some variables
      val newTypeConfig = deltaDebug(currentVars, currentTypeConfig, 0)

      // if nothing changed, stop
      if (newTypeConfig == currentTypeConfig) {
        continue = false
      } else {
        // something did change, so fix the higher-precision variables and only
        // keep the low-precision ones in the running
        val lowPrecision = availablePrecisions(currentLowPrecIndex)

        val varsToLowerFurther = allVars.filter((id => newTypeConfig(id) == lowPrecision))

        // update everything
        currentVars = varsToLowerFurther
        currentTypeConfig = newTypeConfig
        currentLowPrecIndex = currentLowPrecIndex - 1
      }
    }

    val finalCost = costFnc(expr, currentTypeConfig)
    reporter.info(s"initial Cost: $originalCost - final cost: $finalCost")
    reporter.debug(s"number of valid type configs: ---- $numValidConfigs, out of ${candidateTypeConfigs.size} unique configs seen")
    currentTypeConfig
  }

  /*
    Changes a real-valued program into a finite-precision one according to a given
    type configuration and by inserting necessary (down) casts.

  */
  def applyFinitePrecision(expr: Expr, typeConfig: Map[Identifier, Precision])(implicit returnPrec: Precision): Expr = {

    def recurse(e: Expr): Expr = (e: @unchecked) match {

      case x @ Variable(id) =>
        Variable(id.changeType(FinitePrecisionType(typeConfig(id))))

      case Let(id, x @ RealLiteral(r), body) =>
        val idPrec = typeConfig(id)
        val newValue = FinitePrecisionLiteral(r, idPrec, x.stringValue)

        Let(id.changeType(FinitePrecisionType(idPrec)), newValue, recurse(body))

      case Let(id, x @ Variable(t), body) =>
        val idPrec = typeConfig(id)
        val tPrec = typeConfig(t)

        val newValue = recurse(x)

        val withCasts = if (idPrec < tPrec) {
          Cast(newValue, FinitePrecisionType(idPrec))
        } else {
          newValue
        }

        Let(id.changeType(FinitePrecisionType(idPrec)), withCasts, recurse(body))

      case Let(id, x @ ArithOperator(Seq(t @ Variable(tId)), recons), body) =>
        val idPrec = typeConfig(id)
        val tPrec = typeConfig(tId)

        val updatedT = recurse(t)
        val opPrec = getUpperBound(tPrec, idPrec)

        // We only need to introduce an upcast for the inner operation or a downcast for the outer operation.
        // It can't happen that both are necessary, since tPrec < opPrec ==> idPrec = opPrec and
        // idPrec < opPrec ==> tPrec = opPrec.
        val withCasts: Expr =  if (tPrec < opPrec) { // need to introduce upcast for operation
          recons(Seq(Cast(updatedT, FinitePrecisionType(idPrec))))
        } else if (idPrec < opPrec) { // need to downcast for assignment
          Cast(recons(Seq(updatedT)), FinitePrecisionType(idPrec))
        } else {
          recons(Seq(recurse(t)))
        }

        Let(id.changeType(FinitePrecisionType(idPrec)), withCasts, recurse(body))


      case Let(id, x @ ArithOperator(Seq(y @ Variable(lhs), z @ Variable(rhs)), recons), body) =>
        var left = recurse(y)
        var right = recurse(z)

        val idPrec = typeConfig(id)
        val lPrec = typeConfig(lhs)
        val rPrec = typeConfig(rhs)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        // forced upcasts
        if (lPrec != opPrec) {
          left = Cast(left, FinitePrecisionType(opPrec))
        }
        if (rPrec != opPrec) {
          right = Cast(right, FinitePrecisionType(opPrec))
        }

        var tmp = recons(Seq(left, right))

        if (idPrec < opPrec) { //need to downcast
          tmp = Cast(tmp, FinitePrecisionType(idPrec))
        }
        Let(id.changeType(FinitePrecisionType(idPrec)), tmp, recurse(body))

      case x @ ArithOperator(Seq(t @ Variable(_)), recons) =>
        recons(Seq(recurse(t)))

      case x @ ArithOperator(Seq(y @ Variable(_), z @ Variable(_)), recons) =>
        val lPrec = typeConfig(y.id)
        val rPrec = typeConfig(z.id)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), returnPrec)

        var left = recurse(y)
        var right = recurse(z)

        // forced upcasts
        if (lPrec != opPrec) {
          left = Cast(left, FinitePrecisionType(opPrec))
        }
        if (rPrec != opPrec) {
          right = Cast(right, FinitePrecisionType(opPrec))
        }

        recons(Seq(left, right))
    }
    recurse(expr)
  }

  /*
    Computes the roundoff error for a finite-precision expression
    using the annotated ranges and the types from the given map.

    Note: this method uses different semantics than RoundoffEvaluators
    in that for x: Double = y: Float + z: Float, the operation will be done in
    the highest precision, i.e. Double.

    @return (absolute roundoff error of result, precision of result)
    TODO: I don't think we need to return the return precision any more
  */
  def computeAbsError(expr: Expr, precMap: Map[Identifier, Precision],
    constantsPrecision: Precision, rangeMap: Map[(Expr, PathCond), Interval],
    approximate: Boolean = false): (Rational, Precision) = {

    var valMap: Map[Identifier, MPFRAffineForm] = Map.empty

    // computes the new roundoff error for let statements
    // assuming the operation has been done in the highest precision around
    def addNewErrorLet(lhs: Identifier, rhs: Identifier, letID: Identifier, e: Expr,
      path: PathCond, propError: MPFRAffineForm): MPFRAffineForm = {

      val lPrec = precMap(lhs)
      val rPrec = precMap(rhs)
      val assignPrec = precMap(letID)
      val realRange = rangeMap(e, path)

      val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), assignPrec)
      val actualRange = realRange + propError.toInterval

      var rndoff = opPrec.absRoundoff(actualRange)

      if (assignPrec < opPrec) { // we need to cast down
        rndoff = rndoff + assignPrec.absRoundoff(actualRange +/- rndoff)
      }
      if (approximate) {
        rndoff = Rational.limitSize(rndoff)
      }

      propError :+ rndoff.toMPFRInterval  // only add one roundoff term
    }

    def addNewErrorLetUnary(t: Identifier, letID: Identifier, e: Expr, path: PathCond, propError: MPFRAffineForm,
      trans: Boolean = false): MPFRAffineForm = {

      val assignPrec = precMap(letID)
      val tPrec = precMap(t)
      val realRange = rangeMap(e, path)

      val opPrec = getUpperBound(assignPrec, tPrec)
      val actualRange = realRange + propError.toInterval

      var rndoff = if (trans) {
        opPrec.absTranscendentalRoundoff(actualRange)
      } else {
        opPrec.absRoundoff(actualRange)
      }

      if (assignPrec < opPrec) { // we need to cast down
        rndoff = rndoff + assignPrec.absRoundoff(actualRange +/- rndoff)
      }
      if (approximate) {
        rndoff = Rational.limitSize(rndoff)
      }

      propError :+ rndoff.toMPFRInterval
    }

    def addNewError(lhs: Identifier, rhs: Identifier, e: Expr, path: PathCond, propError: MPFRAffineForm): (MPFRAffineForm, Precision) = {
      val lPrec = precMap(lhs)
      val rPrec = precMap(rhs)

      val opPrec = getUpperBound(lPrec, rPrec)
      var rndoff = opPrec.absRoundoff(rangeMap(e, path) + propError.toInterval)
      if (approximate) {
        rndoff = Rational.limitSize(rndoff)
      }
      (propError :+ rndoff.toMPFRInterval, opPrec)
    }

    def addNewErrorUnary(t: Identifier, e: Expr, path: PathCond, propError: MPFRAffineForm,
      trans: Boolean = false): (MPFRAffineForm, Precision) = {
      val opPrec = precMap(t)
      var rndoff = if (trans) {
        opPrec.absTranscendentalRoundoff(rangeMap(e, path) + propError.toInterval)
      } else {
        opPrec.absRoundoff(rangeMap(e, path) + propError.toInterval)
      }
      if (approximate) {
        rndoff = Rational.limitSize(rndoff)
      }
      (propError :+ rndoff.toMPFRInterval, opPrec)
    }

    // returns the error and the type, since the types are not attached to the node,
    // we need to propagate them
    def eval(e: Expr, path: PathCond): (MPFRAffineForm, Precision) = e match {

      // if x has been evaluated before (via let, or inside the expression)
      // we need to cache the errors, or else we are loosing correlations
      case x @ Variable(id) if (valMap.isDefinedAt(id)) =>
        val aform = valMap(id)
        (aform, precMap(id))

      // haven't seen this variable yet
      case x @ Variable(id) =>
        // new error based on interval and data type/precision
        val prec = precMap(id)
        val rndoff = prec.absRoundoff(rangeMap(x, path))
        val aform = MPFRAffineForm.+/-(rndoff)
        valMap  = valMap + (id -> aform)
        (aform, prec)

      // since mixed-precision operates over SSA form, all RealLiterals should show up like this
      // without treating them separately, we do double rounding
      case Let(id, RealLiteral(r), body) =>
        val prec = precMap(id)
        val aformValue = if (prec.canRepresent(r)) {
          MPFRAffineForm.zero
        } else {
          MPFRAffineForm.+/-(prec.absRoundoff(r))
        }
        valMap = valMap + (id -> aformValue)
        eval(body, path)

      case x @ RealLiteral(r) =>
        if (constantsPrecision.canRepresent(r)) {
          (MPFRAffineForm.zero, constantsPrecision)
        } else {
          (MPFRAffineForm.+/-(constantsPrecision.absRoundoff(r)), constantsPrecision)
        }

      case Let(id, x @ Plus(l @ Variable(lhs), r @ Variable(rhs)), body) =>
        val (lAform, _) = eval(l, path)
        val (rAform, _) = eval(r, path)

        // propagated error
        val aform: MPFRAffineForm = lAform + rAform

        val newError = addNewErrorLet(lhs, rhs, id, x, path, aform)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Minus(l @ Variable(lhs), r @ Variable(rhs)), body) =>
        val (lAform, _) = eval(l, path)
        val (rAform, _) = eval(r, path)

        // propagated error
        val aform: MPFRAffineForm = lAform - rAform

        val newError = addNewErrorLet(lhs, rhs, id, x, path, aform)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Times(l @ Variable(lhs), r @ Variable(rhs)), body) =>
        val (lAform, _) = eval(l, path)
        val (rAform, _) = eval(r, path)

        // propagated error
        val lAA = MPFRAffineForm(rangeMap(l, path))
        val rAA = MPFRAffineForm(rangeMap(r, path))
        val lErr = lAform
        val rErr = rAform
        val aform: MPFRAffineForm = lAA*rErr + rAA*lErr + lErr*rErr

        val newError = addNewErrorLet(lhs, rhs, id, x, path, aform)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Division(l @ Variable(lhs), r @ Variable(rhs)), body) =>
        val (errorLhs, _) = eval(l, path)
        val (errorRhs, _) = eval(r, path)

        // propagated error

        val rangeRhs = rangeMap(r, path)
        val rangeLhs = rangeMap(l, path)

        val rightInterval = rangeRhs + errorRhs.toInterval // the actual interval, incl errors

        if (rightInterval.includes(Rational.zero)) {
          throw tools.DivisionByZeroException("trying to divide by error interval containing 0")
        }

        val a = Interval.minAbs(rightInterval)
        val errorMultiplier = -one / (a*a)

        val invErr = errorRhs * errorMultiplier

        val inverse: Interval = rangeRhs.inverse

        // multiplication with inverse
        val aform = MPFRAffineForm(rangeLhs) * invErr +
          MPFRAffineForm(inverse) * errorLhs +
          errorLhs * invErr

        val newError = addNewErrorLet(lhs, rhs, id, x, path, aform)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ UMinus(y @ Variable(t)), body) =>
        // propagated error
        var newError = - eval(y, path)._1

        // no additional roundoff error, but we could have a downcast
        val assignPrec = precMap(id)
        if (assignPrec < precMap(t)) { // we need to cast down
          val castRoundoff = assignPrec.absRoundoff(rangeMap(x, path) + newError.toInterval)
          newError = newError :+ castRoundoff.toMPFRInterval
        }
        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Sqrt(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val a = min(abs(rangeT.xlo), abs(rangeT.xhi))
        val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)
        val aform = errorT * errorMultiplier

        val newError = addNewErrorLetUnary(t, id, x, path, aform)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Sin(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val deriv =  rangeT.cosine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propError = errorT * errorMultiplier

        val newError = addNewErrorLetUnary(t, id, x, path, propError, trans = true)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Cos(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val deriv = -rangeT.sine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propError = errorT * errorMultiplier

        val newError = addNewErrorLetUnary(t, id, x, path, propError, trans = true)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Tan(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val intCosine = rangeT.cosine
        val deriv = (intCosine * intCosine).inverse

        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propError = errorT * errorMultiplier

        val newError = addNewErrorLetUnary(t, id, x, path, propError, trans = true)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Atan(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        // d/dx(atan(x)) = 1 / (x**2 + 1)
        val Interval(a, b) = rangeT

        val errorMultiplier = if (a >= Rational.zero) {
          // The interval is fully above zero, so the maximum derivative is at 1 / (a**2 + 1)
          1 / (a * a + Rational.one)
        } else if (b <= Rational.zero) {
          // The interval is fully below zero, so the maximum derivative is at 1 / (b**2 + 1), since b is the closest to
          // zero.
          1 / (b * b + Rational.one)
        } else {
          // The interval contains zero, so the maximum value is 1 / (0**2 + 1) = 1
          Rational.one
        }

        val propError = errorT * errorMultiplier

        val newError = addNewErrorLetUnary(t, id, x, path, propError, trans = true)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case Let(id, x @ Exp(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val b = rangeT.xhi
        val errorMultiplier = expUp(b)
        val propError = errorT * errorMultiplier

        val newError = addNewErrorLetUnary(t, id, x, path, propError, trans = true)

        valMap = valMap + (id -> newError)
        eval(body, path)


      case Let(id, x @ Log(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val a = rangeT.xlo
        val errorMultiplier = Rational.one / a
        val propError = errorT * errorMultiplier

        val newError = addNewErrorLetUnary(t, id, x, path, propError, trans = true)

        valMap = valMap + (id -> newError)
        eval(body, path)

      case x @ Plus(l @ Variable(lhs), r @ Variable(rhs)) =>
        val (lAform, _) = eval(l, path)
        val (rAform, _) = eval(r, path)

        // propagated error
        val aform: MPFRAffineForm = lAform + rAform

        addNewError(lhs, rhs, x, path, aform)

      case x @ Minus(l @ Variable(lhs), r @ Variable(rhs)) =>
        val (lAform, _) = eval(l, path)
        val (rAform, _) = eval(r, path)

        // propagated error
        val aform: MPFRAffineForm = lAform - rAform

        addNewError(lhs, rhs, x, path, aform)

      case x @ Times(l @ Variable(lhs), r @ Variable(rhs)) =>
        val (lAform, _) = eval(l, path)
        val (rAform, _) = eval(r, path)

        // propagated error
        val lAA = MPFRAffineForm(rangeMap(l, path))
        val rAA = MPFRAffineForm(rangeMap(r, path))
        val lErr = lAform
        val rErr = rAform

        val aform: MPFRAffineForm = lAA*rErr + rAA*lErr + lErr*rErr

        addNewError(lhs, rhs, x, path, aform)

      case x @ Division(l @ Variable(lhs), r @ Variable(rhs)) =>
        val (errorLhs, _) = eval(l, path)
        val (errorRhs, _) = eval(r, path)

        // propagated error

        val rangeRhs = rangeMap(r, path)
        val rangeLhs = rangeMap(l, path)

        val rightInterval = rangeRhs + errorRhs.toInterval // the actual interval, incl errors

        val a = Interval.minAbs(rightInterval)
        val errorMultiplier = -one / (a*a)

        val invErr = errorRhs * errorMultiplier

        val inverse: Interval = rangeRhs.inverse

        // multiplication with inverse
        val aform = MPFRAffineForm(rangeLhs) * invErr +
          MPFRAffineForm(inverse) * errorLhs +
          errorLhs * invErr

        addNewError(lhs, rhs, x, path, aform)

      case x @ Sqrt(y @ Variable(t)) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val a = min(abs(rangeT.xlo), abs(rangeT.xhi))
        val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)
        val aform = errorT * errorMultiplier

        addNewErrorUnary(t, x, path, aform)

      case Let(id, x @ Sin(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val deriv =  rangeT.cosine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propError = errorT * errorMultiplier

        addNewErrorUnary(t, x, path, propError, trans = true)

      case Let(id, x @ Cos(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val deriv = -rangeT.sine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propError = errorT * errorMultiplier

        addNewErrorUnary(t, x, path, propError, trans = true)

      case Let(id, x @ Tan(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val intCosine = rangeT.cosine
        val deriv = (intCosine * intCosine).inverse

        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propError = errorT * errorMultiplier

        addNewErrorUnary(t, x, path, propError, trans = true)

      case x @ Atan(y @ Variable(t)) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        // d/dx(atan(x)) = 1 / (x**2 + 1)
        val Interval(a, b) = rangeT

        val errorMultiplier = if (a >= Rational.zero) {
          // The interval is fully above zero, so the maximum derivative is at 1 / (a**2 + 1)
          1 / (a * a + Rational.one)
        } else if (b <= Rational.zero) {
          // The interval is fully below zero, so the maximum derivative is at 1 / (b**2 + 1), since b is the closest to
          // zero.
          1 / (b * b + Rational.one)
        } else {
          // The interval contains zero, so the maximum value is 1 / (0**2 + 1) = 1
          Rational.one
        }

        val propError = errorT * errorMultiplier

        addNewErrorUnary(t, x, path, propError, trans = true)

      case Let(id, x @ Exp(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val b = rangeT.xhi
        val errorMultiplier = expUp(b)
        val propError = errorT * errorMultiplier

        addNewErrorUnary(t, x, path, propError, trans = true)

      case Let(id, x @ Log(y @ Variable(t)), body) =>
        val rangeT = rangeMap(y, path)

        val errorT = eval(y, path)._1

        val a = rangeT.xlo
        val errorMultiplier = Rational.one / a
        val propError = errorT * errorMultiplier

        addNewErrorUnary(t, x, path, propError, trans = true)

      case x @ UMinus(t) =>
        val (aform, prec) = eval(t, path)   // no additional roundoff error
        ( - aform, prec)

      case Let(binder, x @ Variable(t), body) =>
        val (aform, _) = eval(x, path)

        val error = if (precMap(binder) < precMap(t)) { // we need to cast down
          MPFRAffineForm(precMap(binder).absRoundoff(rangeMap(x, path) + aform.toInterval))
        } else {
          aform
        }

        valMap  = valMap + (binder -> error)
        eval(body, path)

      // TODO: case x @ IfExpr(cond, thenn, elze) =>
    }
    val (aform, prec) = eval(expr, emptyPath)
    (Interval.maxAbs(aform.toInterval), prec)
  }



  /**
   * Generates maxTries random type configurations and returns the one which
   * - satisfies the error specification (worst-case absolute error) and
   * - has smallest cost as given by the cost function
   *
   * @param expr AST to be optimized
   * @param maxTries maximum number of type configurations to try
   * @param errorSpec worst-case absolute error to tolerate
   * @param costFnc function to evaluate the cost of a type config
   * @return (type configuration, precision of return value)
   * TODO: the cost function does not need to return Rationals, perhaps Double will do?
   */
  def randomSearch(expr: Expr, errorSpec: Rational, costFnc: (Expr, Map[Identifier, Precision]) => Rational,
    errorFnc: (Map[Identifier, Precision], Precision) => (Rational, Precision),
    availablePrecisions: Seq[Precision], reporter: Reporter, maxTries: Int = 1000): TypeConfig = {

    val rand = new Random(4789)

    val numPrecisions = availablePrecisions.length
    val highestPrecision = availablePrecisions.last
    val ids = allVariablesOf(expr)

    reporter.info("-----Starting random search----")

    // try a reasonable number of configs
    val maxUniqueTypeConfigs: Long = math.pow(numPrecisions, ids.size).toLong
    val maxCandTypeConfigs: Long = math.min(maxTries, maxUniqueTypeConfigs)
    // the factor is to accommodate random assignment to pick the same
    val maxIterCount = 15 * maxCandTypeConfigs

    var iterCount = 0L

    val candidateTypeConfigs = MSet[TypeConfig]()

    // randomly sample candidate configs
    while (candidateTypeConfigs.size < maxCandTypeConfigs && iterCount < maxIterCount) {
      val tmp = ids.map(id => (id -> availablePrecisions(rand.nextInt(numPrecisions)))).toMap
      candidateTypeConfigs += tmp
      iterCount = iterCount + 1
    }
    //if (iterCount >= maxIterCount) reporter.warning("maxIterCount reached in random search")
    if (candidateTypeConfigs.size == maxUniqueTypeConfigs) reporter.info("exhaustive search")

    reporter.info(s"Generated ${candidateTypeConfigs.size} unique type configs, " +
      s"exhaustive search would need: $maxUniqueTypeConfigs")

    // test all of them, and keep track of best (only)
    //val veryLargeRational = Rational(100000L, 1L)
    var numValidConfigs = 0

    // default is the highest precision, since we know that that must work
    var bestCandidate: TypeConfig = ids.map(v => (v -> availablePrecisions.last)).toMap
    //var bestReturnPrec: Precision = availablePrecisions.last
    var bestCost: Rational = costFnc(expr, bestCandidate)

    candidateTypeConfigs.foreach(tpeConfig => {

      val (error, _) = errorFnc(tpeConfig, highestPrecision)

      if (error <= errorSpec) {
        // hard-constraint error spec is satisfied, so the cost function decides
        numValidConfigs += 1

        val cost = costFnc(expr, tpeConfig)
        if(cost < bestCost) {
          bestCandidate = tpeConfig
          //bestReturnPrec = retPrec
          bestCost = cost
        }

      }
    })
    reporter.info(s"number of valid type configs: ---- $numValidConfigs ----")
    //logNumValid ++= s"$numValidConfigs/${candidateTypeConfigs.size} "
    //logMinCost ++= s"$bestCost "

    bestCandidate //, bestReturnPrec)
  }


  // ----------- Things for genetic search ------------
  var rand = new Random(123456789)

  /*
    Things to tune:
      - mutation function: should we also increase the precision again (with some prob)
      - crossover a percentage of vars instead of just two
      - for crossover perhaps keep a list of types in order of appearance? somehow, this will need to be a hack
      - figure out if starting from high and going to low is the right choice
        and how to adjust the fitness function accordingly
      - TODO: we may want to do it the other way around, since we found that there
        are few valid configs close the higher errors?
      - is the # of generations and population size sufficient, should it be increases based on size of benchmark?
  */

  def mutate(tpeConfig: TypeConfig): TypeConfig = {

    // randomly pick a node
    val keys = tpeConfig.keys.toList
    val id = keys(rand.nextInt(keys.size))

    // decide what to do with the associated type
    val currentType = tpeConfig(id)
    val newType = (currentType: @unchecked) match {
      case Float32 => Float32     // TODO: perhaps we want to move it up again (with some prob?)
      case Float64 => Float32
      case Float128 => Float64
    }

    // update tpeConfig
    val newConfig = tpeConfig + (id -> newType)

    newConfig
  }

  override def crossover(t1: TypeConfig, t2: TypeConfig): (TypeConfig, TypeConfig) = {

    // lets pick two ids and switch them
    val keys = t1.keys.toList     // these are not in the same order as they appear in the program...
    val index = rand.nextInt(keys.size - 1)
    val (id1, id2) = (keys(index), keys(index + 1))

    val newTypeConfig1 = t1 + (id1 -> t2(id1)) + (id2 -> t2(id2))
    val newTypeConfig2 = t2 + (id1 -> t1(id1)) + (id2 -> t1(id2))

    (newTypeConfig1, newTypeConfig2)
  }

  /**
   * Runs a genetic search to find a valid type configuration
   * @param  expr expression to be optimized
   * @param  errorSpec maximum abs error tolerated
   * @param  costFnc function to evaluate the cost of a type config
   * @return (type configuration, return precision)
   */
  def geneticSearch(expr: Expr, errorSpec: Rational,
    costFnc: (Expr, Map[Identifier, Precision]) => Rational,
    errorFnc: (Map[Identifier, Precision], Precision) => (Rational, Precision),
    availablePrecisions: Seq[Precision], reporter: Reporter): TypeConfig = {

    reporter.info("-----Starting genetic search----")

    val highestPrecision = availablePrecisions.last
    val maxCost = Rational(100000)
    var numValidConfigs = 0
    var validConfigs = MSet[TypeConfig]()

    // cache results; sometimes there are duplicates
    val cache = collection.mutable.HashMap[TypeConfig, Rational]()
    var numUsedCache = 0

    // initialize population with all variables at the highest available precision
    val initialTypeConfig: TypeConfig = allVariablesOf(expr).map(id => (id, highestPrecision)).toMap


    val (newTypeConfig, _) = runGenetic(
      initialTypeConfig,
      //(t: TypeConfig) => t,   // why does copy not work? do we need to copy, the map is immutable...?
      (tpeConfig: TypeConfig) => {

        // the fitness is a combination of the error and the cost
        val error = if (cache.contains(tpeConfig)) {
          numUsedCache = numUsedCache + 1
          cache(tpeConfig)
        } else {
          val (tmp, _) = errorFnc(tpeConfig, highestPrecision)
          cache += (tpeConfig -> tmp)
          tmp
        }

        val cost = if (error <= errorSpec) {
          //numValidConfigs += 1
          validConfigs += tpeConfig
          costFnc(expr, tpeConfig)

        } else {
          // the maxCost is added so that we penalize those configs which do not satisfy the spec
          // if we compare two configs which do not satisfy the errorSpec,
          //then the smaller error is better as it is closer to satisfying the spec

          maxCost + error
        }
        cost
      },
      crossoverProb = 0.3)

    reporter.info(s"number of valid type configs: ---- $numValidConfigs, unique: ${validConfigs.size}----")
    reporter.info(s"# used cache: $numUsedCache")
    //val bestCost = costFnc(expr, newTypeConfig)

    newTypeConfig //, computeReturnPrecision(expr, newTypeConfig, highestPrecision))
  }

}
