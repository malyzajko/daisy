package daisy
package opt

import scala.collection.immutable.Seq
import scala.collection.mutable.{Set => MSet}
import util.Random
import lang.Trees._
import lang.Types._
import lang.Identifiers._
import tools.FinitePrecision._
import lang.TreeOps._
import tools.{AffineForm, Interval, Rational}
import lang.Extractors.{ArithOperator, ElemFnc}

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

  override val name = "mixed-precision optimization"
  override val description = "determines a suitable mixed-precision type assignment"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption("mixed-opt-method", Set("random", "delta", "genetic"), "delta",
      "Algorithm to use for mixed-precision optimization")
  )

  override implicit val debugSection = DebugSectionOptimization

  var reporter: Reporter = null

  type TypeConfig = Map[Identifier, Precision]
  type RangeMap = Map[(Expr, Seq[Expr]), Interval]

  var optimizationMethod: String = ""

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    val defaultPrecision = ctx.option[Precision]("precision")
    optimizationMethod = ctx.option[String]("mixed-opt-method")

    val availablePrecisions = defaultPrecision match {
      case FixedPrecision(b) => // given precision specifies the upper bound
        (1 to b).map(FixedPrecision(_))
      case _ =>
        Seq(Float32, Float64, DoubleDouble)
    }
    reporter.info(s"Optimisation method: $optimizationMethod")

    // store updated ranges for each function
    var resAbsoluteErrors: Map[Identifier, Rational] = Map()
    var precisionMap: Map[Identifier, Map[Identifier, Precision]] = Map()
    var newIntermediateRanges: Map[Identifier, RangeMap] = Map()
    var newInputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()

    // only consider function which have no assigned types yet
    val fncsToConsider = if (ctx.hasFlag("approx")) functionsToConsider(ctx, prg).filter(_.returnType == RealType)
      else functionsToConsider(ctx, prg)

    val newDefs: Seq[FunDef] = fncsToConsider.map(fnc => {
      val fullBody = fnc.body.get
      val _rangeMap = ctx.intermediateRanges(fnc.id)

      // there is an output error to optimize for
      val (typeConfig, constPrec) = if (ctx.specResultErrorBounds.contains(fnc.id)) {
        val targetError = ctx.specResultErrorBounds(fnc.id)

        val paths = extractPaths(fullBody, emptyPath, _rangeMap)

        val res: Seq[(TypeConfig, Precision)] = paths.map({ case (pathCond, _body, rangeMap) =>
          reporter.info(s"Analyzing path: $pathCond")
          val body = removeUnusedVars(_body)

          // Step 1: find the smallest available precision which satisfies the error bound
          // TODO: search from the end, the first one which does not satisfy the spec
          availablePrecisions.find( prec => {
            try {
              // path may contain unused variables, which is why use the allIDsOf here
              // TODO: remove unused variables, as they may produce suboptimal results
              val rndoff = computeAbsError(body, allIDsOf(body).map(id => (id -> prec)).toMap,
                prec, rangeMap, pathCond, approximate = false)
              rndoff <= targetError
            } catch {  // e.g. overflow when precision is not enough
              case _ : Throwable => false
            }
          }) match {
            case None =>
              reporter.warning(s"Highest precision is *not enough* for ${fnc.id}; " +
                s"generating the highest-precision (${availablePrecisions.last}) version anyway.")
              (allIDsOf(body).map(v => (v -> availablePrecisions.last)).toMap,
                availablePrecisions.last)

            case Some(prec) if prec == availablePrecisions.head =>
              reporter.info(s"No mixed-precision optimization needed for ${fnc.id} - " +
                s"lowest (${availablePrecisions.head}) is sufficient")
              (allIDsOf(body).map(v => (v -> prec)).toMap, prec)

            case Some(lowestUniformPrec) =>
              val indexLowestUniformPrec = availablePrecisions.indexOf(lowestUniformPrec)

              // Step 2: optimize mixed-precision
              reporter.info(s"Optimizing mixed-precision for ${fnc.id}...")
              val consideredPrecisions = availablePrecisions.take(indexLowestUniformPrec + 1)
              reporter.info(s"Lowest possible uniform precision: ${availablePrecisions(indexLowestUniformPrec)}")

              val costFnc = consideredPrecisions.last match {
                case DoubleDouble | QuadDouble =>
                  simpleMixedPrecisionCost _

                case FixedPrecision(_) =>
                  val original = ctx.originalProgram.defs.find(_.id == fnc.id) match {
                    case Some(x) => x.body.get
                    case None => throw new Exception(s"Original body of the function ${fnc.id} is not available in the context")
                  }
                  ctx.reporter.debug(s"Using ${ctx.option[String]("cost")} cost")
                  ctx.option[String]("cost") match {
                    case "area" => areaBasedCostFunction _
                    case "ml" => mlRegressionCostFunction(original, _:Expr, _:TypeConfig)
                    case "combined" => combinedCost(original, _:Expr, _:TypeConfig)
                  } //  simpleMixedPrecisionCost _

                case _ =>
                  benchmarkedMixedPrecisionCost _
              }

              optimizationMethod match {
                case "delta" =>

                  val (tpeconfig, prec) = (deltaDebuggingSearch(body, targetError, fnc.params, costFnc,
                    computeAbsError(body, _, _, rangeMap, pathCond, approximate = true),
                    consideredPrecisions),
                    lowestUniformPrec)

                  (tpeconfig, prec)

                case "random" =>

                  (randomSearch(body, targetError, costFnc,
                    computeAbsError(body, _, _, rangeMap, pathCond, approximate = true),
                    consideredPrecisions, maxTries = 1000),
                    lowestUniformPrec)

                case "genetic" =>

                  (geneticSearch(body, targetError, benchmarkedMixedPrecisionCost,
                    computeAbsError(body, _, _, rangeMap, pathCond, approximate = true),
                    consideredPrecisions),
                  lowestUniformPrec)

              }
          }
        })

        val (tpeConfigs, constPrecs) = res.unzip

        (mergeTypeConfigs(tpeConfigs), constPrecs.max)

      } else {
        // If no error is given in the postcondition, assign default precision
        reporter.warning(s"No target error bound for ${fnc.id}, " +
          s"assigning default uniform precision $defaultPrecision.")

        (allVariablesOf(fullBody).map(v => (v -> defaultPrecision)).toMap,
          defaultPrecision)
      }

      // final step: apply found type config, and update ranges (mainly due to casts)
      // types have only been applied to variables used in computation, but not conditionals
      // assign default precision to those:
      val _typeConfig = (allIDsOf(fullBody) -- typeConfig.keys).map(id => (id -> constPrec)).toMap
      val newTypeConfig = typeConfig ++ _typeConfig
      val (updatedBody, newRangeMap) = applyFinitePrecision(fullBody, newTypeConfig, _rangeMap)

      // this is what computeAbsError does, but we need the intermediate information
      val inputErrorMap: Map[Identifier, Rational] = freeVariablesOf(fullBody).map({
        case id => (id -> newTypeConfig(id).absRoundoff(_rangeMap((Variable(id), emptyPath))))
      }).toMap
      val (resRoundoff, _) = evalRoundoff[AffineForm](updatedBody, _rangeMap ++ newRangeMap,
        newTypeConfig, inputErrorMap.mapValues(AffineForm.+/-).toMap, AffineForm.zero, AffineForm.+/-,
        AffineForm.apply, constPrec, true, false)
      val resError = Interval.maxAbs(resRoundoff.toInterval)

      resAbsoluteErrors = resAbsoluteErrors + (fnc.id -> resError)
      precisionMap = precisionMap + (fnc.id -> (newTypeConfig))
      newIntermediateRanges = newIntermediateRanges + (fnc.id -> newRangeMap)
      newInputErrors = newInputErrors + (fnc.id -> inputErrorMap)

      val updatedParams = fnc.params.map(valDef =>
        ValDef(valDef.id.changeType(FinitePrecisionType(typeConfig(valDef.id)))))

      val resPrecision = (updatedBody.getType: @unchecked) match {
        case FinitePrecisionType(tpe) => tpe
      }

      fnc.copy(returnType = FinitePrecisionType(resPrecision), params = updatedParams,
        body = Some(updatedBody))

    }) // end of newDefs

    (ctx.copy(resultAbsoluteErrors = resAbsoluteErrors,
      intermediateRanges = ctx.intermediateRanges ++ newIntermediateRanges,
      specInputPrecisions = ctx.specInputPrecisions ++ precisionMap,
      assignedPrecisions = ctx.specInputPrecisions ++ precisionMap,
      specInputErrors = ctx.specInputErrors ++ newInputErrors),
      Program(prg.id, newDefs ++ (functionsToConsider(ctx, prg).diff(fncsToConsider)))) // todo when fnc to consider is specified add approx ones too
  }

  /*
    Computes the roundoff error by first introducing casts into the expressions,
    based on the typeConfig, and then running the standard roundoff error analysis.
   */
  def computeAbsError(expr: Expr, typeConfig: Map[Identifier, Precision],
    constantsPrecision: Precision, rangeMap: Map[(Expr, PathCond), Interval],
    path: PathCond, approximate: Boolean = false): Rational = {

    // create finite-precision version
    val (finitePrecBody, newRangeMap) = applyFinitePrecision(expr, typeConfig, rangeMap)

    // roundoff errors depend on the typeConfig, need to be recomputed
    val inputErrorMap: Map[Identifier, Rational] = freeVariablesOf(expr).map({
      case id: Identifier =>
        (id -> typeConfig(id).absRoundoff(rangeMap((Variable(id), emptyPath))))
    }).toMap

    // run regular roundoff error analysis
    val (resRoundoff, _) = evalRoundoff[AffineForm](finitePrecBody, rangeMap ++ newRangeMap,
      typeConfig,
      inputErrorMap.mapValues(AffineForm.+/-).toMap,
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = constantsPrecision,
      trackRoundoffErrors = true,
      approxRoundoff = approximate)

    Interval.maxAbs(resRoundoff.toInterval)
  }

  // merges type configs from each path by taking the type upper bound
  // for a variable which has been assigned in different paths
  private def mergeTypeConfigs(configs: Seq[TypeConfig]): TypeConfig = {
    val allIDs: Seq[Identifier] = configs.flatMap(_.keys).toSet.toList

    allIDs.map(id => {
      val allPrec: Seq[Precision] = configs.flatMap(_.get(id))

      if (allPrec.size == 1) {
        (id -> allPrec.head)
      } else {
        (id -> allPrec.tail.fold(allPrec.head)({
          case (prec, newPrec) => getUpperBound(prec, newPrec)
        }))
      }
    }).toMap
  }

  // Extracts paths in the program and for each computes
  // - the path condition
  // - the path expression
  // - the new rangeMap for the new path (without the path condition)
  private def extractPaths(e: Expr, path: PathCond, currRanges: RangeMap): Seq[(PathCond, Expr, RangeMap)] = e match {

    case Let(id, value, body) =>
      // there shouldn't be any if-expr in the value (at least we don't support it)
      val (_, _, _rangeMapValue) = extractPaths(value, path, currRanges).head
      val rangeMapValue = _rangeMapValue + ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath)))

      extractPaths(body, path, currRanges).map({
        case (pCond: PathCond, pathBody: Expr, rangeMapBody) =>
          (pCond, Let(id, value, pathBody), rangeMapValue ++ rangeMapBody)
      })

    case IfExpr(cond, thenn, elze) =>
      val thenPaths = extractPaths(thenn, path :+ cond, currRanges).map({
        case (pCond: PathCond, pathBody: Expr, rangeMap) => (cond +: pCond, pathBody, rangeMap)
      })
      val elsePaths = extractPaths(elze, path :+ negate(cond), currRanges).map({
        case (pCond: PathCond, pathBody: Expr, rangeMap) => (negate(cond) +: pCond, pathBody, rangeMap)
      })
      thenPaths ++ elsePaths

    case x @ ArithOperator(args, recons) =>
      val rangeMap: RangeMap = args.map( arg => {
        (arg, emptyPath) -> currRanges(arg, path)
      }).toMap + ((x, emptyPath) -> currRanges(x, path))
      Seq((Seq(), e, rangeMap))

    case _ => Seq((Seq(), e, Map((e, emptyPath) -> currRanges(e, path))))

  }

  def removeUnusedVars(body: Expr): Expr = {
    val unused = allIDsOf(body) -- allVariablesOf(body)

    def remove(e: Expr): Expr = e match {
      // var is unused, skip
      case Let(id, v, b) if (unused.contains(id)) => remove(b)
      case Let(id, v, b) => Let(id, v, remove(b))
      case _ => e
    }

    remove(body)
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
    errorFnc: (Map[Identifier, Precision], Precision) => Rational,
    availablePrecisions: Seq[Precision]): TypeConfig = {

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

        // 1: lower all variables in varsToOpt
        val loweredTypeConfig = lowerVariables(currentVars, typeConfig)
        candidateTypeConfigs += loweredTypeConfig

        // 2: evaluate current config
        val currentError = errorFnc(loweredTypeConfig, highestPrecision)

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

            val errorLeft = errorFnc(loweredLeft, highestPrecision)
            val errorRight = errorFnc(loweredRight, highestPrecision)

            // choose the one with lowest cost
            var currentMinCost = costFnc(expr, loweredTypeConfig)
            var currentBestConfig = loweredTypeConfig
            /* TODO: Robert' edit (but unclear why it is correct)
            val typeConfigCost = costFnc(expr, typeConfig)
            val loweredCost = costFnc(expr, loweredTypeConfig)
            // choose the one with lowest cost
            var (currentMinCost, currentBestConfig) = if (lessThanCost(typeConfigCost, loweredCost)) {
              (typeConfigCost, typeConfig)
            } else {
              (loweredCost, loweredTypeConfig)
            }*/

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

            if (lessThanCost(costLeft, costRight)) {
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
    reporter.info(s"number of valid type configs: ---- $numValidConfigs, out of ${candidateTypeConfigs.size} unique configs seen")
    currentTypeConfig
  }

  /*
    Changes a real-valued program into a finite-precision one according to a given
    type configuration and by inserting necessary (down) casts.

    In addition, this function also computes the updated range map, including
    expressions with casts.
  */
  def applyFinitePrecision(expr: Expr, typeConfig: Map[Identifier, Precision],
    currRanges: RangeMap): (Expr, RangeMap) = {

    def recurse(e: Expr, path: PathCond): (Expr, RangeMap) = (e: @unchecked) match {

      case x @ Variable(id) =>
        val newExpr = Variable(id.changeType(FinitePrecisionType(typeConfig(id))))
        (newExpr, Map((newExpr, emptyPath) -> currRanges(x, emptyPath)))

      case Let(id, x @ Variable(idV), body) =>
        val idPrec = typeConfig(id)
        val newValue = Variable(idV.changeType(FinitePrecisionType(typeConfig(idV))))
        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, bodyMap + ((newValue, path) -> currRanges(x, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))

      case Let(id, x @ RealLiteral(r), body) =>
        val idPrec = typeConfig(id)
        val newValue = FinitePrecisionLiteral(r, idPrec, x.stringValue)
        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, bodyMap + ((newValue, path) -> currRanges(x, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))

      case Let(id, x @ ElemFnc(t @ Variable(tId), recons), body) =>
        val idPrec = typeConfig (id)

        val (valueExpr, valueMap) = recurse(t, path)
        var newValue: Expr = recons(valueExpr)
        if (idPrec < typeConfig(tId) ) { // need to downcast
        newValue = Cast(newValue, FinitePrecisionType(idPrec))
        }

        val (bodyExpr, bodyMap) = recurse (body, path)

        val newExpr = Let (id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, (bodyMap ++ valueMap) + ((newValue, path) -> currRanges (x, path) ) +
        ((Variable (id), emptyPath) -> currRanges ((Variable (id), emptyPath) ) ) +
          ((x, path) -> currRanges (x, path) ) ) // fixes the issue of java.util.NoSuchElementException: key not found: (sin(_tmp),List())


      case Let(id, x @ ArithOperator(Seq(t @ Variable(tId)), recons), body) =>
        val idPrec = typeConfig(id)

        val (valueExpr, valueMap) = recurse(t, path)
        var newValue: Expr = recons(Seq(valueExpr))
        if (idPrec < typeConfig(tId)) { // need to downcast
          newValue = Cast(newValue, FinitePrecisionType(idPrec))
        }

        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, (bodyMap ++ valueMap) + ((newValue, path) -> currRanges(x, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))

      case Let(id, x @ ArithOperator(Seq(y @ Variable(lhs), z @ Variable(rhs)), recons), body) =>
        val idPrec = typeConfig(id)
        val lPrec = typeConfig(lhs)
        val rPrec = typeConfig(rhs)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        val leftExpr = Variable(lhs.changeType(FinitePrecisionType(opPrec)))
        val rightExpr = Variable(rhs.changeType(FinitePrecisionType(opPrec)))

        val leftMap: RangeMap = Map((leftExpr, emptyPath) -> currRanges(y, emptyPath))
        val rightMap = Map((rightExpr, emptyPath) -> currRanges(z, emptyPath))

        var newValue = recons(Seq(leftExpr, rightExpr))

        val rangeMap = (leftMap ++ rightMap) + ((newValue, path) -> currRanges(x, path))
        if (idPrec < opPrec) { //need to downcast
          newValue = Cast(newValue, FinitePrecisionType(idPrec))
        }

        val (bodyExpr, bodyMap) = recurse(body, path)

        val newExpr = Let(id.changeType(FinitePrecisionType(idPrec)), newValue, bodyExpr)

        (newExpr, (rangeMap ++ bodyMap) +
          ((newValue, path) -> currRanges(x, path)) +
          ((leftExpr, path) -> currRanges(y, path)) +
          ((rightExpr, path) -> currRanges(z, path)) +
          ((Variable(id), emptyPath) -> currRanges((Variable(id), emptyPath))))


      case x @ ArithOperator(Seq(t @ Variable(_)), recons) =>
        val (newExpr, newMap) = recurse(t, path)
        val tmp = recons(Seq(newExpr))
        (tmp, newMap + ((tmp, path) -> currRanges(x, path)))

      case x @ ArithOperator(Seq(y @ Variable(_), z @ Variable(_)), recons) =>
        val (lhsExpr, lhsMap) = recurse(y, path)
        val (rhsExpr, rhsMap) = recurse(z, path)
        val tmp = recons(Seq(lhsExpr, rhsExpr))
        (tmp, (lhsMap ++ rhsMap) + ((tmp, path) -> currRanges(x, path)))

      case x @ IfExpr(cond, thenn, elze) =>
        val (condExpr, condMap) = recurse(cond, path)
        val (thenExpr, thenMap) = recurse(thenn, path :+ cond)
        val (elseExpr, elseMap) = recurse(elze, path :+ lang.TreeOps.negate(cond))
        val tmp = IfExpr(condExpr, thenExpr, elseExpr)

        (tmp, (condMap ++ thenMap ++ elseMap) + ((tmp, path) -> currRanges(x, path)))

      case LessThan(lhs, rhs) =>
        (LessThan(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())

      case GreaterThan(lhs, rhs) =>
        (GreaterThan(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())

      case LessEquals(lhs, rhs) =>
        (LessEquals(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())

      case GreaterEquals(lhs, rhs) =>
        (GreaterEquals(recurse(lhs, path)._1, recurse(rhs, path)._1), Map())


    }
    recurse(expr, emptyPath)
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
    errorFnc: (Map[Identifier, Precision], Precision) => Rational,
    availablePrecisions: Seq[Precision], maxTries: Int = 1000): TypeConfig = {

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
    //val veryLargeRational = Rational(100000l, 1l)
    var numValidConfigs = 0

    // default is the highest precision, since we know that that must work
    var bestCandidate: TypeConfig = ids.map(v => (v -> availablePrecisions.last)).toMap
    //var bestReturnPrec: Precision = availablePrecisions.last
    var bestCost: Rational = costFnc(expr, bestCandidate)

    candidateTypeConfigs.foreach(tpeConfig => {

      val error = errorFnc(tpeConfig, highestPrecision)

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

    bestCandidate
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
      case DoubleDouble => Float64
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
    errorFnc: (Map[Identifier, Precision], Precision) => Rational,
    availablePrecisions: Seq[Precision]): TypeConfig = {

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
            val tmp = errorFnc(tpeConfig, highestPrecision)
            cache += (tpeConfig -> tmp)
            tmp
          }

        val cost = if (error <= errorSpec) {
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

    newTypeConfig
  }

}