package daisy.analysis

import daisy._
import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.TreeOps._
import daisy.lang.Trees
import daisy.lang.Trees.RealLiteral.zero
import daisy.lang.Trees._
import daisy.lang.Types.RealType
import daisy.tools.FinitePrecision.Float64
import daisy.tools.MPFRInterval.maxAbs
import daisy.tools.Rational.sqrtUp
import daisy.tools._

import scala.collection.mutable.ListBuffer


/**
 * This phase computes errors in the form of Taylor abstract.
 */
object TaylorAbstractPhase extends DaisyPhase with DeltaAbstractionUtils with daisy.tools.Taylor {

  override implicit val debugSection = DebugSectionAnalysis
  override val name = "TaylorAbstract"
  override val description = "Computes the Taylor abstract"
  val epsilonIntervalFloat64MPFR = MPFRInterval.+/-(Float64.machineEpsilon)
  val deltaIntervalFloat64MPFR = MPFRInterval.+/-(Float64.denormalsError)

  // Float64.machineEpsilon * Rational(2,1) = 2.220446049250313e-16
  val transEpsInterval = MPFRInterval(MPFRFloat.fromDouble(-2.220446049250313e-16),
    MPFRFloat.fromDouble(2.2204460492503131e-16))

  override def runPhase(ctx: Context, prog: Trees.Program): (Context, Trees.Program) = {


    val epsilonIntervalFloat64 = Interval.+/-(Float64.machineEpsilon)
    val deltaIntervalFloat64 = Interval.+/-(Float64.denormalsError)
    val maxDeviations = 0.7 // gets changed based on experiment

    var modularRes: Map[Identifier, ModularContext] = Map()

    // for saving function derivatives for reuse
    var funcDerivs: Map[Identifier, List[(Identifier, Expr)]] = Map()

    analyzeConsideredFunctions(ctx, prog) { fnc =>

      // println("\nAnalysing function " + fnc.id + "...\n")

      ctx.timers.get("runPhase_TaylorAbstract").start()

      val body = daisy.lang.TreeOps.inline(fnc.body.get)

      if (ctx.hasFlag("modularRoundOffEval")) {

        //  var startTimer1 = System.currentTimeMillis
        // the real approximation (epsilon/delta abstract) of a floating-point expression
        val (deltaEpsAbstract: Expr, transEps, _, roundOffFunEpsMap) =
          epsilonDeltaAbstract(body, denormals = true, abstractVars = false, withAbsErr = false)
        //  var endTimer1 = System.currentTimeMillis


        // var startTimer2 = System.currentTimeMillis        // computing round-off error
        val (firstOrderNonFuncE, firstOrderFuncE, nonFunc2ndOrderTermsWithFuncEps, secondOrderFuncEpsTerms,
        remainderVal, firstOrderWrtDeltasEval) =
          computeModularRoundOff(ctx, prog, fnc, deltaEpsAbstract, transEps, funcDerivs)
        // var endTimer2 = System.currentTimeMillis

        // var startTimer3 = System.currentTimeMillis
        // computing propagation error
        val (propFirstOrderNonFuncDerivs, propFirstOrderFuncDerivs, propSecondOrderTerms) =
          computeModularPopag(fnc, body, funcDerivs)
        // var endTimer3 = System.currentTimeMillis


        // println(s"ph1:fnc ${fnc.id}: abstract: ${endTimer1-startTimer1}, roundoff: ${endTimer2-startTimer2},
        // prop: ${endTimer3-startTimer3}")

        modularRes += (fnc.id ->
          ModularContext(
            roundOffFirstOrderWrtEpsSum = firstOrderNonFuncE,
            roundOffRemainder = remainderVal + firstOrderWrtDeltasEval,
            roundOffFirstOrderWrtFuncEps = firstOrderFuncE,
            roundOffFunEpsMap = roundOffFunEpsMap,
            roundOffRemaindersWrtEpsContainingFuncEps = nonFunc2ndOrderTermsWithFuncEps,
            roundOffRemainderWrtFunEps = secondOrderFuncEpsTerms,
            transcendentalEpsilons = transEps,
            propFirstOder = propFirstOrderNonFuncDerivs,
            propRemainder = propSecondOrderTerms,
            propFirstOrderWrtFun = propFirstOrderFuncDerivs))

        funcDerivs += (fnc.id -> propFirstOrderNonFuncDerivs)
      }

      // ctx has FPTaylor flag on
      else {
        //assert(ctx.hasFlag("FPTaylor"))
        assert(false, "This code is now in FPTaylorPhase")

        // the real approximation (epsilon/delta abstract) of a floating-point expression
        val (deltaEpsAbstract: Expr, transEps, varAbsErrAssoc, _) =
          epsilonDeltaAbstract(body, denormals = true, abstractVars = true, withAbsErr = true)

        val epsilons = epsilonsOf(deltaEpsAbstract).toList
        val deltas = deltasOf(deltaEpsAbstract).toList
        val epsDeltas = deltas ++ epsilons

        // computes the first-order error terms of the round-off error

        // partial derivatives with respect to epsilons
        val firstOrderDerivatives = epsilons.map(eps => (eps, getPartialDerivativeWrtExp(deltaEpsAbstract, eps)))

        // evaluates first-order derivatives by applying absErr vars = 0, deltas = 0 and epsilons = 0
        val evalFirstOrderDerivatives = firstOrderDerivatives.map { case (_, partialDerv) =>
          easySimplify(replaceAbsErrVarsWithZeros(replaceDeltasWithZeros(partialDerv)))
        }.reduceOption((x, y) => Plus(x, y)).getOrElse(RealLiteral(Rational.zero))

        // partial derivatives with respect to absolute initial errors (absErrs)
        val absErrVars = varAbsErrAssoc.values
        val firstOrderAbsErrDerivatives = absErrVars.map(v => (v.id, getPartialDerivativeWrtExp(deltaEpsAbstract, v)))

        // evaluates first-order derivatives by applying absErr vars = 0, deltas = 0 and epsilons = 0
        val evalFirstOrderAbsErrDerivatives = firstOrderAbsErrDerivatives.map { case (id, partialDerv) =>
          (id, easySimplify(replaceAbsErrVarsWithZeros(replaceDeltasWithZeros(partialDerv))))
        }.toList


        // computes one part of the remainder term:
        // 1/2 x sum of (second-order derivatives with respect to deltaIds and epsilonIds) x deltaId x epsilonId

        // (second-order terms with respect to deltaIds and epsilonIds) x deltaId x epsilonId
        val secondOrderTerms = epsDeltas.flatMap(e1 =>
          epsDeltas.map(e2 =>
            Times(Times(secondOrderDerivative(deltaEpsAbstract, getEpsilonDeltaExpr(e1), getEpsilonDeltaExpr(e2),
              funcDerivs), getEpsilonDeltaExpr(e1)), getEpsilonDeltaExpr(e2))))

        // TODO: do simplification immediately on secondOrderDerivatives
        val simplifiedSecondOrderTerms = secondOrderTerms.map(term => easySimplify(term))

        // applying interval analysis on the remainder terms
        val deltaValMap: Map[Identifier, MPFRInterval] = deltas.map(d => d.id -> deltaIntervalFloat64MPFR).toMap

        val epsilonValMap: Map[Identifier, MPFRInterval] = epsilons.map(eps =>
          if (!transEps.contains(eps.id)) {
            eps.id -> epsilonIntervalFloat64MPFR
          } else {
            eps.id -> transEpsInterval
          }).toMap

        // add initial absolute error ranges for absErr vars
        val absErrValMap: Map[Identifier, Interval] = absErrVars.map(v =>
          v.id -> Interval.+/-(ctx.specInitErrors(fnc.id)(varAbsErrAssoc.find(_._2 == v).map(_._1).get.id))).toMap

        val inputValMap: Map[Identifier, MPFRInterval] = ctx.specInputRanges(fnc.id).mapValues(MPFRInterval(_)).toMap ++
          deltaValMap ++ epsilonValMap ++ absErrValMap.mapValues(MPFRInterval(_))

        val remainderTermsEval = simplifiedSecondOrderTerms.map(remainderTerm =>
          evalRange[MPFRInterval](remainderTerm, inputValMap, MPFRInterval.apply)._1)

        val sumOfTerms = remainderTermsEval.reduceOption(_ + _).getOrElse(MPFRInterval.zero)

        val remainderVal = MPFRInterval(Rational(1l, 2l)) * sumOfTerms

        // computing second part of the remainder for initial errors
        val absErrSecondOrderTermsPart1 = epsDeltas.flatMap(e1 =>
          absErrVars.map(e2 =>
            Times(Times(secondOrderDerivative(deltaEpsAbstract, getEpsilonDeltaExpr(e1), e2, funcDerivs),
              getEpsilonDeltaExpr(e1)), RealLiteral(Interval.maxAbs(absErrValMap(e2.id))))))

        // TODO: easySimplify on secondOrderDerivative
        val simplifiedAbsErrSecondOrderTermsPart1 = absErrSecondOrderTermsPart1.map(term => easySimplify(term))

        val absErrSecondOrderTermsPart2 = absErrVars.flatMap(v1 =>
          absErrVars.map(v2 =>
            Times(Times(secondOrderDerivative(deltaEpsAbstract, v1, v2, funcDerivs),
              RealLiteral(Interval.maxAbs(absErrValMap(v1.id)))),
              RealLiteral(Interval.maxAbs(absErrValMap(v2.id))))))

        val simplifiedAbsErrSecondOrderTermsPart2 = absErrSecondOrderTermsPart2.map(term => easySimplify(term))


        val absErrRemainderTermsEvalPart1 = simplifiedAbsErrSecondOrderTermsPart1.map(remainderTerm =>
          evalRange[MPFRInterval](remainderTerm, inputValMap, MPFRInterval.apply)._1)
        val absErrRemainderValP1 = absErrRemainderTermsEvalPart1.reduceOption(_ + _).getOrElse(MPFRInterval.zero)


        val absErrRemainderTermsEvalPart2 = simplifiedAbsErrSecondOrderTermsPart2.map(remainderTerm =>
          evalRange[MPFRInterval](remainderTerm, inputValMap, MPFRInterval.apply)._1)
        val sum2 = absErrRemainderTermsEvalPart2.reduceOption(_ + _).getOrElse(MPFRInterval.zero)
        val absErrRemainderValP2 = MPFRInterval(Rational(1l, 2l)) * sum2


        // adding first-order partial derivative wrt to delta terms to remainder
        val firstOrderDeltaDerivatives = deltas.map(d => (d, getPartialDerivativeWrtExp(deltaEpsAbstract, d)))
        val evalFirstOrderDeltaDerivatives = firstOrderDeltaDerivatives.map { case (id, partialDerv) =>
          (id, easySimplify(replaceAbsErrVarsWithZeros(replaceDeltasWithZeros(partialDerv))))
        }


        val firstOrderDeltaEval = evalFirstOrderDeltaDerivatives.map({ case (termID, termExpr) =>

          val termInterval = evalRange[MPFRInterval](termExpr, inputValMap, MPFRInterval.apply)._1
          (termID, termInterval)
        })

        val sumOfFirstOrderDerivativesDeltaTerms =
          firstOrderDeltaEval.map(_._2).reduceOption(_ + _).getOrElse(MPFRInterval.zero)

        val firstOrderDeltaEvaluated = MPFRInterval(Float64.denormalsError) *
          sumOfFirstOrderDerivativesDeltaTerms


        // compute size of expressions
        val optExprSize = lang.TreeOps.size(evalFirstOrderDerivatives)

        modularRes += (fnc.id ->
          ModularContext(roundOffFirstOrderWrtEpsSum = List(evalFirstOrderDerivatives),
            roundOffRemainder = remainderVal + absErrRemainderValP1 + absErrRemainderValP2 + firstOrderDeltaEvaluated,
            firstOderWrtInitAbsErrs = evalFirstOrderAbsErrDerivatives,
            absErrVarsErrBounds = absErrValMap)
          )

      }
      ctx.timers.get("runPhase_TaylorAbstract").stop()
    }
    (ctx.copy(resultModularAnalysis = modularRes), prog)
  }

  def computeModularRoundOff(ctx: Context, prog: Trees.Program, fnc: FunDef, deltaEpsAbstract: Expr, transEps:
  Seq[Identifier], funcDerivs: Map[Identifier, List[(Identifier, Expr)]])
  :(List[Expr], List[(Epsilon, Expr)], List[Expr], List[(Expr, Expr, Expr)], MPFRInterval, MPFRInterval) = {

    val allEps = epsilonsOf(deltaEpsAbstract).toList
    val (nonFuncEps, funcEps) = allEps.partition(i => !i.id.name.contains("_Func"))
    val deltas = deltasOf(deltaEpsAbstract).toList
    val epsDeltas = deltas ++ allEps

    // var startTimer1 = System.currentTimeMillis
    val (firstOrderNonFuncE, firstOrderFuncE, firstOrderDerivs) =
      computeRoundOffFirstOrderTerms(deltaEpsAbstract, allEps, funcEps, funcDerivs)
    // var endTimer1 = System.currentTimeMillis

    // var startTimer2 = System.currentTimeMillis
    val (nonFunc2ndOrderTermsWithFuncEps, secondOrderFuncEpsTerms, remainderVal, firstOrderWrtDeltasEval) =
      computeRoundOffRemainderTerms(ctx, prog, fnc, deltaEpsAbstract, deltas, epsDeltas, funcEps, nonFuncEps,
        transEps, funcDerivs, firstOrderDerivs)
    // var endTimer2 = System.currentTimeMillis

    // println(s"ph1:fnc ${fnc.id}: firstOrderRoundOff: ${endTimer1-startTimer1},
    // roundoffRemainder: ${endTimer2-startTimer2}")
    (firstOrderNonFuncE, firstOrderFuncE, nonFunc2ndOrderTermsWithFuncEps, secondOrderFuncEpsTerms, remainderVal,
      firstOrderWrtDeltasEval)
  }

  def computeRoundOffFirstOrderTerms(deltaEpsAbstract: Expr, allEps: List[Epsilon], funcEps: List[Epsilon], funcDerivs
  : Map[Identifier, List[(Identifier, Expr)]]): (List[Expr], List[(Epsilon, Expr)], List[(Epsilon, Expr)]) = {


    // first-order derivatives with respect to all epsilons
    var i = 0
    var firstOrderFuncE: ListBuffer[(Epsilon, Expr)] = ListBuffer.empty
    var firstOrderNonFuncE: ListBuffer[(Epsilon, Expr)] = ListBuffer.empty
    val allEpsSize = allEps.size
    while (i < allEpsSize) {
      val eps = allEps(i)
      // partition first-order derivatives to ones wrt non-function epsilons and ones wrt function epsilons
      if (funcEps.contains(eps)) {
        val item = (eps, simplifyDerivative(easySimplify(replaceDeltasWithZeros(
          getPartialDerivativeWrtExp(deltaEpsAbstract, eps, funcDerivs)))))
        firstOrderFuncE += item
      } else {
        val item = (eps, simplifyDerivative(easySimplify(replaceDeltasWithZeros(
          getPartialDerivativeWrtExp(deltaEpsAbstract, eps, funcDerivs)))))
        firstOrderNonFuncE += item
      }

      i = i + 1
    }

    val firstOrderNonFuncEAsList = firstOrderNonFuncE.toList
    val firstOrderFuncEAsList = firstOrderFuncE.toList

    val simplifiedFirstOrderNonFuncE = simplifyMultipleExprs(firstOrderNonFuncEAsList.map(_._2))

    (simplifiedFirstOrderNonFuncE, firstOrderFuncEAsList, firstOrderNonFuncEAsList ++ firstOrderFuncEAsList)

  }

  def computeRoundOffRemainderTerms(ctx: Context, prog: Trees.Program, fnc: FunDef, deltaEpsAbstract: Expr, deltas:
  List[Delta], epsDeltas: List[Expr], funcEps: List[Epsilon], nonFuncEps: List[Epsilon], transEps: Seq[Identifier],
  funcDerivs: Map[Identifier, List[(Identifier, Expr)]], firstOrderDerivs: List[(Epsilon, Expr)])
  : (List[Expr], List[(Expr, Expr, Expr)], MPFRInterval, MPFRInterval) = {

    val zeroExpr = RealLiteral(Rational.zero)

    // second-order derivatives with respect to deltaIds and epsilonIds
    var simplifiedNonFuncSecondOrderTerms: scala.collection.mutable.ListBuffer[Expr] = ListBuffer.empty
    var nonFunc2ndOrderTermsWithFuncEps: scala.collection.mutable.ListBuffer[Expr] = ListBuffer.empty
    val secondOrderFuncEpsTerms: scala.collection.mutable.ListBuffer[(Expr, Expr, Expr)] = ListBuffer.empty
    var i = 0
    val allEps = funcEps ++ nonFuncEps ++ transEps.map(t => Epsilon(t))
    val firstOrderDerivsSize = firstOrderDerivs.size
    while (i < firstOrderDerivsSize) {

      val (e1, deriv) = firstOrderDerivs(i)
      allEps.map({ case e2 =>
        // todo investigate this: only the d(e1)d(e2) case is covered not the d(e2)d(e1)
        val remainderTerm = easySimplify(getPartialDerivativeWrtExp(deriv, e2, funcDerivs))

        if (remainderTerm != zeroExpr) {
          // remainder terms that are not wrt funcEps
          if (!funcEps.contains(e1) && !funcEps.contains(e2)) {

            // separate terms that contain funcEps and are not wrt funcEps
            val funcEpsOfT = epsilonsOf(remainderTerm).filter(e => e.id.name.contains("_Func"))
            if (funcEpsOfT.isEmpty) {
              simplifiedNonFuncSecondOrderTerms += easySimplify(Times(Times(remainderTerm, getEpsilonDeltaExpr(e1)),
                getEpsilonDeltaExpr(e2)))
            } else {
              nonFunc2ndOrderTermsWithFuncEps += easySimplify(Times(Times(remainderTerm, getEpsilonDeltaExpr(e1)),
                getEpsilonDeltaExpr(e2)))
            }
          } else {
            // remainder terms that are at least wrt one funcEps
            // TODO for optimisation we could filter terms not containing funcEps to be evaluated now
            val t: (Expr, Expr, Expr) = (e1, e2, remainderTerm)
            secondOrderFuncEpsTerms += t
          }
        }
      })

      deltas.map({ case dl =>

        val remainderTerm = easySimplify(getPartialDerivativeWrtExp(deriv, dl, funcDerivs))

        if (remainderTerm != zeroExpr) {
          // remainder terms that are not wrt funcEps
          if (!funcEps.contains(e1)) {

            val funcEpsOfT = epsilonsOf(remainderTerm).filter(e => e.id.name.contains("_Func"))
            if (funcEpsOfT.isEmpty) {
              simplifiedNonFuncSecondOrderTerms += easySimplify(Times(Times(remainderTerm, getEpsilonDeltaExpr(e1)),
                getEpsilonDeltaExpr(dl)))
            } else {
              nonFunc2ndOrderTermsWithFuncEps += easySimplify(Times(Times(remainderTerm, getEpsilonDeltaExpr(e1)),
                getEpsilonDeltaExpr(dl)))
            }
          } else {
            // remainder terms that are at least wrt one funcEps
            // TODO for optimisation we could filter terms not containing funcEps to be evaluated now
            val t: (Expr, Expr, Expr) = (e1, dl, remainderTerm)
            secondOrderFuncEpsTerms += t
          }
        }
      })

      i = i + 1
    }

    val restOfRemainderTerms = deltas.flatMap(d1 =>
      epsDeltas.map(e2 =>
        (d1, e2, easySimplify(Times(d1, Times(e2, secondOrderDerivative(deltaEpsAbstract,
          d1, e2, funcDerivs)))))))

    i = 0
    val restOfRemainderTermsSize = restOfRemainderTerms.size
    while (i < restOfRemainderTermsSize) {
      val (d1, e2, term) = restOfRemainderTerms(i)
      if (term != zeroExpr) {
        if (!funcEps.contains(e2)) {
          val funcEpsOfT = epsilonsOf(term).filter(e => e.id.name.contains("_Func"))
          if (funcEpsOfT.isEmpty) {
            simplifiedNonFuncSecondOrderTerms += term
          } else {
            nonFunc2ndOrderTermsWithFuncEps += term
          }
        } else {
          // remainder terms that are at least wrt one funcEps
          // TODO for optimisation we could filter terms not containing funcEps to be evaluated now
          val t: (Expr, Expr, Expr) = (d1, e2, term)
          secondOrderFuncEpsTerms += t
        }
      }
      i = i + 1
    }

    // one single delta, epsilon and transcendental epsilon for evaluating the range
    val sinDelta = FreshIdentifier("sinDel", RealType, alwaysShowUniqueID = true).toDeltaVariable
    val sinEps = FreshIdentifier("sinEps", RealType, alwaysShowUniqueID = true).toEpsilonVariable
    val sinTransEps = FreshIdentifier("sinTransEps", RealType, alwaysShowUniqueID = true).toEpsilonVariable

    val doubleSimplifiedNonFuncSecondOrderTerms = simplifyMultipleExprsForEvalRange(
      simplifiedNonFuncSecondOrderTerms.map(term => {
        val nTerm = lang.TreeOps.replace {
          case Delta(_) => sinDelta
          case Epsilon(id) => if (!transEps.contains(id)) sinEps else sinTransEps
        }(term)
        easySimplify(nTerm)
      }))

    // applying interval analysis on the remainder terms
    val deltaValMap: Map[Identifier, MPFRInterval] = Map(sinDelta.id -> deltaIntervalFloat64MPFR)
    val epsilonValMap: Map[Identifier, MPFRInterval] = Map(sinEps.id -> epsilonIntervalFloat64MPFR,
      sinTransEps.id -> transEpsInterval)

    val inputValMap: Map[Identifier, MPFRInterval] = deltaValMap ++ epsilonValMap ++
      ctx.specInputRanges(fnc.id).mapValues(MPFRInterval(_))

    val remainderTermsEval = doubleSimplifiedNonFuncSecondOrderTerms.map(remainderTerm => {
      evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(remainderTerm, prog),
        inputValMap, MPFRInterval.apply)._1})

    val sumOfTerms = remainderTermsEval.reduceOption(_ + _).getOrElse(MPFRInterval.zero)
    val remainderVal = MPFRInterval(Rational(1l, 2l)) * sumOfTerms

    // adding first-order partial derivative wrt to delta terms to remainder
    i = 0
    val firstOrderDerivWrtDeltaList: ListBuffer[Expr] = ListBuffer.empty
    val deltasSize = deltas.size
    while (i < deltasSize) {
      val d = deltas(i)
      val firstOrderDerivWrtDelta = easySimplify(replaceDeltasWithZeros(
        getPartialDerivativeWrtExp(deltaEpsAbstract, d, funcDerivs)))
      firstOrderDerivWrtDeltaList += firstOrderDerivWrtDelta
      i = i + 1
    }

    val simplifiedFirstOrderDerivWrtDeltaList = simplifyMultipleExprsForEvalRange(firstOrderDerivWrtDeltaList)

    val firstOrderWrtDeltaSum = simplifiedFirstOrderDerivWrtDeltaList.map({ case dTerm =>
      evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(dTerm, prog), inputValMap, MPFRInterval.apply)._1
    }).reduceOption(_ + _).getOrElse(MPFRInterval.zero)

    val firstOrderWrtDeltasEval = firstOrderWrtDeltaSum * MPFRInterval(Float64.denormalsError)

    (nonFunc2ndOrderTermsWithFuncEps.toList, secondOrderFuncEpsTerms.toList, remainderVal, firstOrderWrtDeltasEval)
  }

  def computeModularPopag(fnc: FunDef, body: Expr, funcDerivs: Map[Identifier, List[(Identifier, Expr)]])
  : (List[(Identifier, Expr)], List[(FunctionInvocation, Expr)], List[(Expr, Expr, Expr)]) = {

    val outerFuns = getOuterFuns(body)
    val varIds = allVariablesOf(body)

    // var startTimer1 = System.currentTimeMillis
    val (propFirstOrderNonFuncDerivs, propFirstOrderFuncDerivs) =
      computePropagFirstOrderTerms(body, varIds, outerFuns, funcDerivs)
    // var endTimer1 = System.currentTimeMillis

    // var startTimer2 = System.currentTimeMillis
    val propSecondOrderTerms = computePropagRemainderTerms(body, varIds, outerFuns, funcDerivs,
      propFirstOrderNonFuncDerivs, propFirstOrderFuncDerivs)
    // var endTimer2 = System.currentTimeMillis

    // println(s"ph1:fnc ${fnc.id}: firstOrderpropag: ${endTimer1-startTimer1},
    // propagRemainder: ${endTimer2-startTimer2}")
    (propFirstOrderNonFuncDerivs, propFirstOrderFuncDerivs, propSecondOrderTerms)
  }

  def computePropagFirstOrderTerms(body: Expr, varIds: Set[Identifier], outerFuns: List[FunctionInvocation],
    funcDerivs: Map[Identifier, List[(Identifier, Expr)]])
  : (List[(Identifier, Expr)], List[(FunctionInvocation, Expr)]) = {

    // derivatives wrt variables
    val propFirstOrderNonFuncDerivs = varIds.map(id =>
      (id, simplifyDerivative(easySimplify(getPartialDerivativeWrtExp(body, Variable(id), funcDerivs))))).toList.
      filter(p => p._2 != RealLiteral(0.0))

    // derivatives wrt outermost functions
    val propFirstOrderFuncDerivs = outerFuns.map(func =>
      (func, easySimplify(getPartialDerivativeWrtExp(body, func, funcDerivs)))).
      filter(p => p._2 != RealLiteral(0.0))

    (propFirstOrderNonFuncDerivs, propFirstOrderFuncDerivs)
  }

  def computePropagRemainderTerms(body: Expr, varIds: Set[Identifier], outerFuns: List[FunctionInvocation],
    funcDerivs: Map[Identifier, List[(Identifier, Expr)]], propFirstOrderNonFuncDerivs: List[(Identifier, Expr)],
    propFirstOrderFuncDerivs: List[(FunctionInvocation, Expr)]): List[(Expr, Expr, Expr)] = {

    // (second-order terms with respect to varIds and funcIds)
    val allVarsAndFuncs: Set[Expr] = varIds.map(i => Variable(i)) ++ outerFuns

    val propRemainderTerms =
      (propFirstOrderNonFuncDerivs ++ propFirstOrderFuncDerivs).map({ case (wrt, fOrderTerm) =>
        val wrt1: Expr = wrt match {
          case f: FunctionInvocation => f
          case id: Identifier => Variable(id)
        }
        allVarsAndFuncs.map(wrt2 =>
          (wrt1, wrt2, easySimplify(getPartialDerivativeWrtExp(fOrderTerm, wrt2, funcDerivs))))
      }).flatten.filter(p => p._3 != RealLiteral(0.0))

    propRemainderTerms
  }

  // computes second-order derivative of e with respect to wrt2 and then wrt1
  def secondOrderDerivative(e: Expr, wrt1: Expr, wrt2: Expr, savedDerivs: Map[Identifier, List[(Identifier, Expr)]])
  : Expr = {
    getPartialDerivativeWrtExp(getPartialDerivativeWrtExp(e, wrt2, savedDerivs), wrt1, savedDerivs)
  }

  /** replaces all absErr vars in the expression with zeros */
  def replaceAbsErrVarsWithZeros(expr: Expr): Expr = replace {
    case Variable(id) =>
      if (id.name.startsWith("absErr")) {
        zero
      } else {
        Variable(id)
      }
  }(expr)

  // returns the outermost functions in an expression
  def getOuterFuns(body: Expr): List[FunctionInvocation] = body match {
    case Variable(_) => List()
    case RealLiteral(_) => List()
    case UMinus(in) => getOuterFuns(in)
    case Plus(x, y) => getOuterFuns(x) ++ getOuterFuns(y)
    case Minus(x, y) => getOuterFuns(x) ++ getOuterFuns(y)
    case Times(x, y) => getOuterFuns(x) ++ getOuterFuns(y)
    case Division(x, y) => getOuterFuns(x) ++ getOuterFuns(y)
    case IntPow(x, _) => getOuterFuns(x)
    case Sqrt(x) => getOuterFuns(x)
    case Sin(x) => getOuterFuns(x)
    case Cos(x) => getOuterFuns(x)
    case Tan(x) => getOuterFuns(x)
    case Exp(x) => getOuterFuns(x)
    case Log(x) => getOuterFuns(x)
    case Atan(x) => getOuterFuns(x)
    case f@FunctionInvocation(_, _, _, _) => List(f)
    case z => throw new IllegalArgumentException(s"Unknown expression $z")
  }

  // In a list of Exprs reduces repeated exprs into a single multiplied Expr
  // also removes opposite exprs e.g. e and UMinus(e)
  def simplifyMultipleExprs(derivs: List[Expr]): List[Expr] = {

    // UMinus(e) and e both exist
    val uxe = derivs.filter(e => !e.isInstanceOf[UMinus]).filter(e => derivs.contains(UMinus(e)))

    val grouped = derivs.groupBy(identity)

    val simplifiedList =
      if ((derivs.distinct.size != derivs.size) || uxe.nonEmpty) {

        grouped.collect { case (x, ys) => x match {
          case UMinus(e) =>
            if (!grouped.contains(e)) {
              Times(RealLiteral(ys.size), x)
            }
            else {
              RealLiteral(Rational.zero)
            }

          case _ =>
            if (grouped.contains(UMinus(x))) {
              val uys = grouped(UMinus(x)).size
              if (ys.size > uys) {
                Times(RealLiteral(ys.size - uys), x)
              }
              else if (ys.size < uys) {
                Times(RealLiteral(uys - ys.size), UMinus(x))
              }
              else {
                RealLiteral(Rational.zero)
              }
            }
            else {
              Times(RealLiteral(ys.size), x)
            }
        }
        }.toList.filter(e => e != RealLiteral(Rational.zero)) // removes zeros from the list
      }
      else {
        derivs
      }
    // replaces 1 * expr with expr
    simplifiedList.map(x => {
      x match {
        case Times(RealLiteral(Rational.one), ex) => ex
        case _ => x
      }
    })
  }

  // In a list of Exprs reduces repeated exprs into a single multiplied Expr
  // It does not remove opposite exprs e.g. e and UMinus(e)
  def simplifyMultipleExprsForEvalRange(derivs: ListBuffer[Expr]): List[Expr] = {

    val grouped = derivs.groupBy(identity)

    val simplifiedList = {
      if (derivs.distinct.size != derivs.size) {
        grouped.collect { case (x, ys) =>
          Times(RealLiteral(ys.size), x)
        }
      }
      else {
        derivs
      }
    }.toList.filter(e => e != RealLiteral(Rational.zero))
    // replaces 1 * expr with expr
    simplifiedList.map(x => {
      x match {
        case Times(RealLiteral(Rational.one), ex) => ex
        case _ => x
      }
    })
  }

  // turns the derivative (consisting of Plus exprs) into list of exprs
  def turnExprIntoList(deriv: Expr): List[Expr] = deriv match {
    case Plus(lhs, rhs) => turnExprIntoList(lhs) ++ turnExprIntoList(rhs)
    case _ => deriv :: List()
  }

  // turns the derivative (consisting of Plus exprs) into list of exprs and then
  // applies simplification on the list
  def simplifyDerivative(deriv: Expr): Expr = {

    val dParts: List[Expr] = turnExprIntoList(deriv)
    val simpleDeriv = simplifyMultipleExprs(dParts)
    simpleDeriv.reduceOption((x, y) => Plus(x, y)).getOrElse(RealLiteral(Rational.zero))
  }

  // TODO handle function calls + add to round-off and propagation error computation
  // moves small first-order terms to remainder
  def moveToRemainder2(exprToShrink: Expr, maxDeviations: Rational, funcInputRanges: Map[Identifier, Interval])
  : (Expr, MPFRInterval, String) = {

    // computing intermediate ranges
    val epsilonIds = epsilonsOf(exprToShrink).map(epsilonExp => epsilonExp.id).toList
    val deltasIds = deltasOf(exprToShrink).map(deltaExp => deltaExp.id).toList
    val deltaValMap: Map[Identifier, MPFRInterval] = deltasIds.map(id => id -> deltaIntervalFloat64MPFR).toMap
    val epsilonValMap: Map[Identifier, MPFRInterval] = epsilonIds.map(id => id -> epsilonIntervalFloat64MPFR).toMap
    var addedToRemainder: MPFRInterval = MPFRInterval.zero
    val removedTerms: ListBuffer[Expr] = ListBuffer()

    val inputValMap: Map[Identifier, MPFRInterval] = funcInputRanges.mapValues(MPFRInterval(_)).toMap ++ deltaValMap ++
      epsilonValMap

    val derivsList = turnExprIntoList(exprToShrink).map(e =>
      (e, evalRange[MPFRInterval](e, inputValMap, MPFRInterval.apply)._1))
      .sortWith((a, b) => maxAbs(a._2) < maxAbs(b._2))

    val sumList = derivsList.map(_._2).reduceOption((a, b) => a + b).getOrElse(MPFRInterval.zero)
    val mean = Interval.maxAbs(sumList.toInterval) / derivsList.length
    val distanceFromMean: Map[Expr, Rational] = derivsList.map(x =>
      (x._1, Rational.abs(Interval.maxAbs(x._2.toInterval) - mean))).toMap
    val standardDeviation = sqrtUp(distanceFromMean.map(x => x._2 * x._2).
      reduceOption((a, b) => a + b).getOrElse(Rational.zero) / derivsList.length)

    // not_outlier = distance_from_mean < maxDeviations * standard_deviation
    derivsList.partition(e => (distanceFromMean(e._1) >= (maxDeviations * standardDeviation)) &&
      Interval.maxAbs(e._2.toInterval) < mean)._1
      .map({ case (t, r) =>
        addedToRemainder += r
        removedTerms += t
      })

    val replacementReport = removedTerms.size + "/" + derivsList.size

    val optExpr = derivsList.filter(d => !removedTerms.contains(d._1)).map(t => t._1).
      reduceOption((x, y) => Plus(x, y)).getOrElse(RealLiteral(Rational.zero))

    (optExpr, addedToRemainder, replacementReport)
  }
}
