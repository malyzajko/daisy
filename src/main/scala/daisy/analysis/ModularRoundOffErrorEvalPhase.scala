// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.analysis

import daisy._
import daisy.analysis.FunctionCallPhase.inlineFncCalls
import daisy.lang.Identifiers.Identifier
import daisy.lang.TreeOps.{deltasOf, epsilonsOf}
import daisy.lang.Trees.{Delta, Epsilon, Expr, FunctionInvocation, RealLiteral, ValDef, Variable}
import daisy.lang.{Identifiers, Trees}
import daisy.tools.FinitePrecision.Float64
import daisy.tools._
import sys.process._



/**
 * This phase evaluates the modular round-off error approach
 */
object ModularRoundOffErrorEvalPhase extends DaisyPhase with DeltaAbstractionUtils with IntervalBranchAndBound {

  override implicit val debugSection = DebugSectionAnalysis
  override val name = "modularRoundOffEval"
  override val description = "evaluates modular round-off error computation"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    FlagOption(
      "shorterInterval",
      "using the shorter inner interval for modular round-off error evaluation"),
    FlagOption(
      "bb",
      "using branch and bound algorithm for range evaluation"),
    FlagOption(
      "gelpia",
      "using gelpia for range evaluation"),
    FlagOption(
      "checkInterval",
      "validates the ranges of the arguments of the called function")
  )

  val minWidth = MPFRFloat.fromDouble(0.1) // for Branch and Bound
  val maxSplits: Int = 10000 // for Branch and Bound
  val epsilonIntervalFloat64MPFR = MPFRInterval.+/-(Float64.machineEpsilon)
  val deltaIntervalFloat64MPFR = MPFRInterval.+/-(Float64.denormalsError)

  // Float64.machineEpsilon * Rational(2,1) = 2.220446049250313e-16
  val transEpsInterval = MPFRInterval(MPFRFloat.fromDouble(-2.220446049250313e-16),
    MPFRFloat.fromDouble(2.2204460492503131e-16))


  val roundOffErrFuncEval: collection.mutable.Map[(Identifier, Map[Identifier, Interval], Map[Expr, Expr]),
    Rational] = collection.mutable.Map()

  // for save and reuse of computation results
  val propagErrFuncEval: collection.mutable.Map[(Identifier, Map[Identifier, Interval], Map[Expr, Expr]),
    Rational] = collection.mutable.Map()

  override def runPhase(ctx: Context, prog: Trees.Program): (Context, Trees.Program) = {

    val useBandB = ctx.hasFlag("bb")
    val useShorterInterval = ctx.hasFlag("shorterInterval")
    val useGelpia = ctx.hasFlag("gelpia")
    val checkInterval = ctx.hasFlag("checkInterval")

    val res: Map[Identifier, Rational] = analyzeConsideredFunctions(ctx, prog) { fnc =>

      ctx.reporter.info(s"using B&B for range evaluation: $useBandB," +
        s"using shorter input intervals: $useShorterInterval")

      ctx.reporter.info(s"using Gelpia for range evaluation: $useGelpia")

      val inputVarsRanges = if (!useShorterInterval) {
        ctx.specInputRanges(fnc.id)
      } else {
        ctx.specInputRanges(fnc.id).map { case (id: Identifier, i: Interval) =>

          val quartile = (i.xhi - i.xlo) / Rational(4)
          (id, Interval(i.xlo + quartile, i.xhi - quartile))

          /* val quartile = (i.xhi - i.xlo) / Rational(20)
           (id, Interval(i.xlo + (quartile * 9), i.xhi - (quartile * 9))) */
        }
      }

      // println("analyzed function: " + fnc.id)

      // getting information of all called functions
      val calledFuncsEpsMap = ctx.resultModularAnalysis.keys.
        map({ case f =>
          ctx.resultModularAnalysis(f).roundOffFunEpsMap
        }).flatten.toMap

      // var startTimer1 = System.currentTimeMillis
      val roundOffErr = evalRoundOffErrFunc(ctx, prog, fnc.id, inputVarsRanges, Map(), calledFuncsEpsMap, useBandB,
        useGelpia, checkInterval)
      // var endTimer1 = System.currentTimeMillis

      // var startTimer2 = System.currentTimeMillis
      val propagationErr = evalPropagationErrFunc(ctx, prog, fnc.id, fnc.id, inputVarsRanges, fnc.params
        .map({ x: ValDef => Variable(x.id) }).zip(fnc.params.map({ x: ValDef => Variable(x.id) })).toMap, useBandB, useGelpia)
      // var endTimer2 = System.currentTimeMillis

      // println("propagation Err: " + propagationErr)
      // println("roundOff Err: " + roundOffErr)

      //  println(s"ph2:fnc ${fnc.id}: roundoff: ${endTimer1-startTimer1}, propag: ${endTimer2-startTimer2}")
      roundOffErr + propagationErr
    }

    (ctx.copy(resultAbsoluteErrors = res), prog)
  }

  def evalRoundOffErrFunc(ctx: Context, prog: Trees.Program, funId: Identifiers.Identifier,
    inputVarRanges: Map[Identifier, Interval], params2args: Map[Expr, Expr],
    calledFuncsEpsMap: Map[Epsilon, FunctionInvocation], useBandB: Boolean, useGelpia: Boolean, checkInterval: Boolean)
    : Rational = {

    // println("round-off error analysis of: " + funId + " with params2args mapping: " + params2args)

    val reused = fetchSavedResults(funId, inputVarRanges, params2args, roundOffErrFuncEval.toMap )
    if(reused != None) {
      // println("###### reused round-off results ######")
      reused.get
    }
    else {
      if (checkInterval) {
        intervalCheck(ctx, prog, funId)
      }

      // var startTimer1 = System.currentTimeMillis
      // evaluate first-order derivatives
      val roundOffFirstOrderEval = evalRoundOffFirstOrderTerms(ctx, prog, funId, inputVarRanges, params2args,
        calledFuncsEpsMap, useBandB, useGelpia, checkInterval)
      // var endTimer1 = System.currentTimeMillis


      // var startTimer2 = System.currentTimeMillis
      // evaluate remainder terms
      val roundOffRemainderEval = evalRoundOffRemainderTerms(ctx, prog, funId, inputVarRanges, params2args,
        calledFuncsEpsMap, useBandB, useGelpia, checkInterval)
      //  var endTimer2 = System.currentTimeMillis

      // println(s"ph2:fnc ${funId}: roundOffFirstOrder: ${endTimer1-startTimer1},
      // roundoffRemainder: ${endTimer2-startTimer2}")

      val modularValue = roundOffFirstOrderEval + roundOffRemainderEval

      roundOffErrFuncEval += (funId, inputVarRanges, params2args) -> modularValue

      modularValue
    }
  }

  def evalRoundOffFirstOrderTerms(ctx: Context, prog: Trees.Program, funId: Identifiers.Identifier,
    inputVarRanges: Map[Identifier, Interval], params2args: Map[Expr, Expr],
    calledFuncsEpsMap: Map[Epsilon, FunctionInvocation], useBandB: Boolean, useGelpia: Boolean, checkInterval: Boolean)
  : Rational = {

    val transEps = ctx.resultModularAnalysis(funId).transcendentalEpsilons
    val roundOffFirstOrderDerivs = ctx.resultModularAnalysis(funId).roundOffFirstOrderWrtEpsSum

    var i = 0
    var sum = Rational.zero
    val roundOffFirstOrderDerivsSize = roundOffFirstOrderDerivs.size
    while (i < roundOffFirstOrderDerivsSize) {
      val varsRenamed = lang.TreeOps.replace(params2args)(roundOffFirstOrderDerivs(i))

      // inlining function calls
      val funcCallReplaced = FunctionCallPhase.inlineFncCalls(varsRenamed, prog)

      // evaluating using interval analysis or  B&B or Gelpia
      // the first-order derivatives wrt non-Function epsilons
      val (nonFuncEps, funcEps) = epsilonsOf(funcCallReplaced).partition(i => !i.id.name.contains("_Func"))
      val nonFuncEpsValMap: Map[Identifier, MPFRInterval] = nonFuncEps.map(_.id).map(eps =>
        if (!transEps.contains(eps)) {
          eps -> epsilonIntervalFloat64MPFR
        } else {
          eps -> transEpsInterval
        }).toMap

      val funcEpsValMap: Map[Identifier, MPFRInterval] =
        computeFuncEpsValMap(ctx, prog, funcEps, inputVarRanges, calledFuncsEpsMap, params2args, useBandB, useGelpia,
          checkInterval)
      val inputValMap: Map[Identifier, MPFRInterval] = nonFuncEpsValMap ++ funcEpsValMap ++
        inputVarRanges.mapValues(MPFRInterval(_))

      val termInterval = if (useGelpia) {
        applyGelpia(funcCallReplaced, inputValMap)
          .toInterval
      } else if (!useBandB) {
        evalRange[MPFRInterval](funcCallReplaced, inputValMap, MPFRInterval.apply)._1.toInterval

      } else {
        applyBandB(funcCallReplaced, inputValMap, minWidth, minWidth, maxSplits).toInterval
      }
      sum = sum + Interval.maxAbs(termInterval)
      i = i + 1
    }
    val roundOffFirstOrderEval = sum * Float64.machineEpsilon


    // evaluating first-order round-off derivative terms wrt function epsilons
    // and then multiplying evaluated terms by their respective round-off error function
    val roundOffFirstOrderWrtFuncEps = ctx.resultModularAnalysis(funId).roundOffFirstOrderWrtFuncEps

    i = 0
    var roundOffFirstOrderWrtFuncEpsEval = Rational.zero
    val roundOffFirstOrderWrtFuncEpsSize = roundOffFirstOrderWrtFuncEps.size
    while (i < roundOffFirstOrderWrtFuncEpsSize) {
      val (ep, t) = roundOffFirstOrderWrtFuncEps(i)
      val varsRenamed = lang.TreeOps.replace(params2args)(t)
      val (nonFuncEps, funcEps) = epsilonsOf(varsRenamed).partition(i => !i.id.name.contains("_Func"))
      val nonFuncEpsValMap = nonFuncEps.map(_.id).map(eps =>
        if (!transEps.contains(eps)) {
          eps -> epsilonIntervalFloat64MPFR
        } else {
          eps -> transEpsInterval
        }).toMap
      val funcEpsValMap: Map[Identifier, MPFRInterval] =
        computeFuncEpsValMap(ctx, prog, funcEps, inputVarRanges, calledFuncsEpsMap, params2args, useBandB, useGelpia,
          checkInterval)
      val inputValMap: Map[Identifier, MPFRInterval] = nonFuncEpsValMap ++ funcEpsValMap ++
        inputVarRanges.mapValues(MPFRInterval(_))

      val termInterval = if (useGelpia) {
        applyGelpia(FunctionCallPhase.inlineFncCalls(varsRenamed, prog), inputValMap).toInterval
      } else if (!useBandB) {
        evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(varsRenamed, prog), inputValMap,
          MPFRInterval.apply)._1.toInterval
      } else {
        applyBandB(FunctionCallPhase.inlineFncCalls(varsRenamed, prog),
          inputValMap, minWidth, minWidth, maxSplits).toInterval
      }
      // evaluating related function epsilon
      val nestedFunc = calledFuncsEpsMap(ep)
      val newParams2Args: Map[Expr, Expr] = nestedFunc.params.map({ x: ValDef => Variable(x.id) })
        .zip(nestedFunc.args.map({ case e => lang.TreeOps.replace(params2args)(e) })).toMap
      roundOffFirstOrderWrtFuncEpsEval = roundOffFirstOrderWrtFuncEpsEval +
        (evalRoundOffErrFunc(ctx, prog, nestedFunc.fdId, inputVarRanges, newParams2Args, calledFuncsEpsMap, useBandB,
          useGelpia, checkInterval) * Interval.maxAbs(termInterval))
      i = i + 1
    }

    roundOffFirstOrderEval + roundOffFirstOrderWrtFuncEpsEval
  }

  def evalRoundOffRemainderTerms(ctx: Context, prog: Trees.Program, funId: Identifiers.Identifier,
    inputVarRanges: Map[Identifier, Interval], params2args: Map[Expr, Expr],
    calledFuncsEpsMap: Map[Epsilon, FunctionInvocation], useBandB: Boolean, useGelpia: Boolean, checkInterval: Boolean)
  : Rational = {
    // remainders wrt non-Function epsilons
    val roundOffRemainderWrtNonFuncEps = Interval.maxAbs(ctx.resultModularAnalysis(funId).roundOffRemainder.toInterval)

    val transEps = ctx.resultModularAnalysis(funId).transcendentalEpsilons

    // remainders wrt function epsilons
    val roundOffRemainderWrtFuncEps = ctx.resultModularAnalysis(funId).roundOffRemainderWrtFunEps.
      map({ case t => (t._1, t._2, lang.TreeOps.replace(params2args)(t._3)) })

    // remainder terms that are wrt normal epsilons and contain function epsilons
    val roundOffRemaindersWrtEpsWithFuncEps = ctx.resultModularAnalysis(funId).roundOffRemaindersWrtEpsContainingFuncEps.
      map(t => lang.TreeOps.replace(params2args)(t))

    // evaluating remainders wrt function epsilons
    val roundOffRemainderWrtFuncEpsEval = roundOffRemainderWrtFuncEps.map { case t =>
      val (nonFuncEps, funcEps) = epsilonsOf(t._3).partition(i => !i.id.name.contains("_Func"))
      val nonFuncEpsValMap = nonFuncEps.map(_.id).map(eps =>
        if (!transEps.contains(eps)) {
          eps -> epsilonIntervalFloat64MPFR
        } else {
          eps -> transEpsInterval
        }).toMap
      val deltasIds = deltasOf(t._3).map(deltaExp => deltaExp.id).toList
      val funcEpsValMap: Map[Identifier, MPFRInterval] =
        computeFuncEpsValMap(ctx, prog, funcEps, inputVarRanges, calledFuncsEpsMap, params2args, useBandB, useGelpia,
          checkInterval)
      val deltaValMap: Map[Identifier, MPFRInterval] = deltasIds.map(id => id -> deltaIntervalFloat64MPFR).toMap
      val inputValMap: Map[Identifier, MPFRInterval] =  nonFuncEpsValMap ++ deltaValMap ++ funcEpsValMap ++
        inputVarRanges.mapValues(MPFRInterval(_))

      val termInterval = Interval.maxAbs(
        if (useGelpia) {
        applyGelpia(FunctionCallPhase.inlineFncCalls(t._3, prog), inputValMap).toInterval
      } else if (!useBandB) {
        evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(t._3, prog), inputValMap, MPFRInterval.apply)._1.toInterval
      } else {
        applyBandB(FunctionCallPhase.inlineFncCalls(t._3, prog), inputValMap, minWidth, minWidth, maxSplits).toInterval
      })

      // evaluating potential function epsilons
      val epsIntervals = List(t._1, t._2).map { case e1 =>
        e1 match {

          case e@Epsilon(id) =>
            // if the second-order derivative is wrt a function epsilon
            if (id.name.contains("_Func")) {
              val nestedFunc = calledFuncsEpsMap(e)
              val newParams2Args: Map[Expr, Expr] = nestedFunc.params.map({ x: ValDef => Variable(x.id) })
                .zip(nestedFunc.args.map({ case e => lang.TreeOps.replace(params2args)(e) })).toMap

              evalRoundOffErrFunc(ctx, prog, nestedFunc.fdId, inputVarRanges, newParams2Args, calledFuncsEpsMap,
                useBandB, useGelpia, checkInterval)
            }
            // if the second-order derivative is wrt a normal epsilon
            else {
              Float64.machineEpsilon
            }
          // if the second-order derivative is wrt a delta
          case Delta(_) => Float64.denormalsError
        }
      }
      epsIntervals(0) * epsIntervals(1) * termInterval
    }.foldLeft(Rational.zero)(_ + _) / 2

    // evaluating remainder terms that are wrt normal epsilons and contain function epsilons
    val roundOffRemaindersWrtEpsWithFuncEpsEval = roundOffRemaindersWrtEpsWithFuncEps.map({ case t =>
      val (nonFuncEps, funcEps) = epsilonsOf(t).partition(i => !i.id.name.contains("_Func"))
      val deltasIds = deltasOf(t).map(_.id).toList
      val nonFuncEpsValMap = nonFuncEps.map(_.id).map(eps =>
        if (!transEps.contains(eps)) {
          eps -> epsilonIntervalFloat64MPFR
        } else {
          eps -> transEpsInterval
        }).toMap
      val deltaValMap: Map[Identifier, MPFRInterval] =
        deltasIds.map(id => id -> deltaIntervalFloat64MPFR).toMap
      val funcEpsValMap: Map[Identifier, MPFRInterval] =
        computeFuncEpsValMap(ctx, prog, funcEps, inputVarRanges, calledFuncsEpsMap, params2args, useBandB, useGelpia,
          checkInterval)
      val inputValMap: Map[Identifier, MPFRInterval] =  nonFuncEpsValMap ++ funcEpsValMap ++ deltaValMap ++
        inputVarRanges.mapValues(MPFRInterval(_))

      Interval.maxAbs(if (useGelpia) {
        applyGelpia(FunctionCallPhase.inlineFncCalls(t, prog), inputValMap).toInterval
      } else if (!useBandB) {
        evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(t, prog), inputValMap,
          MPFRInterval.apply)._1.toInterval
      } else {
        applyBandB(FunctionCallPhase.inlineFncCalls(t, prog), inputValMap, minWidth, minWidth, maxSplits).toInterval
      })
    }).foldLeft(Rational.zero)(_ + _) / 2

    roundOffRemainderWrtNonFuncEps + roundOffRemainderWrtFuncEpsEval + roundOffRemaindersWrtEpsWithFuncEpsEval
  }


  def computeFuncEpsValMap(ctx: Context, prog: Trees.Program, funcEps: Set[Trees.Epsilon],
    inputVarRanges: Map[Identifiers.Identifier, Interval], calledFuncsEpsMap: Map[Trees.Epsilon, Trees.FunctionInvocation],
    params2args: Map[Expr, Expr], useBandB: Boolean, useGelpia: Boolean, checkInterval: Boolean): Map[Identifier, MPFRInterval] = {

    funcEps.map({ e =>
      val funcEnv = calledFuncsEpsMap(e)
      val funcEpsParams2Args: Map[Expr, Expr] = {
        funcEnv.params.map({ x: ValDef => Variable(x.id) })
          .zip(funcEnv.args.map({ case e => lang.TreeOps.replace(params2args)(e) })).toMap
      }
      e.id -> MPFRInterval(
        evalRoundOffErrFunc(ctx, prog, funcEnv.fdId, inputVarRanges, funcEpsParams2Args, calledFuncsEpsMap, useBandB,
          useGelpia, checkInterval))
    }).toMap
  }

  def evalPropagationErrFunc(ctx: Context, prog: Trees.Program, origFun: Identifiers.Identifier,
    termId: Identifiers.Identifier, inputVarRanges: Map[Identifier, Interval], params2args: Map[Expr, Expr],
    useBandB: Boolean, useGelpia: Boolean): Rational = {

    // println("propagation error analysis of: " + termId + " with params2args mapping: " + params2args)

    // reading initial errors
    val initErrs = ctx.specInputErrors(origFun)

    // reusing the already computed analysis results
    val reused = fetchSavedResults(termId, inputVarRanges, params2args, propagErrFuncEval.toMap)
    if (reused != None) {
      // println("###### reused propagation results ######")
      reused.get
    }
    // if termId is id of a parameter that is mapped to a variable as argument
    else if (!ctx.resultModularAnalysis.contains(termId)) {
      // save to reuse analysis results
      propagErrFuncEval += (termId, inputVarRanges, params2args) -> initErrs(termId)
      initErrs(termId)
    }
    else {
      // var startTimer1 = System.currentTimeMillis
      // evaluating propagation first-order terms
      val propagFirstOrderEval =
        evalPropagFirstOrderTerms(ctx, prog, origFun, termId, inputVarRanges, params2args, useBandB, useGelpia)
      // var endTimer1 = System.currentTimeMillis

      // var startTimer2 = System.currentTimeMillis
      // evaluating propagation remainder terms
      val propRemainderTermsEval =
        evalPropagRemainderTerms(ctx, prog, origFun, termId, inputVarRanges, params2args, useBandB, useGelpia)
      // var endTimer2 = System.currentTimeMillis

      val modularValue = propagFirstOrderEval + propRemainderTermsEval

      // println(s"ph2:fnc ${origFun}: propagFirstOrder: ${endTimer1-startTimer1},
      // propagRemainder: ${endTimer2-startTimer2}")

      // save to reuse analysis results
      propagErrFuncEval += (termId, inputVarRanges, params2args) ->
        modularValue

      modularValue
    }
  }

  def evalPropagFirstOrderTerms(ctx: Context, prog: Trees.Program, origFun: Identifiers.Identifier,
    termId: Identifiers.Identifier, inputVarRanges: Map[Identifier, Interval], params2args: Map[Expr, Expr],
    useBandB: Boolean, useGelpia: Boolean): Rational = {

    // evaluating first-order terms wrt variables
    val firstOrderNonFuncPropagDerivs = ctx.resultModularAnalysis(termId).propFirstOder
    val inputValMap: Map[Identifier, MPFRInterval] = inputVarRanges.mapValues(MPFRInterval(_)).toMap

    var i = 0
    var firstOrderNonFuncPropagDerivsEval = Rational.zero
    val firstOrderNonFuncPropagDerivsSize = firstOrderNonFuncPropagDerivs.size
    while (i < firstOrderNonFuncPropagDerivsSize) {

      val (id, t) = firstOrderNonFuncPropagDerivs(i)
      val varReplaced = lang.TreeOps.replace(params2args)(t)
      val termInterval = Interval.maxAbs(
        if (useGelpia) {
          applyGelpia(FunctionCallPhase.inlineFncCalls(varReplaced, prog), inputValMap)
            .toInterval
        } else if (!useBandB) {
          evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(varReplaced, prog), inputValMap,
            MPFRInterval.apply)._1.toInterval
        } else {
          applyBandB(FunctionCallPhase.inlineFncCalls(varReplaced, prog), inputValMap,
            minWidth, minWidth, maxSplits).toInterval
        })

      val a = params2args(Variable(id))
      val termEval = a match {
        case RealLiteral(r) =>
          termInterval * evalLiteralErr(r)

        // if there is a function as input argument
        case Variable(i) =>
          termInterval * evalPropagationErrFunc(ctx, prog, origFun, i, inputVarRanges, Map(), useBandB, useGelpia)

        // if there is a function as input argument
        case FunctionInvocation(i, p, a, _) =>
          termInterval *
            evalPropagationErrFunc(ctx, prog, origFun, i, inputVarRanges,
              params2args ++ genNewMapping(p, a, params2args), useBandB, useGelpia)
      }

      firstOrderNonFuncPropagDerivsEval = firstOrderNonFuncPropagDerivsEval + termEval
      i = i + 1
    }

    // evaluating first-order derivatives wrt functions
    val propagFirstOrderFuncTerms = ctx.resultModularAnalysis(termId).propFirstOrderWrtFun

    i = 0
    var propagFirstOrderFuncEEval = Rational.zero
    val propagFirstOrderFuncTermsSize = propagFirstOrderFuncTerms.size
    while (i < propagFirstOrderFuncTermsSize) {
      val (id, t) = propagFirstOrderFuncTerms(i)
      val varsReplaced = lang.TreeOps.replace(params2args)(t)

      // evaluate propagation derivatives wrt functions and multiply it by the related error function
      val termInterval = if (useGelpia) {
        applyGelpia(FunctionCallPhase.inlineFncCalls(varsReplaced, prog), inputValMap).toInterval
      } else if (!useBandB) {
        evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(varsReplaced, prog),
          inputValMap, MPFRInterval.apply)._1.toInterval
      } else {
        applyBandB(FunctionCallPhase.inlineFncCalls(varsReplaced, prog), inputValMap,
          minWidth, minWidth, maxSplits).toInterval
      }

      val termEval = Interval.maxAbs(termInterval) *
        evalPropagationErrFunc(ctx, prog, origFun, id.fdId, inputVarRanges,
          params2args ++ genNewMapping(id.params, id.args, params2args), useBandB, useGelpia)

      propagFirstOrderFuncEEval = propagFirstOrderFuncEEval + termEval
      i = i + 1
    }

    firstOrderNonFuncPropagDerivsEval + propagFirstOrderFuncEEval
  }

  def evalPropagRemainderTerms(ctx: Context, prog: Trees.Program, origFun: Identifiers.Identifier,
    termId: Identifiers.Identifier, inputVarRanges: Map[Identifier, Interval], params2args: Map[Expr, Expr],
    useBandB: Boolean, useGelpia: Boolean): Rational = {

    val propagRemainder = ctx.resultModularAnalysis(termId).propRemainder
      .map({ case (id1, id2, t) => (id1, id2, lang.TreeOps.replace(params2args)(t)) })
    // applying interval analysis on the remainder terms
    val inputValMap: Map[Identifier, MPFRInterval] = inputVarRanges.mapValues(MPFRInterval(_)).toMap
    val propRemainderTermsEval = propagRemainder.map({ case (wrt1, wrt2, term) =>
      val termInterval = evalRange[MPFRInterval](FunctionCallPhase.inlineFncCalls(term, prog),inputValMap,
        MPFRInterval.apply)._1

      Interval.maxAbs(termInterval.toInterval) *
        CallPropagWithNewArgs(wrt1, params2args, ctx, prog, origFun, inputVarRanges, useBandB, useGelpia) *
        CallPropagWithNewArgs(wrt2, params2args, ctx, prog, origFun, inputVarRanges, useBandB, useGelpia)
    }).reduceOption(_ + _).getOrElse(Rational.zero) * Rational(1l, 2l)
    propRemainderTermsEval
  }

  // check if the input ranges are inside the bounds of the input bound conditions of called functions
  def intervalCheck(ctx: Context, prog: Trees.Program, funId: Identifiers.Identifier): Unit = {
    val calledFuncEpsMap = ctx.resultModularAnalysis(funId).roundOffFunEpsMap
    val inputVarRanges = ctx.specInputRanges(funId)
    val transEps = ctx.resultModularAnalysis(funId).transcendentalEpsilons

    calledFuncEpsMap.map { case (_, f) =>
      val calledFuncId = f.fdId
      val args = f.args
      val args2Params = args.zip(f.params).toMap
      val calledFunInputVarRanges = ctx.specInputRanges(calledFuncId)
      args.map { arg => // TODO if arg is function call? for now function calls are inlined
        val epsilonIds = epsilonsOf(arg).map(epsilonExp => epsilonExp.id).toList
        val deltasIds = deltasOf(arg).map(deltaExp => deltaExp.id).toList
        val epsilonValMap = epsilonIds.map(eps =>
          if (!transEps.contains(eps)) {
            eps -> epsilonIntervalFloat64MPFR
          } else {
            eps -> transEpsInterval
          }).toMap
        val deltaValMap: Map[Identifier, MPFRInterval] = deltasIds.map(id => id -> deltaIntervalFloat64MPFR).toMap
        val inputValMap: Map[Identifier, MPFRInterval] = epsilonValMap ++ deltaValMap ++
          inputVarRanges.mapValues(MPFRInterval(_))
        val argEval = evalRange[MPFRInterval](inlineFncCalls(arg, prog), inputValMap, MPFRInterval.apply)._1.toInterval
        // TODO add evalRoundoff and add to argEval
        /* val argErr = evalRoundoff[Interval](inlineFncCalls(arg, prog), intermediateRanges,
          ctx.specInputPrecisions(fnc.id), ctx.specInputErrors(fnc.id).mapValues(Interval.+/-).toMap,
          zeroError = Interval.zero, fromError = Interval.+/-, interval2T = Interval.apply,
          constantsPrecision = ctx.option[Precision]("precision"), trackRoundoffErrs) */

        val origInputBounds = calledFunInputVarRanges(args2Params(arg).id)
        if (argEval.xlo.toDouble < origInputBounds.xlo.toDouble ||
          argEval.xhi.toDouble > origInputBounds.xhi.toDouble) {
          // TODO toDouble is wrong. we need to add the error. for now we assume the intervals are big enough
          ctx.reporter.fatalError("the input intervals are not inside the input intervals of called functions:" +
            " the function: " + calledFuncId + ", the variable: " + args2Params(arg).id +
            ", arg bounds: " + argEval + ", original input bounds: " + origInputBounds)
        }
      }
    }
  }

  def genNewMapping(params: Seq[ValDef], args: Seq[Expr], p2a: Map[Expr, Expr])
  : Map[Expr, Expr] = {
    val m: Map[Expr, Expr] = params.map({ x: ValDef => Variable(x.id) }).zip(args).toMap
    val m2 = m.map({ case (v, e) =>
      e match {
        case FunctionInvocation(f, p, argss, r) =>
          val newArgs = argss.map({ case arg =>
            lang.TreeOps.replace(p2a)(arg)
          })
          (v, FunctionInvocation(f, p, newArgs, r))
        case _ =>
          if (p2a.contains(e)) {
            (v, p2a(e))
          }
          else {
            (v, e)
          }
      }
    })
    m2
  }

  def CallPropagWithNewArgs(wrt: Expr, params2args: Map[Expr, Expr], ctx: Context,
    prog: Trees.Program, origFun: Identifiers.Identifier, inputVarRanges: Map[Identifier, Interval], useBandB: Boolean,
    useGelpia: Boolean)
  : Rational = wrt match {
    // if derivative is wrt to a function
    case FunctionInvocation(i, p, args, _) =>
      evalPropagationErrFunc(ctx, prog, origFun, i, inputVarRanges,
        params2args ++ genNewMapping(p, args, params2args), useBandB, useGelpia)
    // if derivative is wrt to a variable
    case v@Variable(_) =>
      val a1 = params2args(v)
      a1 match {
        //  if there is a literal as input argument
        case RealLiteral(r) => evalLiteralErr(r)

        // if there is a variable as input argument
        case Variable(id) =>
          evalPropagationErrFunc(ctx, prog, origFun, id, inputVarRanges, Map(), useBandB, useGelpia)

        // if there is a function as input argument
        case FunctionInvocation(i, p, a, _) =>
          evalPropagationErrFunc(ctx, prog, origFun, i, inputVarRanges,
            params2args ++ params2args ++ genNewMapping(p, a, params2args), useBandB, useGelpia)
      }
  }

  def fetchSavedResults(funID: Identifier, inputVarRanges: Map[Identifier, Interval], params2args: Map[Expr, Expr],
    saved: Map[(Identifier, Map[Identifier, Interval], Map[Expr, Expr]), Rational]): Option[Rational] = {

    if (saved.keys.filter(k => k._1 == funID).isEmpty) {
      None
    }
    else if (saved.contains((funID, inputVarRanges, params2args))) {
      Some(saved((funID, inputVarRanges, params2args)))
    }
    else {
      // filtering saved results that are for the same function
      val containReqFuncs = saved.keys.filter(k => k._1 == funID)

      // filtering saved results that contain the requested input variables
      val containReqIntervals = containReqFuncs.filter({ k =>
        val newVarMap = k._2.map({ case (id, _) =>
          if (params2args.contains(Variable(id))) {
            val newArg = params2args(Variable(id))
            newArg match {
              case Variable(vId) => vId
              case RealLiteral(_) =>
              case _ =>
            }
          }
        }).filter(_ != ())
        newVarMap.toSet subsetOf inputVarRanges.keys.toSet
      })

      val funcAsArgs = params2args.map({ case (p, a) =>
        a match {
          case Variable(_) =>
          case RealLiteral(_) =>
          case _ => (p, a)
        }
      }).filter(_ != ())

      // filtering saved results with the same params2args mapping
      val containReqMapping = containReqIntervals.filter({ case k =>
        funcAsArgs.toSet subsetOf k._3.toSet
      })

      if (!containReqMapping.isEmpty) {
        Some(saved(containReqMapping.head))
      }
      else {
        None
      }
    }
  }

  def evalLiteralErr(r: Rational): Rational = {

    val constantsPrecision = FinitePrecision.Float64
    val error: Rational = if (constantsPrecision.canRepresent(r)) {
      0.0
    } else {
      constantsPrecision.absRoundoff(r)
    }
    error
  }

  def applyBandB(expr: Expr, inputConfig: InputConfig, minInputWidth: MPFRFloat, minOutputWidth: MPFRFloat, maxSplits:
  Long): MPFRInterval = {

    //  branchAndBound(expr, inputConfig, minInputWidth, minOutputWidth, maxSplits)
    branchAndBoundInParallel(expr, inputConfig, minInputWidth, minOutputWidth, maxSplits)
  }

  def applyGelpia(expr: Expr, inputConfig: InputConfig): MPFRInterval = {

    val inputs = inputConfig.map({case (x,y)  =>
      x + "=" + y + ";"
    }) mkString ""


    val command = "gelpia --function \"" + inputs + expr + "\" --mode=min-max --input-epsilon 1e-4" +
      " --output-epsilon 1e-4 --output-epsilon-relative 1e-4 --timeout 10 --max-iters 4000"
    val result = command.!!

    val bounds = result.split("\n")
    val minLowerBound = bounds(0).split(" ")(3)
    val maxUpperBound = bounds(3).split(" ")(3)
    MPFRInterval(MPFRFloat.fromString(minLowerBound), MPFRFloat.fromString(maxUpperBound))
  }
}
