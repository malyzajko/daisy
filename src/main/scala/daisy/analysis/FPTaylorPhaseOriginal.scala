package daisy.analysis

import daisy._
import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees
import daisy.lang.Trees._
import daisy.lang.Trees.RealLiteral.zero
import daisy.lang.TreeOps._
import daisy.lang.Trees.Expr
import daisy.tools.FinitePrecision.Float64
import daisy.tools.Interval.maxAbs
import daisy.tools.{DeltaAbstractionUtils, Interval, IntervalBranchAndBound, MPFRFloat, MPFRInterval, Rational}
import sys.process._

import scala.collection.immutable.Map

/**
 * This phase evaluates the round-off using the FPTaylor approach
 *
 * This code is as written by Rosa.
 */
object FPTaylorPhaseOriginal extends DaisyPhase with DeltaAbstractionUtils with IntervalBranchAndBound {

  override implicit val debugSection = DebugSectionAnalysis
  override val name = "FPTaylor"
  override val description = "evaluates the Taylor abstract"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    FlagOption(
      "bb",
      "using branch and bound algorithm for range evaluation"),
    FlagOption(
      "gelpia",
      "using gelpia for range evaluation")
  )

  val minWidth = MPFRFloat.fromDouble(0.1) // for Branch and Bound
  val maxSplits: Int = 10000 // for Branch and Bound
  //val deltaIntervalFloat64 = Interval.+/-(Float64.denormalsError) is in DeltaAbstractionUtils
  val epsilonIntervalFloat64MPFR = MPFRInterval.+/-(Float64.machineEpsilon)
  val deltaIntervalFloat64MPFR = MPFRInterval.+/-(Float64.denormalsError)
  // Float64.machineEpsilon * Rational(2,1) = 2.220446049250313e-16
  val transEpsInterval = MPFRInterval(MPFRFloat.fromDouble(-2.220446049250313e-16),
    MPFRFloat.fromDouble(2.2204460492503131e-16))



  override def runPhase(ctx: Context, prog: Trees.Program): (Context, Trees.Program) = {

    val useBandB = ctx.hasFlag("bb")
    val useGelpia = ctx.hasFlag("gelpia")

    // TODO: revisit
    // for saving function derivatives for reuse
    var funcDerivs: Map[Identifier, List[(Identifier, Expr)]] = Map()



    val res: Map[Identifier, Rational] = analyzeConsideredFunctions(ctx, prog) { fnc =>

      ctx.reporter.info(s"using B&B for range evaluation: $useBandB")
      ctx.reporter.info(s"using Gelpia for range evaluation: $useGelpia")

      val inputVarsRanges = ctx.specInputRanges(fnc.id)

      // TODO: is this needed if we have the FunctionCallPhase already before this?
      val body = daisy.lang.TreeOps.inline(fnc.body.get)

      /* ===========================

        Taylor abstract - first order part

         ===========================
      */

      // the real approximation (epsilon/delta abstract) of a floating-point expression
      val (deltaEpsAbstract: Expr, transEps, varAbsErrAssoc, _) =
        epsilonDeltaAbstract(body, denormals = true, abstractVars = true, withAbsErr = true)
      println(deltaEpsAbstract)

      val epsilons = epsilonsOf(deltaEpsAbstract).toList
      val deltas = deltasOf(deltaEpsAbstract).toList
      val epsDeltas = deltas ++ epsilons
      println(epsDeltas)
      println(epsilons)
      println(deltas)


      // computes the first-order error terms of the round-off error

      // partial derivatives with respect to epsilons
      val firstOrderDerivatives = epsilons.map(eps => (eps, getPartialDerivativeWrtExp(deltaEpsAbstract, eps)))
      //println(firstOrderDerivatives)
      // evaluates first-order derivatives by applying absErr vars = 0, deltas = 0 and epsilons = 0
      val evalFirstOrderDerivatives = firstOrderDerivatives.map { case (_, partialDerv) =>
        easySimplify(replaceAbsErrVarsWithZeros(replaceDeltasWithZeros(partialDerv)))
      }.reduceOption((x, y) => Plus(x, y)).getOrElse(RealLiteral(Rational.zero))
      println(evalFirstOrderDerivatives)

      // partial derivatives with respect to absolute initial errors (absErrs)
      val absErrVars = varAbsErrAssoc.values
      val firstOrderAbsErrDerivs = absErrVars.map(v => (v.id, getPartialDerivativeWrtExp(deltaEpsAbstract, v)))

      // evaluates first-order derivatives by applying absErr vars = 0, deltas = 0 and epsilons = 0
      val evalFirstOrderAbsErrDerivatives = firstOrderAbsErrDerivs.map { case (id, partialDerv) =>
        (id, easySimplify(replaceAbsErrVarsWithZeros(replaceDeltasWithZeros(partialDerv))))
      }.toList

      val firstOrderEpsDerivatives = List(evalFirstOrderDerivatives)
      val firstOrderAbsErrDerivatives = evalFirstOrderAbsErrDerivatives

      // Evaluate Taylor Abstract First Order Epsilon Part
      val sumfirstOrderEpsEval = firstOrderEpsDerivatives.map({ case term =>
        val termInterval = if (useGelpia) {
          applyGelpia(term, inputVarsRanges.mapValues(MPFRInterval(_)).toMap).toInterval
        } else if (!useBandB) {
          evalRange[MPFRInterval](term, inputVarsRanges.mapValues(MPFRInterval(_)).toMap,
            MPFRInterval.apply)._1.toInterval
        } else {
          applyBandB(term, inputVarsRanges.mapValues(MPFRInterval(_)).toMap, minWidth, minWidth, maxSplits).toInterval
        }
        Interval.maxAbs(termInterval)
      }).foldLeft(Rational.zero)(_ + _)
      println(sumfirstOrderEpsEval)

      // FIXME: the terms that correspond to the trans epsilons should be multiplied by a larger error
      // (Machine-Epsilon x sumOfFirstOrderEpsilonDerivatives)
      val modularErrPart1 = (Float64.machineEpsilon * sumfirstOrderEpsEval)

      // ctx.timers.get("FPTaylor-EvalFirstOrderEPSErr").stop()

      // ctx.timers.get("FPTaylor-EvalFirstOrderAbsErr").start()

      // Evaluate first-order AbsErr derivatives
      val firstOrderAbsErrEval = firstOrderAbsErrDerivatives.map({ case (termID, termExpr) =>

        val termInterval = if (useGelpia) {
          applyGelpia(termExpr, inputVarsRanges.mapValues(MPFRInterval(_)).toMap).toInterval
        } else if (!useBandB) {
          evalRange[MPFRInterval](termExpr, inputVarsRanges.mapValues(MPFRInterval(_)).toMap,
            MPFRInterval.apply)._1.toInterval
        } else {
          applyBandB(termExpr, inputVarsRanges.mapValues(MPFRInterval(_)).toMap, minWidth, minWidth, maxSplits)
            .toInterval
        }
        (termID, Interval.maxAbs(termInterval))
      })

      //val absErrValMap = ctx.resultModularAnalysis(fnc.id).absErrVarsErrBounds.mapValues(_.toInterval)
      // add initial absolute error ranges for absErr vars
      val absErrValMap: Map[Identifier, Interval] = absErrVars.map(v =>
        v.id -> Interval.+/-(ctx.specInitErrors(fnc.id)(varAbsErrAssoc.find(_._2 == v).map(_._1).get.id))).toMap

      // AbsErrInterval * respected evaluated first-Order term
      val absErrTerms = firstOrderAbsErrEval.map({ case (id, r) => Interval.maxAbs(absErrValMap(id)) * r })

      // Sum (AbsErrInterval * respected evaluated first-Order term)
      val modularErrPart3 = absErrTerms.reduceOption(_ + _).getOrElse(Rational.zero)

      // ctx.timers.get("FPTaylor-EvalFirstOrderAbsErr").stop()

      /* ===========================

        Taylor abstract - second order part (remainder)

         ===========================
      */

      // Taylor abstract - the remainder part

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

      // // add initial absolute error ranges for absErr vars
      // val absErrValMap: Map[Identifier, Interval] = absErrVars.map(v =>
      //   v.id -> Interval.+/-(ctx.specInitErrors(fnc.id)(varAbsErrAssoc.find(_._2 == v).map(_._1).get.id))).toMap

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

      val roundOffRemainder = remainderVal + absErrRemainderValP1 + absErrRemainderValP2 + firstOrderDeltaEvaluated

      //val modularErrPart2 = maxAbs(ctx.resultModularAnalysis(fnc.id).roundOffRemainder.toInterval)
      val modularErrPart2 = maxAbs(roundOffRemainder.toInterval)

      // ctx.timers.get("FPTaylor-EvalFirstOrderEPSErr").start()


      // Taylor abstract error evaluated
      val modularErrEval = modularErrPart1 + modularErrPart2 + modularErrPart3

      // println("\nMaximum evaluated modular error: " + modularErrEval + "\n")
      // println("FPTaylor Eval Summary: " + "firstOrderEpsDerErr: " + modularErrPart1 + " absErr: " + modularErrPart3)
      modularErrEval
    }

    (ctx.copy(resultAbsoluteErrors = res), prog)
  }

  def applyBandB(expr: Expr, inputConfig: InputConfig,
               minInputWidth: MPFRFloat, minOutputWidth: MPFRFloat, maxSplits: Long): MPFRInterval = {

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

  /** replaces all absErr vars in the expression with zeros */
  private def replaceAbsErrVarsWithZeros(expr: Expr): Expr = replace {
    case Variable(id) =>
      if (id.name.startsWith("absErr")) {
        zero
      } else {
        Variable(id)
      }
  }(expr)

  // computes second-order derivative of e with respect to wrt2 and then wrt1
  private def secondOrderDerivative(e: Expr, wrt1: Expr, wrt2: Expr, savedDerivs: Map[Identifier, List[(Identifier, Expr)]])
  : Expr = {
    getPartialDerivativeWrtExp(getPartialDerivativeWrtExp(e, wrt2, savedDerivs), wrt1, savedDerivs)
  }
}
