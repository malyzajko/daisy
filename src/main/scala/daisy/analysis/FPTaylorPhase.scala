package daisy.analysis

import daisy._
import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees
import daisy.lang.Trees._
import daisy.lang.Trees.RealLiteral.{zero, one}
import lang.Types.RealType
import lang.Identifiers._
import daisy.lang.TreeOps._
import daisy.lang.Trees.Expr
import daisy.tools.FinitePrecision.Float64
import daisy.tools.Interval.maxAbs
import daisy.tools.{DeltaAbstractionUtils, Interval, IntervalBranchAndBound, MPFRFloat, MPFRInterval, Rational}
import sys.process._

import scala.collection.immutable.Map
import daisy.experiment.BenchmarkingPhase
import daisy.tools.Omelette

/**
 * This phase evaluates the round-off using the FPTaylor approach
 */
object FPTaylorPhase extends DaisyPhase with IntervalBranchAndBound {

  override implicit val debugSection = DebugSectionAnalysis
  override val name = "FPTaylor"
  override val description = "evaluates the Taylor abstract"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "taylorRange", 
      Set("branchAndBound", "gelpia", "MPFR", "omelette"), 
      "MPFR", 
      "Method used for evaluating ranges in the FPTaylor phase", 
    )
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

    val rangeMethod = ctx.option[String]("taylorRange")

    // TODO: revisit
    // for saving function derivatives for reuse
    var funcDerivs: Map[Identifier, List[(Identifier, Expr)]] = Map()

    val res: Map[Identifier, Rational] = analyzeConsideredFunctions(ctx, prog) { fnc =>

      ctx.reporter.info(s"using $rangeMethod for range evaluation")

      val inputVarsRanges = ctx.specInputRanges(fnc.id).mapValues(MPFRInterval(_)).toMap

      // TODO: is this needed if we have the FunctionCallPhase already before this?
      val body = daisy.lang.TreeOps.inline(fnc.body.get)

      /* ===========================

        Taylor abstract - first order part

         ===========================
      */
      val specInitErrors: Map[Identifier, Rational] = ctx.specInitErrors(fnc.id)

      // the real approximation (epsilon/delta abstract) of a floating-point expression
      val (deltaEpsAbstract: Expr,
           epsilons: Seq[Epsilon],
           transEpsilons: Seq[Identifier],  // subset of epsilons, probably should be a Set
           deltas: Seq[Delta],
           absErrVarsMap: Map[Variable, Variable], _) =
        epsilonDeltaAbstract(body, specInitErrors, denormals = true, abstractVars = true)

      val evalDerivRanges = (wrtIter: Iterator[Variable]) => {
        val derivs = wrtIter.map((wrt: Variable) => {
          val deriv = getPartialDerivativeWrtExp(deltaEpsAbstract, wrt)
          val simpleDeriv = easySimplify(replaceAbsErrVarsWithZeros(replaceDeltasWithZeros(deriv)))
          simpleDeriv
        })
        val evalDerivRange = (deriv: Expr) => {
          rangeMethod match {
            case "gelpia" => applyGelpia(deriv, inputVarsRanges);
            case "branchAndBound" => applyBandB(deriv, inputVarsRanges, minWidth, minWidth, maxSplits);
            case "MPFR" => evalRange[MPFRInterval](deriv, inputVarsRanges, MPFRInterval.apply)._1;
            case "omelette" | // omelette is already handled
            _ => ctx.reporter.fatalError(s"$rangeMethod is not supported.");
          }
        }
        if (rangeMethod == "omelette") {
          val iter_limit = ctx.option[Long]("omeletteIterLimit")
          val cost = ctx.option[String]("omeletteCostFn")
          val omelette = new Omelette(iter_limit, cost)
          val ranges = ctx.specInputRanges(fnc.id)
          omelette.evalRange(derivs, ranges).iterator
        } else {
          derivs.map(evalDerivRange)
        }
      }

      // partial derivatives with respect to epsilons (arithmetic and transcendental)
      val firstOrderEpsilonsFactor = evalDerivRanges(epsilons.iterator)
        .zip(epsilons.iterator)
        .map({ case (interval, eps) => {
          val maxAbs = Interval.maxAbs(interval.toInterval)
          (eps, maxAbs)
        } })

      val (transEpsFactors, arithEpsFactors) = firstOrderEpsilonsFactor.partition({
        case (eps, _) => transEpsilons.contains(eps.id)
      })
      val arithmeticError = Float64.machineEpsilon * arithEpsFactors.map(_._2).foldLeft(Rational.zero)(_ + _)
      // TODO: make this an option to specify how large the transcendental error is
      val transcendentalError = 2 * Float64.machineEpsilon * transEpsFactors.map(_._2).foldLeft(Rational.zero)(_ + _)
      val firstOrderError1: Rational = arithmeticError + transcendentalError


      // partial derivatives with respect to absolute initial errors (absErrs)
      val absErrVars = absErrVarsMap.values

      val propagatedInputErrors = evalDerivRanges(absErrVars.iterator)
        .zip(absErrVarsMap.keys)
        .map({ case (interval, v) => {
          val maxAbs = Interval.maxAbs(interval.toInterval)
          specInitErrors(v.id) * maxAbs
        }}).foldLeft(Rational.zero)(_ + _)

      val firstOrderError2: Rational = propagatedInputErrors

      /* ===========================

        Taylor abstract - second order part (remainder)

         ===========================
      */

      // first-order partial derivative wrt to delta terms also go to remainder
      val firstOrderDeltaError: MPFRInterval = MPFRInterval(Float64.denormalsError) * deltas.map(d => {
        val deriv = getPartialDerivativeWrtExp(deltaEpsAbstract, d)
        val simpleDeriv = easySimplify(replaceAbsErrVarsWithZeros(replaceDeltasWithZeros(deriv)))
        val interval: MPFRInterval = evalRange[MPFRInterval](simpleDeriv, inputVarsRanges, MPFRInterval.apply)._1
        MPFRInterval(MPFRInterval.maxAbs(interval))
      }).foldLeft(MPFRInterval.zero)(_ + _)
      val firstOrderError3: Rational = Interval.maxAbs(firstOrderDeltaError.toInterval)


      // computes one part of the remainder term:
      // 1/2 x sum of (second-order derivatives with respect to deltaIds and epsilonIds) x deltaId x epsilonId
      val allErrorVars = deltas ++ epsilons ++ absErrVars

      val deltaValMap = deltas.map(d => d.id -> deltaIntervalFloat64MPFR).toMap

      val epsilonValMap = epsilons.map(eps =>
        if (!transEpsilons.contains(eps.id)) {
          eps.id -> epsilonIntervalFloat64MPFR
        } else {
          eps.id -> transEpsInterval
        }).toMap

      val absErrValMap = absErrVarsMap.map({
        case (variable, errorVar) => (errorVar.id -> MPFRInterval.+/-(specInitErrors(variable.id)))
      })

      val inputValMap: Map[Identifier, MPFRInterval] = inputVarsRanges ++
        deltaValMap ++ epsilonValMap ++ absErrValMap


      val sumOfTerms = allErrorVars.flatMap(e1 =>
        allErrorVars.map(e2 => {
          val deriv = easySimplify(secondOrderDerivative(deltaEpsAbstract, e1, e2, funcDerivs))
          val term = Times(Times(deriv, e1), e2)

          val interval = evalRange[MPFRInterval](term, inputValMap, MPFRInterval.apply)._1
          MPFRInterval(MPFRInterval.maxAbs(interval))

        })).foldLeft(MPFRInterval.zero)(_ + _)
      val remainder: Rational = Rational(1l, 2l) * Interval.maxAbs(sumOfTerms.toInterval)

      firstOrderError1 + firstOrderError2 + firstOrderError3 + remainder
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


  // returns the epsilon-delta abstraction of expr, with a list of normal epsilons, transcendental epsilons, deltas, absolute error
  // epsilons (and their mapping to corresponding variables) and function epsilons (with mapping to funcs they belong to)
  private def epsilonDeltaAbstract(expr: Expr, inputErrors: Map[Identifier, Rational],
    denormals: Boolean, abstractVars: Boolean = true):
    (Expr, Seq[Epsilon], Seq[Identifier], Seq[Delta], Map[Variable, Variable], Map[Epsilon, FunctionInvocation]) = {
    // TODO: there is a version in DeltaAbstractionUtils of this function, but that function
    // assumes a different semantics of input errors -> this function follows the semantics assumed in the other
    // analyses: if there is a specified input error, there is no additional rounding error.

    var allEpsilons: Seq[Epsilon] = Seq()
    var allDeltas: Seq[Delta] = Seq()
    def getAnEpsilon: Epsilon = {
      val eps = FreshIdentifier(epsilonName, RealType, alwaysShowUniqueID = true).toEpsilonVariable
      allEpsilons = allEpsilons :+ eps
      eps
    }
    def getADelta: Delta = {
      val delta = FreshIdentifier(deltaName, RealType, alwaysShowUniqueID = true).toDeltaVariable
      allDeltas = allDeltas :+ delta
      delta
    }

    val epsilon: Map[Variable, Epsilon] = allVariablesOf(expr).map(Variable(_) -> getAnEpsilon).toMap
    val delta: Map[Variable, Delta] = allVariablesOf(expr).map(Variable(_) -> getADelta).toMap

    var transcendentalEpsilons = Seq[Identifier]()
    def getATransEpsilon: Epsilon = {
      val d = getAnEpsilon; transcendentalEpsilons :+= d.id; d
    }

    // maps program variables to their error variables TODO: maybe encode better?
    // val absErrVars: Map[Variable, Variable] = allVariablesOf(expr).map(Variable(_) ->
    //   FreshIdentifier("absErr", RealType, alwaysShowUniqueID = true).toVariable
    // ).toMap
    //only those variables that have an input error have a error variable
    val absErrVars : Map[Variable, Variable] = inputErrors.filter(x => x._2 > Rational.zero).map({
      case (id, error) =>
        (Variable(id) -> FreshIdentifier("absErr", RealType, alwaysShowUniqueID = true).toVariable)
    }).toMap

    // handling functions
    val funcData: collection.mutable.Map[Epsilon, FunctionInvocation] = collection.mutable.Map()
    def getAFuncEpsilon(fun: FunctionInvocation): Epsilon = {
      val eps = FreshIdentifier(epsilonName + "_Func",
        RealType, alwaysShowUniqueID = true).toEpsilonVariable
      funcData += (eps -> fun)
      eps
    }


    val res = replace {

      case x: Variable if !abstractVars =>
        x

      // case x: Variable if absErrVars.isDefinedAt(x) =>
      //   x Plus absErrVars(x)

      case x: Variable if !denormals =>
        x Times (one Plus epsilon(x))

      // TODO: is variable has a specified input error, this should subsume normal or denormal rounding errors
      case x: Variable if absErrVars.isDefinedAt(x) =>
        x Times (one Plus epsilon(x)) Plus delta(x) Plus absErrVars(x)

      case x: Variable =>
        x Times (one Plus epsilon(x)) Plus delta(x)

      // x - > x   ## negation does not incur any error
      case UMinus(x) =>
        UMinus(x)

      case e@(Plus(_, _) | Minus(_, _)) =>
        e Times (one Plus getAnEpsilon)

      // e -> e * (1 + d)
      case e@(Sqrt(_) | Times(_, _) | Division(_, _)) if !denormals =>
        e Times (one Plus getAnEpsilon)

      case e@(Times(_, _) | Division(_, _) | Sqrt(_)) =>
        e Times (one Plus getAnEpsilon) Plus getADelta

      case e@(Sin(_) | Cos(_) | Tan(_) | Log(_) | Exp(_)) if !denormals =>
        e Times (one Plus getATransEpsilon)

      case e@(Sin(_) | Cos(_) | Tan(_) | Log(_) | Exp(_) | Atan(_)) =>
        e Times (one Plus getATransEpsilon) Plus getADelta
      // todo add Pow ?

      case e@FunctionInvocation(_, params, args, _) =>

        Plus(e, getAFuncEpsilon(e))

      case Let(x, value, body) => throw new Exception("Let not supported by delta abstraction")

    }(expr)

    // returns the epsilon-delta abstraction of expr, with a list of normal epsilons, transcendental epsilons, deltas, absolute error
    // epsilons (and their mapping to corresponding variables) and function epsilons (with mapping to funcs they belong to)
    (res, allEpsilons, transcendentalEpsilons, allDeltas, absErrVars, funcData.toMap)

    //(res, transcendentalEpsilons, absErrVars, funcData.toMap)
  }

  private def getEpsilonDeltaExpr(ex: Expr): Expr = {

    ex match {
      case d@Delta(_) => d
      case e@Epsilon(_) => e
      case _ => throw new Exception("not a valid Epsilon or Delta term")
    }
  }


  /**
   * replaces all deltas in the expression with zeros
   * @param expr - expression to replace deltas and epsilons in
   * @return expr - expression with RealLiteral(0.) instead of deltas and epsilons
   */
  private def replaceDeltasWithZeros(expr: Expr): Expr = replace { case Delta(_) | Epsilon(_) => zero }(expr)

}
