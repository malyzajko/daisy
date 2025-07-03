// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Trees._
import lang.Types.RealType
import lang.Identifiers._
import lang.TreeOps.{allVariablesOf, replace}
import tools.FinitePrecision.Float64
import lang.Trees.RealLiteral.{one, zero}

import scala.collection.immutable.Seq

trait DeltaAbstractionUtils {

  val epsilonIntervalFloat64 = Interval.+/-(Float64.machineEpsilon)
  val deltaIntervalFloat64 = Interval.+/-(Float64.denormalsError)

  /**
   * Removes deltas from input maps to avoid not necessary
   * sub-division and reduce map dimension
   * @param in map for variables and deltas
   * @return map for variables
   */
  def removeDeltasFromMap(in: Map[Identifier, Interval]): Map[Identifier, Interval] = {
    in.filterKeys(x => !x.isDeltaId && !x.isEpsilonId).toMap
  }

  // returns the epsilon-delta-abstracted expression, as well as the list of epsilons
  // corresponding to transcendental functions and map of abs initial errors corresponding to variables
  // also returns a map that associates the newly generated function epsilons  to the  function signatures
  def epsilonDeltaAbstract(expr: Expr, denormals: Boolean, abstractVars: Boolean = true, withAbsErr: Boolean = false):
  (Expr, Seq[Identifier], Map[Variable, Variable], Map[Epsilon, FunctionInvocation]) = {

    def getAnEpsilon: Epsilon = FreshIdentifier(epsilonName, RealType, alwaysShowUniqueID = true).toEpsilonVariable

    val funcData: collection.mutable.Map[Epsilon, FunctionInvocation] = collection.mutable.Map()

    def getAFuncEpsilon(fun: FunctionInvocation): Epsilon = {
      val eps = FreshIdentifier(epsilonName + "_Func",
        RealType, alwaysShowUniqueID = true).toEpsilonVariable
      funcData += (eps -> fun)
      eps
    }

    def getADelta: Delta = FreshIdentifier(deltaName, RealType, alwaysShowUniqueID = true).toDeltaVariable

    var transcendentalEpsilons = Seq[Identifier]()

    def getATransEpsilon: Epsilon = {
      val d = getAnEpsilon; transcendentalEpsilons :+= d.id; d
    }

    val epsilon: Map[Variable, Epsilon] = allVariablesOf(expr).map(Variable(_) -> getAnEpsilon).toMap
    val delta: Map[Variable, Delta] = allVariablesOf(expr).map(Variable(_) -> getADelta).toMap
    val absErrVars: Map[Variable, Variable] = allVariablesOf(expr).map(Variable(_) ->
      FreshIdentifier("absErr", RealType, alwaysShowUniqueID = true).toVariable
    ).toMap

    val res = replace {

      case x: Variable if !abstractVars =>
        x

      // initial error
      case x: Variable if !denormals =>
        x Times (one Plus epsilon(x))

      case x: Variable if withAbsErr =>
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

    (res, transcendentalEpsilons, absErrVars, funcData.toMap)
  }

  def getEpsilonDeltaExpr(ex: Expr): Expr = {

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
  def replaceDeltasWithZeros(expr: Expr): Expr = replace { case Delta(_) | Epsilon(_) => zero }(expr)


}
