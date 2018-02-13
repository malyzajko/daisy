// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Trees._
import lang.Types.RealType
import lang.Identifiers._
import lang.TreeOps.{allVariablesOf, replace}
import tools.FinitePrecision.Float64
import lang.Trees.RealLiteral.{one, zero}

trait DeltaAbstractionUtils {

  val deltaIntervalFloat64 = Interval.+/-(Float64.machineEpsilon)
  val epsilonIntervalFloat64 = Interval.+/-(Float64.denormalsError)

  /**
   * Removes deltas from input maps to avoid not necessary
   * sub-division and reduce map dimension
   * @param in map for variables and deltas
   * @return map for variables
   */
  def removeDeltasFromMap(in: Map[Identifier, Interval]): Map[Identifier, Interval] ={
    in.filterKeys(x => !x.isDeltaId && !x.isEpsilonId)
  }

  // returns the delta-abstracted expression, as well as the list of deltas
  // corresponding to transcendental functions
  def deltaAbstract(expr: Expr, denormals: Boolean): (Expr, Seq[Identifier]) = {
    def getADelta: Delta = FreshIdentifier(deltaName, RealType, alwaysShowUniqueID = true).toDeltaVariable
    def getAnEpsilon: Epsilon = FreshIdentifier(epsilonName, RealType, alwaysShowUniqueID = true).toEpsilonVariable

    var transcendentalDeltas = Seq[Identifier]()
    def getATransDelta: Delta = {val d = getADelta; transcendentalDeltas :+= d.id; d}

    val delta: Map[Variable, Delta] = allVariablesOf(expr).map(Variable(_) -> getADelta).toMap
    val epsilon: Map[Variable, Epsilon] = allVariablesOf(expr).map(Variable(_) -> getAnEpsilon).toMap

    val res = replace {

      // initial error
      case x: Variable if !denormals =>
        x Times (one Plus delta(x))

      case x: Variable =>
        x Times (one Plus delta(x)) Plus epsilon(x)

      // x - > x   ## negation does not incur any error
      case UMinus(x) =>
        UMinus(x)

      case e @ (Plus(_,_) | Minus(_,_)) =>
        e Times (one Plus getADelta)

      // e -> e * (1 + d)
      case e @ (Sqrt(_) | Times(_, _) | Division(_, _)) if !denormals =>
        e Times (one Plus getADelta)

      case e @ (Times(_, _) | Division(_, _) | Sqrt(_)) =>
        e Times (one Plus getADelta) Plus getAnEpsilon

      case e @ (Sin(_) | Cos(_) | Tan(_) | Log(_) | Exp(_)) if !denormals =>
        e Times (one Plus getATransDelta)

      case e @ (Sin(_) | Cos(_) | Tan(_) | Log(_) | Exp(_)) => ???

      // todo add Pow ?

      case  Let(x, value, body) => throw new Exception("Let not supported by delta abstraction")

    } (expr)


    (res, transcendentalDeltas)
  }

  /**
   * replaces all deltas in the expression with zeros
   * @param expr - expression to replace deltas and epsilons in
   * @return expr - expression with RealLiteral(0.) instead of deltas and epsilons
   */
  def replaceDeltasWithZeros(expr: Expr): Expr = replace{ case Delta(_) | Epsilon(_) => zero }(expr)


}