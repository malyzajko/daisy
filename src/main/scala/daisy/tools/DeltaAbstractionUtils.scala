// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Trees._
import lang.Types.RealType
import lang.Identifiers._
import lang.TreeOps.freeVariablesOf
import tools.FinitePrecision.Float64

trait DeltaAbstractionUtils {

  val deltaName = "delta"
  val epsilonName = "eps"

  val zero = RealLiteral(Rational.zero)
  val one = RealLiteral(Rational.one)
  val two = RealLiteral(Rational.two)

  // TODO: these should probably not be here globally
  var trackRoundoffErrs = true
  var denormals = true

  val deltaIntervalFloat64 = Interval(-Float64.machineEpsilon, Float64.machineEpsilon)
  val epsilonIntervalFloat64 = Interval(-Float64.denormalsError, Float64.denormalsError)

  def getADelta: Delta = {
    val tmp = FreshIdentifier(deltaName, RealType, alwaysShowUniqueID = true).toDeltaVariable
    // tmp.interval = Interval( -Float64.machineEpsilon, Float64.machineEpsilon)
    tmp
  }

  def getAnEpsilon: Epsilon = {
    val tmp = FreshIdentifier(epsilonName, RealType, alwaysShowUniqueID = true).toEpsilonVariable
    // tmp.interval = Interval( -Float64.denormalsError, Float64.denormalsError)
    tmp
  }

  def mapDeltasToVars(e: Expr): Map[Variable, Delta] = {
    freeVariablesOf(e).map(x => {
      (Variable(x) -> getADelta)
    }).toMap
  }
  def mapEpsilonsToVars(e: Expr): Map[Variable, Epsilon] = {
    freeVariablesOf(e).map(x => {
      (Variable(x) -> getAnEpsilon)
    }).toMap
  }

  /**
   * Removes deltas from input maps to avoid not necessary
   * sub-division and reduce map dimension
   * @param in map for variables and deltas
   * @return map for variables
   */
  def removeDeltasFromMap(in: Map[Identifier, Interval]): Map[Identifier, Interval] ={
    in.filterKeys(x => !x.isDeltaId && !x.isEpsilonId)
  }

  def deltaAbstract(e: Expr, deltaToVarMap: Map[Variable, Delta],
    epsToVarMap: Map[Variable,Epsilon]): Expr = (e: @unchecked) match {

    // x -> x * (1 + delta)
    case x: Variable if trackRoundoffErrs && denormals =>
      // initial error
      if (deltaToVarMap.contains(x))
        if (epsToVarMap.contains(x)) {
          Plus(Times(x, Plus(one, deltaToVarMap(x))), epsToVarMap(x))
        } else {
          Plus(Times(x, Plus(one, deltaToVarMap(x))), getAnEpsilon)
        }
      else
        if (epsToVarMap.contains(x)) {
          Plus(Times(x, Plus(one, getADelta)), epsToVarMap(x))
        } else {
          Plus(Times(x, Plus(one, getADelta)), getAnEpsilon)
        }
    // x -> x * (1 + delta)
    case x: Variable if trackRoundoffErrs =>
      // initial error
      if (deltaToVarMap.contains(x)) {
        Times(x, Plus(one, deltaToVarMap(x)))
      } else {
        Times(x, Plus(one, getADelta))
      }
    case x: Variable =>
      // no initial error
      x

    // c -> c * (1 + delta)   ## except if representable directly
    case x: RealLiteral =>
      // No initial error
      // Times(x, Plus(RealLiteral(one), getADelta))
      x

    // x - > x   ## negation does not incur any error
    case UMinus(x) =>
      UMinus(deltaAbstract(x, deltaToVarMap, epsToVarMap))


    // (x + y) -> (x + y)(1 + delta)
    case z @ Plus(x, y) =>
      val xDelta = deltaAbstract(x, deltaToVarMap, epsToVarMap)
      val yDelta = deltaAbstract(y, deltaToVarMap, epsToVarMap)
      Times(Plus(xDelta, yDelta),
        Plus(one,
          getADelta))

    // (x - y) -> (x - y)(1 + delta)
    case z @ Minus(x, y) =>
      val xDelta = deltaAbstract(x, deltaToVarMap, epsToVarMap)
      val yDelta = deltaAbstract(y, deltaToVarMap, epsToVarMap)
      Times(Minus(xDelta, yDelta),
        Plus(one,
          getADelta))

    // (x * y) -> (x * y)(1 + delta) + eps
    case z @ Times(x, y) if denormals =>
      val xDelta = deltaAbstract(x, deltaToVarMap, epsToVarMap)
      val yDelta = deltaAbstract(y, deltaToVarMap, epsToVarMap)
      Plus(
        Times(Times(xDelta, yDelta),
        Plus(one,
          getADelta)), getAnEpsilon)

    // (x * y) -> (x * y)(1 + delta)
    case z @ Times(x, y) =>
      val xDelta = deltaAbstract(x, deltaToVarMap, epsToVarMap)
      val yDelta = deltaAbstract(y, deltaToVarMap, epsToVarMap)
        Times(Times(xDelta, yDelta),
        Plus(one,
          getADelta))

    // (x / y) -> ( x / y)(1 + delta) + eps
    case z @ Division(x, y) if denormals =>
      val xDelta = deltaAbstract(x, deltaToVarMap, epsToVarMap)
      val yDelta = deltaAbstract(y, deltaToVarMap, epsToVarMap)
      Plus(
        Times(Division(xDelta, yDelta),
        Plus(one,
          getADelta)), getAnEpsilon)

    // (x / y) -> ( x / y)(1 + delta)
    case z @ Division(x, y) =>
      val xDelta = deltaAbstract(x, deltaToVarMap, epsToVarMap)
      val yDelta = deltaAbstract(y, deltaToVarMap, epsToVarMap)
        Times(Division(xDelta, yDelta),
        Plus(one,
          getADelta))

    // sqrt(x) = sqrt(x)(1 + delta) + eps
    case z @ Sqrt(x) if denormals =>
      Plus(
        Times(Sqrt(deltaAbstract(x, deltaToVarMap, epsToVarMap)),
        Plus(one, getADelta))
        ,getAnEpsilon)

    // sqrt(x) = sqrt(x)(1 + delta)
    case z @ Sqrt(x) =>
      Times(Sqrt(deltaAbstract(x, deltaToVarMap, epsToVarMap)),
      Plus(one, getADelta))

    // todo add Pow ?

    case z @ Let(x, value, body) =>
      Let(x, deltaAbstract(value, deltaToVarMap, epsToVarMap),
        deltaAbstract(body, deltaToVarMap, epsToVarMap))

    case z =>
      throw new IllegalArgumentException(s"Unknown expression $z. Parsing failed")
  }

  /**
   * replaces all deltas in the expression with zeros
   * @param expr - expression to replace deltas and epsilons in
   * @return expr - expression with RealLiteral(0.) instead of deltas and epsilons
   */
  def replaceDeltasWithZeros(expr: Expr): Expr = expr match {
    case x @ Delta(id) => zero
    case x @ Epsilon(id) => zero
    case x @ Variable(id) => x
    case x @ RealLiteral(r) => x
    case UMinus(x) =>
      UMinus(replaceDeltasWithZeros(x))

    case z @ Plus(x, y) =>
      Plus(replaceDeltasWithZeros(x), replaceDeltasWithZeros(y))

    case z @ Minus(x, y) =>
      Minus(replaceDeltasWithZeros(x), replaceDeltasWithZeros(y))

    case z @ Times(x, y) =>
      Times(replaceDeltasWithZeros(x), replaceDeltasWithZeros(y))

    case z @ Division(x, y) =>
      Division(replaceDeltasWithZeros(x), replaceDeltasWithZeros(y))

    case z @ Pow(x, n) =>
      Pow(replaceDeltasWithZeros(x), replaceDeltasWithZeros(n))

    case z @ Sqrt(x) =>
      Sqrt(replaceDeltasWithZeros(x))

    case z @ Let(x, value, body) =>
      Let(x, value, replaceDeltasWithZeros(body))

    case z => throw new IllegalArgumentException(s"Unknown expression $z. Replacing deltas failed")
  }


}