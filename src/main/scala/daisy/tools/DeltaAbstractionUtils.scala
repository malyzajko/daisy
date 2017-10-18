// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Trees._
import lang.Types.RealType
import lang.Identifiers._
import lang.TreeOps.freeVariablesOf
import tools.FinitePrecision.Float64

trait DeltaAbstractionUtils {
  val cfg: Config

  val deltaName = "delta"
  val epsilonName = "eps"

  val zero = RealLiteral(Rational.zero)
  val one = RealLiteral(Rational.one)
  val two = RealLiteral(Rational.two)

  // scala quirk, cfg is not yet defined when the trait's constructor is called
  lazy val trackRoundoffErrs = !cfg.hasFlag("noRoundoff")
  lazy val denormals = cfg.hasFlag("denormals")

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

  // returns the delta-abstracted expression, as well as the list of deltas
  // corresponding to transcendental functions
  def deltaAbstract(expr: Expr, deltaToVarMap: Map[Variable, Delta],
    epsToVarMap: Map[Variable,Epsilon]): (Expr, Seq[Identifier]) = {

    var transcendentalDeltas = Seq[Identifier]()

    def _deltaAbstract(e: Expr): Expr = (e: @unchecked) match {

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
        UMinus(_deltaAbstract(x))


      // (x + y) -> (x + y)(1 + delta)
      case z @ Plus(x, y) =>
        val xDelta = _deltaAbstract(x)
        val yDelta = _deltaAbstract(y)
        Times(Plus(xDelta, yDelta),
          Plus(one,
            getADelta))

      // (x - y) -> (x - y)(1 + delta)
      case z @ Minus(x, y) =>
        val xDelta = _deltaAbstract(x)
        val yDelta = _deltaAbstract(y)
        Times(Minus(xDelta, yDelta),
          Plus(one,
            getADelta))

      // (x * y) -> (x * y)(1 + delta) + eps
      case z @ Times(x, y) if denormals =>
        val xDelta = _deltaAbstract(x)
        val yDelta = _deltaAbstract(y)
        Plus(
          Times(Times(xDelta, yDelta),
          Plus(one,
            getADelta)), getAnEpsilon)

      // (x * y) -> (x * y)(1 + delta)
      case z @ Times(x, y) =>
        val xDelta = _deltaAbstract(x)
        val yDelta = _deltaAbstract(y)
          Times(Times(xDelta, yDelta),
          Plus(one,
            getADelta))

      // (x / y) -> ( x / y)(1 + delta) + eps
      case z @ Division(x, y) if denormals =>
        val xDelta = _deltaAbstract(x)
        val yDelta = _deltaAbstract(y)
        Plus(
          Times(Division(xDelta, yDelta),
          Plus(one,
            getADelta)), getAnEpsilon)

      // (x / y) -> ( x / y)(1 + delta)
      case z @ Division(x, y) =>
        val xDelta = _deltaAbstract(x)
        val yDelta = _deltaAbstract(y)
          Times(Division(xDelta, yDelta),
          Plus(one,
            getADelta))

      // sqrt(x) = sqrt(x)(1 + delta) + eps
      case z @ Sqrt(x) if denormals =>
        Plus(
          Times(Sqrt(_deltaAbstract(x)),
          Plus(one, getADelta))
          ,getAnEpsilon)

      // sqrt(x) = sqrt(x)(1 + delta)
      case z @ Sqrt(x) =>
        Times(Sqrt(_deltaAbstract(x)),
        Plus(one, getADelta))

      case z @ Sin(x) if denormals => ???
      case z @ Sin(x) =>
        val transDelta = getADelta
        transcendentalDeltas = transcendentalDeltas :+ transDelta.id
        Times(Sin(_deltaAbstract(x)),
        Plus(one, transDelta))

      case z @ Cos(x) if denormals => ???
      case z @ Cos(x) =>
        val transDelta = getADelta
        transcendentalDeltas = transcendentalDeltas :+ transDelta.id
        Times(Cos(_deltaAbstract(x)),
        Plus(one, transDelta))

      case z @ Tan(x) if denormals => ???
      case z @ Tan(x) =>
        val transDelta = getADelta
        transcendentalDeltas = transcendentalDeltas :+ transDelta.id
        Times(Tan(_deltaAbstract(x)),
        Plus(one, transDelta))

      case z @ Exp(x) if denormals => ???
      case z @ Exp(x) =>
        val transDelta = getADelta
        transcendentalDeltas = transcendentalDeltas :+ transDelta.id
        Times(Exp(_deltaAbstract(x)),
        Plus(one, transDelta))

      case z @ Log(x) if denormals => ???
      case z @ Log(x) =>
        val transDelta = getADelta
        transcendentalDeltas = transcendentalDeltas :+ transDelta.id
        Times(Log(_deltaAbstract(x)),
        Plus(one, transDelta))

      // todo add Pow ?

      case z @ Let(x, value, body) =>
        Let(x, _deltaAbstract(value),
          _deltaAbstract(body))

      case z =>
        throw new IllegalArgumentException(s"Unknown expression $z. Parsing failed")
    }

    val res = _deltaAbstract(expr)
    (res, transcendentalDeltas)
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

    case z @ Sin(x) =>
      Sin(replaceDeltasWithZeros(x))

    case z @ Cos(x) =>
      Cos(replaceDeltasWithZeros(x))

    case z @ Tan(x) =>
      Tan(replaceDeltasWithZeros(x))

    case z @ Exp(x) =>
      Exp(replaceDeltasWithZeros(x))

    case z @ Log(x) =>
      Log(replaceDeltasWithZeros(x))

    case z @ Let(x, value, body) =>
      Let(x, value, replaceDeltasWithZeros(body))

    case z => throw new IllegalArgumentException(s"Unknown expression $z. Replacing deltas failed")
  }


}