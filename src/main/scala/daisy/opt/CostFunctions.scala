
package daisy
package opt

import scala.collection.immutable.Seq
import lang.Trees._
import tools.FinitePrecision._
import tools.Rational
import lang.Identifiers._
import lang.Extractors.ArithOperator
import Rational.zero
import daisy.lang.Types.FinitePrecisionType

import scala.collection.mutable.HashMap

trait CostFunctions {

  // lexicographic ordering on (precision, op count) pairs
  def lessThanByOpCounts(counts1: Map[Precision, Int], counts2: Map[Precision, Int]): Boolean = {
    counts1.keys.head match {
      case FloatPrecision(_) => //assuming floats and fixed are not mixed
        if (counts1(Float128) < counts2(Float128)) {
          true
        } else if (counts2(Float128) < counts1(Float128)) {
          false
        } else { // counts1 == counts2 on Float128
          if (counts1(Float64) < counts2(Float64)) {
            true
          } else if (counts2(Float64) < counts1(Float64)) {
            false
          } else { // counts1 == counts2 on Float64
            // then the Float32 counts must also be equal, since we assume equal total counts
            assert(counts1(Float32) == counts2(Float32))
            false
          }
        }

      case _ =>
        if (counts1(FixedPrecision(32)) < counts2(FixedPrecision(32))) {
          true
        } else if (counts2(FixedPrecision(32)) < counts1(FixedPrecision(32))) {
          false
        } else {
          counts1(FixedPrecision(16)) < counts2(FixedPrecision(16))
        }
    }
  }

  def lessThanByRationalCost(cost1: Rational, cost2: Rational): Boolean = {
    cost1 < cost2
  }


  // counts the number of operations performed in each precision and in addition
  // also counts the types of *free* variables
  // this seems to be the cost function in FPTuner
  def countNumOps(expr: Expr, typeConfig: Map[Identifier, Precision]): Map[Precision, Int] = {

    val counts = new HashMap[Precision, Int]
    counts += (Float32 -> 0)
    counts += (Float64 -> 0)
    counts += (Float128 -> 0)
    counts += (FixedPrecision(32) -> 0)
    counts += (FixedPrecision(16) -> 0)

    def eval(e: Expr): Unit = (e: @unchecked) match {
      //case Variable(id) => typeConfig(id)

      case Let(id, RealLiteral(_), body) => eval(body)

      case Let(id, UMinus(Variable(t)), body) =>
        val opPrec = typeConfig(t)
        counts += ((opPrec -> (counts(opPrec) + 1)))
        eval(body)

      case Let(id, ArithOperator(Seq(Variable(lhs), Variable(rhs)), recons), body) =>
        val precL = typeConfig(lhs)
        val precR = typeConfig(rhs)
        val precId = typeConfig(id)
        val newPrec = getUpperBound(getUpperBound(precL, precR), precId)
        counts += ((newPrec -> (counts(newPrec) + 1)))
        eval(body)

      case UMinus(Variable(t)) =>
        val opPrec = typeConfig(t)
        counts += ((opPrec -> (counts(opPrec) + 1)))


      case ArithOperator(Seq(Variable(lhs), Variable(rhs)), recons) =>
        val precL = typeConfig(lhs)
        val precR = typeConfig(rhs)
        val newPrec = getUpperBound(precL, precR)
        counts += ((newPrec -> (counts(newPrec) + 1)))
    }
    eval(expr)

    for(id <- lang.TreeOps.freeVariablesOf(expr)) {
      val prec = typeConfig(id)
      counts += ((prec -> (counts(prec) + 1)))
    }

    counts.toMap
  }

  // cost function for rewriting
  def countOps(expr: Expr): Int = (expr: @unchecked) match {
    case Variable(id) => 0

    case RealLiteral(_) => 0

    case Let(id, RealLiteral(_), body) => countOps(body)

    case ArithOperator(Seq(t), recons) =>
      countOps(t) + 1

    case ArithOperator(Seq(lhs, rhs), recons) =>
      countOps(lhs) + countOps(rhs) + 1

    case Let(id, value, body) =>
      countOps(value) + countOps(body)
  }

  // based on benchmarked cost for Float64
  def rewritingCost(expr: Expr): Double = (expr: @unchecked) match {
    case Variable(id) => 0

    case RealLiteral(_) => 0

    case Let(id, RealLiteral(_), body) => rewritingCost(body)

    case UMinus(t) => rewritingCost(t) + 1.1129

    case Sqrt(t) => rewritingCost(t) + 1.6171

    case Plus(lhs, rhs) =>
      rewritingCost(lhs) + rewritingCost(rhs) + 1.7357

    case Minus(lhs, rhs) =>
      rewritingCost(lhs) + rewritingCost(rhs) + 2.1457

    case Times(lhs, rhs) =>
      rewritingCost(lhs) + rewritingCost(rhs) + 1.81

    case Division(lhs, rhs) =>
      rewritingCost(lhs) + rewritingCost(rhs) + 1.95

    case Let(id, value, body) =>
      countOps(value) + countOps(body)
  }

  /*
  This cost function has been determined by benchmarking individual operations using the scripts in the
  scripts/benchmarked_cost directory.
 */
  def benchmarkedMixedPrecisionCostForC(expr: Expr, typeConfig: Map[Identifier, Precision]): Rational = {

    // from -> to
    def castCost(from: Precision, to: Precision) = ((from, to): @unchecked) match {
      case (Float32, Float32) => Rational.one
      case (Float32, Float64) => Rational.fromReal(1.058)
      case (Float32, Float128) => Rational.fromReal(1.054)
      case (Float64, Float32) => Rational.fromReal(1.003)
      case (Float64, Float64) => Rational.fromReal(1.049)
      case (Float64, Float128) => Rational.fromReal(1.054)
      case (Float128, Float32) => Rational.fromReal(1.048)
      case (Float128, Float64) => Rational.fromReal(1.059)
      case (Float128, Float128) => Rational.fromReal(1.057)
    }

    def plusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.019)
      case Float64 => Rational.fromReal(1.035)
      case Float128 => Rational.fromReal(1.051)
    }

    def minusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.057)
      case Float64 => Rational.fromReal(1.056)
      case Float128 => Rational.fromReal(1.049)
    }

    def timesCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.051)
      case Float64 => Rational.fromReal(1.052)
      case Float128 => Rational.fromReal(1.055)
    }

    def divCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.049)
      case Float64 => Rational.fromReal(1.057)
      case Float128 => Rational.fromReal(1.054)
    }

    def uminusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.063)
      case Float64 => Rational.fromReal(1.057)
      case Float128 => Rational.fromReal(1.053)
    }

    def sqrtCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.054)
      case Float64 => Rational.fromReal(1.055)
      case Float128 => Rational.fromReal(3.353)
    }

    def sinCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.one
      case Float64 => Rational.fromReal(1.051)
      case Float128 => Rational.fromReal(58.646)
    }

    def cosCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(10.0)
      case Float64 => Rational.fromReal(11.0)
      case Float128 => Rational.fromReal(20.0)
    }

    def tanCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.015)
      case Float64 => Rational.fromReal(1.05)
      case Float128 => Rational.fromReal(57.209)
    }

    def logCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.003)
      case Float64 => Rational.fromReal(1.008)
      case Float128 => Rational.fromReal(70.627)
    }

    def expCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.007)
      case Float64 => Rational.fromReal(1.003)
      case Float128 => Rational.fromReal(17.8)
    }

    def asinCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(10.0)
      case Float64 => Rational.fromReal(11.0)
      case Float128 => Rational.fromReal(20.0)
    }

    def acosCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.041)
      case Float64 => Rational.fromReal(1.054)
      case Float128 => Rational.fromReal(95.387)
    }

    def atanCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.004)
      case Float64 => Rational.fromReal(1.022)
      case Float128 => Rational.fromReal(71.247)
    }

    def eval(e: Expr): Rational = (e: @unchecked) match {

      // constant declarations
      case Let(id, RealLiteral(_), body) =>
        eval(body)

      // cost of arithmetic operations
      case Let(id, UMinus(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)
        val opCost = uminusCost(tPrec)

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (opCost + castCosts + eval(body))

      case Let(id, Sqrt(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)
        val opCost = sqrtCost(tPrec)

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (opCost + castCosts + eval(body))

      case Let(id, ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons), body) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val idPrec = typeConfig(id)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        // TODO: this is probably not the best way of doing it
        val opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => plusCost(opPrec)
          case _: Minus => minusCost(opPrec)
          case _: Times => timesCost(opPrec)
          case _: Division => divCost(opPrec)
        }
        var castCosts = zero
        // upcasts
        if (lPrec != opPrec) {
          castCosts = castCosts + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          castCosts = castCosts + castCost(rPrec, opPrec)
        }
        // downcast
        if (idPrec < opPrec) {
          castCosts = castCosts + castCost(opPrec, idPrec)
        }

        (opCost + castCosts + eval(body))

      // must be a transcendental function
      case Let(id, fun @ ArithOperator(Seq(y @ Variable(t)), _), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)
        val opCost = fun match {
          case Sin(_) => sinCost(tPrec)
          case Cos(_) => cosCost(tPrec)
          case Tan(_) => tanCost(tPrec)
          case Log(_) => logCost(tPrec)
          case Exp(_) => expCost(tPrec)
          case Asin(_) => asinCost(tPrec)
          case Acos(_) => acosCost(tPrec)
          case Atan(_) => atanCost(tPrec)
        }

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (opCost + castCosts + eval(body))

      case Let(id, Variable(t), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (castCosts + eval(body))

      case ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val opPrec = getUpperBound(lPrec, rPrec)

        val opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => plusCost(opPrec)
          case _: Minus => minusCost(opPrec)
          case _: Times => timesCost(opPrec)
          case _: Division => divCost(opPrec)
        }
        var castCosts = zero
        // upcasts
        if (lPrec != opPrec) {
          castCosts = castCosts + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          castCosts = castCosts + castCost(rPrec, opPrec)
        }
        (opCost + castCosts)

      case UMinus(Variable(t)) =>
        val tPrec = typeConfig(t)
        uminusCost(tPrec)

      case Sqrt(Variable(t)) =>
        val tPrec = typeConfig(t)
        sqrtCost(tPrec)

      case Variable(t) =>
        // Returning just a variable costs nothing
        Rational.zero

    }
    eval(expr)
  }

  /*
    This cost function has been determined by benchmarking individual operations.
   */
  def benchmarkedMixedPrecisionCost(expr: Expr, typeConfig: Map[Identifier, Precision]): Rational = {

    // from -> to
    def castCost(from: Precision, to: Precision) = ((from, to): @unchecked) match {
      case (Float32, Float32) => zero
      case (Float32, Float64) => zero  // Rational.fromReal(1.0157)
      case (Float32, Float128) => Rational.fromReal(2.1643)
      case (Float64, Float32) => Rational.fromReal(1.3357)
      case (Float64, Float64) => zero
      case (Float64, Float128) => Rational.fromReal(2.21)
      case (Float128, Float32) => Rational.fromReal(1.4814)
      case (Float128, Float64) => Rational.fromReal(1.6971)
      case (Float128, Float128) => zero
    }

    def plusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.6114)
      case Float64 => Rational.fromReal(1.7357)
      case Float128 => Rational.fromReal(2.3343)
      //case QuadDouble => Rational.fromReal(2.3343)
    }

    def minusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.8829)
      case Float64 => Rational.fromReal(2.1457)
      case Float128 => Rational.fromReal(2.3229)
      //case QuadDouble => Rational.fromReal(2.3343)
    }

    def timesCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.8029)
      case Float64 => Rational.fromReal(1.81)
      case Float128 => Rational.fromReal(2.1986)
      //case QuadDouble => Rational.fromReal(2.1986)
    }

    def divCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.7657)
      case Float64 => Rational.fromReal(1.95)
      case Float128 => Rational.fromReal(3.2657)
      //case QuadDouble => Rational.fromReal(3.2657)
    }

    def uminusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.1714)
      case Float64 => Rational.fromReal(1.1129)
      case Float128 => Rational.fromReal(1.7086)
      //case QuadDouble => Rational.fromReal(1.7086)
    }

    def sqrtCost(prec: Precision): Rational = (prec: @unchecked) match {
      case Float32 => sqrtCost(Float64) + castCost(Float64, Float32)
      case Float64 => Rational.fromReal(1.6171)
      case Float128 => Rational.fromReal(6.3686)
      //case QuadDouble => Rational.fromReal(1.7086)
    }

    // TODO: these should be measured properly
    // these are chosen simply an order of magnitude bigger than arithmetic
    def transCost(prec: Precision): Rational = (prec: @unchecked) match {
      case Float32 => transCost(Float64) + castCost(Float64, Float32)
      case Float64 => Rational.fromReal(11.0)
      case Float128 => Rational.fromReal(20.0)
    }


    def eval(e: Expr): Rational = (e: @unchecked) match {

      // constant declarations
      case Let(id, RealLiteral(_), body) =>
        eval(body)

      // cost of arithmetic operations
      case Let(id, UMinus(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)
        val opCost = uminusCost(tPrec)

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (opCost + castCosts + eval(body))

      case Let(id, Sqrt(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)
        val opCost = sqrtCost(tPrec)

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (opCost + castCosts + eval(body))

      case Let(id, ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons), body) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val idPrec = typeConfig(id)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        // TODO: this is probably not the best way of doing it
        val opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => plusCost(opPrec)
          case _: Minus => minusCost(opPrec)
          case _: Times => timesCost(opPrec)
          case _: Division => divCost(opPrec)
        }
        var castCosts = zero
        // upcasts
        if (lPrec != opPrec) {
          castCosts = castCosts + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          castCosts = castCosts + castCost(rPrec, opPrec)
        }
        // downcast
        if (idPrec < opPrec) {
          castCosts = castCosts + castCost(opPrec, idPrec)
        }

        (opCost + castCosts + eval(body))

      // must be a transcendental function
      case Let(id, ArithOperator(Seq(y @ Variable(t)), recons), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)
        val opCost = transCost(tPrec)

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (opCost + castCosts + eval(body))

      case Let(id, Variable(t), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val castCosts = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { zero }

        (castCosts + eval(body))

      case ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val opPrec = getUpperBound(lPrec, rPrec)

        val opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => plusCost(opPrec)
          case _: Minus => minusCost(opPrec)
          case _: Times => timesCost(opPrec)
          case _: Division => divCost(opPrec)
        }
        var castCosts = zero
        // upcasts
        if (lPrec != opPrec) {
          castCosts = castCosts + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          castCosts = castCosts + castCost(rPrec, opPrec)
        }
        (opCost + castCosts)

      case UMinus(Variable(t)) =>
        val tPrec = typeConfig(t)
        uminusCost(tPrec)

      case Sqrt(Variable(t)) =>
        val tPrec = typeConfig(t)
        sqrtCost(tPrec)

      case Variable(t) =>
        // Returning just a variable costs nothing
        Rational.zero

    }
    eval(expr)
  }

  /*
    A very simple cost function, essentially counting the number of double,
    single operations as well as casts.
    Assumes that floating-point and fixed-point costs are not mixed.
    Returns a rational for compatibility reasons.
    The cost values are randomly assigned based on intuition.
   */
  def simpleMixedPrecisionCost(expr: Expr, typeConfig: Map[Identifier, Precision]): Rational = {

    def varCost(prec: Precision): Int = (prec: @unchecked) match {
      case Float32 => 1
      case Float64 => 2
      case Float128 => 4
      //case QuadDouble => (8, QuadDouble)
      case FixedPrecision(16) => 1
      case FixedPrecision(32) => 2
      case FixedPrecision(64) => 4
    }

    // from -> to
    def castCost(from: Precision, to: Precision): Int = (to: @unchecked) match {
      case Float32 => 1  // TODO: this should not really happen...
      case Float64 => 1
      case Float128 => 2
      //case QuadDouble => 4
      case FixedPrecision(16) => 1   // TODO: this should not really happen
      case FixedPrecision(32) => 1
      case FixedPrecision(64) => 1
    }

    // constant cost per operation, but depending on the type
    def opCost(prec: Precision): Int = (prec: @unchecked) match {
      case Float32 => 1
      case Float64 => 2
      case Float128 => 4
      //case QuadDouble => 8
      case FixedPrecision(16) => 1
      case FixedPrecision(32) => 2
      case FixedPrecision(64) => 4
    }

    def plusCost(prec: Precision) = opCost(prec)
    def minusCost(prec: Precision) = opCost(prec)
    def timesCost(prec: Precision) = opCost(prec)
    def divCost(prec: Precision) = opCost(prec)
    def uminusCost(prec: Precision) = opCost(prec)
    def sqrtCost(prec: Precision) = opCost(prec)
    def transCost(prec: Precision) = opCost(prec) * 10

    def eval(e: Expr): Rational = (e: @unchecked) match {

      // constant declarations
      case Let(id, RealLiteral(_), body) =>
        eval(body)

      case Let(id, Variable(t), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = varCost(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _castCost + eval(body))

      // cost of arithmetic operations
      case Let(id, UMinus(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = varCost(tPrec)
        val _opCost = uminusCost(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _opCost + _castCost + eval(body))

      case Let(id, Sqrt(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = varCost(tPrec)
        val _opCost = sqrtCost(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _opCost + _castCost + eval(body))

      // must be a transcendental function
      case Let(id, ArithOperator(Seq(y @ Variable(t)), recons), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = varCost(tPrec)
        val _opCost = transCost(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _opCost + _castCost + eval(body))

      case Let(id, ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons), body) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val idPrec = typeConfig(id)

        val _varCost = varCost(lPrec) + varCost(rPrec)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        // TODO: this is probably not the best way of doing it
        val _opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => plusCost(opPrec)
          case _: Minus => minusCost(opPrec)
          case _: Times => timesCost(opPrec)
          case _: Division => divCost(opPrec)
        }
        var _castCost = zero
        // upcasts
        if (lPrec != opPrec) {
          _castCost = _castCost + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          _castCost = _castCost + castCost(rPrec, opPrec)
        }
        // downcast
        if (idPrec < opPrec) {
          _castCost = _castCost + castCost(opPrec, idPrec)
        }

        (_varCost + _opCost + _castCost + eval(body))

      case ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val opPrec = getUpperBound(lPrec, rPrec)

        val _varCost = varCost(lPrec) + varCost(rPrec)
        val _opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => plusCost(opPrec)
          case _: Minus => minusCost(opPrec)
          case _: Times => timesCost(opPrec)
          case _: Division => divCost(opPrec)
        }
        var _castCost = zero
        // upcasts
        if (lPrec != opPrec) {
          _castCost = _castCost + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          _castCost = _castCost + castCost(rPrec, opPrec)
        }
        (_varCost + _opCost + _castCost)

      case UMinus(Variable(t)) =>
        val tPrec = typeConfig(t)
        (uminusCost(tPrec) + varCost(tPrec))

      case Sqrt(Variable(t)) =>
        val tPrec = typeConfig(t)
        (sqrtCost(tPrec) + varCost(tPrec))

      case Variable(t) =>
        // Returning just a variable costs nothing
        Rational.zero

    }
    eval(expr)
  }


  /*
    An area based cost function, essentially counting the number of double,
    single operations as well as casts.
    Assumes that given expreesion is in fixed point.
    Returns a rational for compatibility reasons.
    The cost values represent the area of circuit needed to implement the operations.
   */


  def areaBasedCostFunction(expr: Expr, typeConfig: Map[Identifier, Precision]): Rational = {

    // Gives the number of bits of Fixed Precision Type
    def extractBits(prec: Precision): Int = (prec: @unchecked) match {
      case FixedPrecision(a) => a
    }

    // from -> to
    // TODO: cast cost function need to be updated. This is a naive cost
    def castCost(from: Precision, to: Precision): Int = extractBits(to)

    // TODO : Unary cost is just en estimate it needs to be refined according to the way unary operation works
    def uminusCost(prec: Precision) = extractBits(prec)
    def transCost(prec: Precision) = extractBits(prec) * 10

    def timesCost(lPrec: Precision, rPrec: Precision) = {
      val (lCost,rCost) = (lPrec: @unchecked, rPrec: @unchecked) match {
        case (FixedPrecision(a),FixedPrecision(b)) => (a,b)
      }
      lCost*rCost
    }

    def eval(e: Expr): Rational = (e: @unchecked) match {

      // constant declarations
      case Let(id, RealLiteral(_), body) =>
        eval(body)

      // cost of arithmetic operations
      case Let(id, UMinus(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = extractBits(tPrec)
        val _opCost = uminusCost(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _opCost + _castCost + eval(body))

      case Let(id, Variable(t), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = extractBits(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _castCost + eval(body))

      case Let(id, Sqrt(Variable(t)), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = extractBits(tPrec)
        val _opCost = transCost(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _opCost + _castCost + eval(body))

      // must be a transcendental function
      case Let(id, ArithOperator(Seq(y @ Variable(t)), recons), body) =>
        val tPrec = typeConfig(t)
        val idPrec = typeConfig(id)

        val _varCost = extractBits(tPrec)
        val _opCost = transCost(tPrec)

        val _castCost = if (idPrec < tPrec) {
          castCost(tPrec, idPrec)
        } else { 0 }

        (_varCost + _opCost + _castCost + eval(body))

      case Let(id, ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons), body) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val idPrec = typeConfig(id)

        val _varCost = extractBits(lPrec) + extractBits(rPrec)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        // TODO: this is probably not the best way of doing it
        val _opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => extractBits(opPrec)
          case _: Minus => extractBits(opPrec)
          case _: Times => timesCost(lPrec,rPrec)
          case _: Division => extractBits(opPrec)
        }
        var _castCost = zero
        // upcasts
        if (lPrec != opPrec) {
          _castCost = _castCost + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          _castCost = _castCost + castCost(rPrec, opPrec)
        }
        // downcast
        if (idPrec < opPrec) {
          _castCost = _castCost + castCost(opPrec, idPrec)
        }

        (_varCost + _opCost + _castCost + eval(body))

      case ArithOperator(Seq(y @ Variable(l), z @ Variable(r)), recons) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)
        val opPrec = getUpperBound(lPrec, rPrec)

        val _varCost = extractBits(lPrec) + extractBits(rPrec)
        val _opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => extractBits(opPrec)
          case _: Minus => extractBits(opPrec)
          case _: Times => timesCost(lPrec,rPrec)
          case _: Division => extractBits(opPrec)
        }
        var _castCost = zero
        // upcasts
        if (lPrec != opPrec) {
          _castCost = _castCost + castCost(lPrec, opPrec)
        }
        if (rPrec != opPrec) {
          _castCost = _castCost + castCost(rPrec, opPrec)
        }
        (_varCost + _opCost + _castCost)

      case UMinus(Variable(t)) =>
        val tPrec = typeConfig(t)
        (uminusCost(tPrec) + extractBits(tPrec))

      case Sqrt(Variable(t)) =>
        val tPrec = typeConfig(t)
        (transCost(tPrec) + extractBits(tPrec))

      case Variable(t) =>
        // Returning just a variable costs nothing
        Rational.zero
    }
    eval(expr)
  }


  /*def maximizeDoubleVars(expr: Expr, typeConfig: Map[Identifier, Precision],
    goodPrecision: Precision = Float64): Rational = {
    def eval(e: Expr): (Int, Precision) = (e: @unchecked) match {
      case Variable(id) =>
        if (typeConfig(id) == goodPrecision) {
          (0, typeConfig(id))
        } else {
          (1, typeConfig(id))
        }

      case Let(id, RealLiteral(_), body) =>
        if (typeConfig(id) == goodPrecision) {
          eval(body)
        } else {
          val (costBody, precBody) = eval(body)
          (costBody + 1, precBody)
        }

      //case RealLiteral(_) => (costConst, constantsPrecision)

      case UMinus(t) => eval(t)

      case ArithOperator(Seq(lhs, rhs), recons) =>
        val (costL, precL) = eval(lhs)
        val (costR, precR) = eval(rhs)
        (costL + costR, getUpperBound(precL, precR))

      case Let(id, value, body) =>
        // no extra cost if we need to cast
        val (costValue, precValue) = eval(value)
        val (costBody, precBody) = eval(body)
        if (typeConfig(id) == goodPrecision) {
          (costValue + costBody, precBody)
        } else {
          (costValue + costBody + 1, precBody)
        }
    }
    Rational(eval(expr)._1)
  }

  // this cost function adds '1' for each operation which is not done in Double
  def maximizeDoubleOps(expr: Expr, typeConfig: Map[Identifier, Precision],
    goodPrecision: Precision = Float64): Rational = {
    def eval(e: Expr): (Int, Precision) = (e: @unchecked) match {
      case Variable(id) => (0, typeConfig(id))

      case Let(id, RealLiteral(_), body) =>
        eval(body)

      //case RealLiteral(_) => (costConst, constantsPrecision)

      case UMinus(t) =>
        val (cost, prec) = eval(t)
        if (prec == goodPrecision)
          (cost, prec)
        else
          (cost + 1, prec)

      case ArithOperator(Seq(lhs, rhs), recons) =>
        val (costL, precL) = eval(lhs)
        val (costR, precR) = eval(rhs)
        val newPrec = getUpperBound(precL, precR)
        if (newPrec == goodPrecision)
          (costL + costR, newPrec)
        else
          (costL + costR + 1, newPrec)

      case Let(id, value, body) =>
        // no extra cost if we need to cast
        val (costValue, precValue) = eval(value)
        val (costBody, precBody) = eval(body)
        (costValue + costBody, precBody)
    }
    Rational(eval(expr)._1)
  }

  def maximizeDoubleOpsAndVars(expr: Expr, typeConfig: Map[Identifier, Precision],
    goodPrecision: Precision = Float64): Rational = {
    def eval(e: Expr): (Int, Precision) = (e: @unchecked) match {
      case Variable(id) =>
        if (typeConfig(id) == goodPrecision) {
          (0, typeConfig(id))
        } else {
          (1, typeConfig(id))
        }
      case Let(id, RealLiteral(_), body) =>
        if (typeConfig(id) == goodPrecision) {
          eval(body)
        } else {
          val (costBody, precBody) = eval(body)
          (costBody + 1, precBody)
        }
      //case RealLiteral(_) => (costConst, constantsPrecision)

      case UMinus(t) =>
        val (cost, prec) = eval(t)
        if (prec == goodPrecision)
          (cost, prec)
        else
          (cost + 1, prec)

      case ArithOperator(Seq(lhs, rhs), recons) =>
        val (costL, precL) = eval(lhs)
        val (costR, precR) = eval(rhs)
        val newPrec = getUpperBound(precL, precR)
        if (newPrec == goodPrecision)
          (costL + costR, newPrec)
        else
          (costL + costR + 1, newPrec)

      case Let(id, value, body) =>
        // no extra cost if we need to cast
        val (costValue, precValue) = eval(value)
        val (costBody, precBody) = eval(body)
        if (typeConfig(id) == goodPrecision) {
          (costValue + costBody, precBody)
        } else {
          (costValue + costBody + 1, precBody)
        }
    }
    Rational(eval(expr)._1)
  }*/

}
