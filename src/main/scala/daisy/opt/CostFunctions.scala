
package daisy
package opt

import daisy.lang.Extractors.{ArithOperator, ElemFnc}
import daisy.lang.Identifiers._
import daisy.lang.Trees._
import daisy.lang.Types.FinitePrecisionType
import daisy.opt.ApproxPhase.TypeConfig
import daisy.tools.FinitePrecision._
import daisy.tools.Rational
import daisy.tools.Rational.zero

import scala.collection.immutable.Seq
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.math.{max, min}

trait CostFunctions {

  // lexicographic ordering on (precision, op count) pairs
  def lessThanByOpCounts(counts1: Map[Precision, Int], counts2: Map[Precision, Int]): Boolean = {
    counts1.keys.head match {
      case FloatPrecision(_) => //assuming floats and fixed are not mixed
        if (counts1(DoubleDouble) < counts2(DoubleDouble)) {
          true
        } else if (counts2(DoubleDouble) < counts1(DoubleDouble)) {
          false
        } else { // counts1 == counts2 on DoubleDouble
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
    counts += (DoubleDouble -> 0)
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
    case FinitePrecisionLiteral(_, _, _) => 0

    case Let(id, RealLiteral(_), body) => countOps(body)

    case ArithOperator(Seq(t), recons) =>
      countOps(t) + 1

    case ArithOperator(Seq(lhs, rhs), recons) =>
      countOps(lhs) + countOps(rhs) + 1

    case Let(id, value, body) =>
      countOps(value) + countOps(body)

    case Cast(e, _) => countOps(e)

    case IfExpr(cond, thenn, elze) => // if itself is not an operation
        countOps(thenn) + countOps(elze)

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
    This cost function has been determined by benchmarking individual operations.
   */
  def benchmarkedMixedPrecisionCost(expr: Expr, typeConfig: Map[Identifier, Precision]): Rational = {

    // from -> to
    def castCost(from: Precision, to: Precision) = ((from, to): @unchecked) match {
      case (Float32, Float32) => zero
      case (Float32, Float64) => zero  // Rational.fromReal(1.0157)
      case (Float32, DoubleDouble) => Rational.fromReal(2.1643)
      case (Float64, Float32) => Rational.fromReal(1.3357)
      case (Float64, Float64) => zero
      case (Float64, DoubleDouble) => Rational.fromReal(2.21)
      case (DoubleDouble, Float32) => Rational.fromReal(1.4814)
      case (DoubleDouble, Float64) => Rational.fromReal(1.6971)
      case (DoubleDouble, DoubleDouble) => zero
    }

    def plusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.6114)
      case Float64 => Rational.fromReal(1.7357)
      case DoubleDouble => Rational.fromReal(2.3343)
      //case QuadDouble => Rational.fromReal(2.3343)
    }

    def minusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.8829)
      case Float64 => Rational.fromReal(2.1457)
      case DoubleDouble => Rational.fromReal(2.3229)
      //case QuadDouble => Rational.fromReal(2.3343)
    }

    def timesCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.8029)
      case Float64 => Rational.fromReal(1.81)
      case DoubleDouble => Rational.fromReal(2.1986)
      //case QuadDouble => Rational.fromReal(2.1986)
    }

    def divCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.7657)
      case Float64 => Rational.fromReal(1.95)
      case DoubleDouble => Rational.fromReal(3.2657)
      //case QuadDouble => Rational.fromReal(3.2657)
    }

    def uminusCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(1.1714)
      case Float64 => Rational.fromReal(1.1129)
      case DoubleDouble => Rational.fromReal(1.7086)
      //case QuadDouble => Rational.fromReal(1.7086)
    }

    def sqrtCost(prec: Precision) = (prec: @unchecked) match {
      //case Float32 => Rational.fromReal(1.1714)
      case Float64 => Rational.fromReal(1.6171)
      case DoubleDouble => Rational.fromReal(6.3686)
      //case QuadDouble => Rational.fromReal(1.7086)
    }

    // TODO: these should be measured properly
    // these are chosen simply an order of magnitude bigger than arithmetic
    def transCost(prec: Precision) = (prec: @unchecked) match {
      case Float32 => Rational.fromReal(10.0)
      case Float64 => Rational.fromReal(11.0)
      case DoubleDouble => Rational.fromReal(20.0)
    }


    def eval(e: Expr): Rational = (e: @unchecked) match {

      // constant declarations
      case Let(id, RealLiteral(_), body) =>
        eval(body)

      case Let(id, Variable(_), body) =>
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

      case Variable(_) =>
        zero

      case UMinus(Variable(t)) =>
        val tPrec = typeConfig(t)
        uminusCost(tPrec)

      case Sqrt(Variable(t)) =>
        val tPrec = typeConfig(t)
        sqrtCost(tPrec)

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
      case DoubleDouble => 4
      //case QuadDouble => (8, QuadDouble)
      case FixedPrecision(16) => 1
      case FixedPrecision(32) => 2
      case FixedPrecision(64) => 4
    }

    // from -> to
    def castCost(from: Precision, to: Precision): Int = (to: @unchecked) match {
      case Float32 => 1  // TODO: this should not really happen...
      case Float64 => 1
      case DoubleDouble => 2
      //case QuadDouble => 4
      case FixedPrecision(16) => 1   // TODO: this should not really happen
      case FixedPrecision(32) => 1
      case FixedPrecision(64) => 1
    }

    // constant cost per operation, but depending on the type
    def opCost(prec: Precision): Int = (prec: @unchecked) match {
      case Float32 => 1
      case Float64 => 2
      case DoubleDouble => 4
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

      // must be a transcendental function
      case ArithOperator(Seq(y @ Variable(t)), recons) =>
        val tPrec = typeConfig(t)

        val _varCost = varCost(tPrec)
        val _opCost = transCost(tPrec)

        (_varCost + _opCost)

      case Variable(t) =>
        val tPrec = typeConfig(t)
        varCost(tPrec)

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
    //def castCost(from: Precision, to: Precision): Int = extractBits(to)
    def castCost(from: Precision, to: Precision): Int = math.abs(extractBits(from) - extractBits(to))

    // TODO : Unary cost is just en estimate it needs to be refined according to the way unary operation works
    def uminusCost(prec: Precision) = extractBits(prec)

    def transCost(prec: Precision) = 10 * extractBits(prec) // value selected based on ATVA experiments

    def timesCost(lPrec: Precision, rPrec: Precision) = {
      val (lCost, rCost) = (lPrec: @unchecked, rPrec: @unchecked) match {
        case (FixedPrecision(a), FixedPrecision(b)) => (a, b)
      }
      lCost * rCost
    }

    def eval(e: Expr): Rational = (e: @unchecked) match {

      // constant declarations
      case Let(id, RealLiteral(_), body) =>
        eval(body)

      case Let(id, FinitePrecisionLiteral(_, _, _), body) =>
        eval(body)

      case Let(id, Variable(_), body) =>
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

      case Let(id, ArithOperator(Seq(y@Variable(l), z@Cast(Variable(r),tpeR)), recons), body) =>
        val lPrec = typeConfig(l)
        val rPrec = tpeR match { case FinitePrecisionType(prec) => prec }//typeConfig(r)
        val idPrec = typeConfig(id)

        val _varCost = extractBits(lPrec) + extractBits(rPrec)
        val opPrec = getUpperBound(getUpperBound(lPrec, rPrec), idPrec)

        val _opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => extractBits(opPrec)
          case _: Minus => extractBits(opPrec)
          case _: Times => timesCost(lPrec, rPrec)
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

        (_varCost + _opCost + _castCost + eval(body) + castCost(typeConfig(r), rPrec))

      case Let(id, ArithOperator(Seq(y@Variable(l), z@Variable(r)), recons), body) =>
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

      case Let(id, value@ Cast(ArithOperator(Seq(y@Variable(l), z@Variable(r)), recons), tpe), body) =>
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

        val vPrec = tpe match {
          case FinitePrecisionType(x) => x
        }
        // downcast cast op
        if (vPrec < opPrec) {
          _castCost = _castCost + castCost(opPrec, vPrec)
        }
        // downcast assignment
        if (idPrec < vPrec) {
          _castCost = _castCost + castCost(vPrec, idPrec)
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

      case ArithOperator(Seq(y@Variable(l), z@Cast(Variable(r),tpeR)), recons) =>
        val lPrec = typeConfig(l)
        val rPrec = tpeR match { case FinitePrecisionType(prec) => prec }//typeConfig(r)
        val opPrec = getUpperBound(lPrec, rPrec)

        val _varCost = extractBits(lPrec) + extractBits(rPrec)
        val _opCost = (recons(Seq(y, z)): @unchecked) match {
          case _: Plus => extractBits(opPrec)
          case _: Minus => extractBits(opPrec)
          case _: Times => timesCost(lPrec, rPrec)
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
        (_varCost + _opCost + _castCost + castCost(typeConfig(r), rPrec))

      case Variable(_) => zero

      case UMinus(Variable(t)) =>
        val tPrec = typeConfig(t)
        (uminusCost(tPrec) + extractBits(tPrec))

      case Sqrt(Variable(t)) =>
        val tPrec = typeConfig(t)
        (transCost(tPrec) + extractBits(tPrec))

      case IfExpr(cond, thenn, elze) =>
        //eval(cond) +
        eval(thenn) + eval(elze)

      case Cast(body@ ArithOperator(Seq(y@Variable(l), z@Variable(r)), recons), tpe) =>
        val lPrec = typeConfig(l)
        val rPrec = typeConfig(r)

        val fromPrec = getUpperBound(lPrec, rPrec)
        val toPrec = tpe match {
          case FinitePrecisionType(prec) => prec
        }

        eval(body) + Rational(castCost(fromPrec, toPrec))

    }

    eval(expr)
  }

  def mlRegressionCostFunction(originalTree: Expr, expr: Expr, typeConfig: TypeConfig): Rational = {
    def getOpName(e: Expr): String = e match {
      case Plus(_, _) => "+"
      case Minus(_, _) => "-"
      case Times(_, _) => "*"
      case Division(_, _) => "/"
    }

    def getASTHeight(e: Expr): Int = e match {
      case Let(id, value, body) => getASTHeight(value) + getASTHeight(body)
      case t: Terminal => 1
      case ArithOperator(args, _) => args.map(getASTHeight).max + 1
      case IfExpr(_, thenn, elze) => max(getASTHeight(thenn), getASTHeight(elze)) + 1
      case Cast(x, _) => getASTHeight(x)
    }

    class Counter {
      var minMap = Map[String, Integer]()
      var maxMap = Map[String, Integer]()
      // (num elements, current avrg)
      var avrgMap = Map[String, (Integer, Double)]()

      def update(name: String, value: Integer): Unit = {
        updateMax(name, value)
        updateMin(name, value)
        updateAvrg(name, value)
      }

      def updateMax(name: String, value: Integer): Unit = {
        maxMap.get(name) match {
          case Some(curr) => maxMap = maxMap + (name -> max(curr, value))
          case None => maxMap = maxMap + (name -> value)
        }
      }

      def updateMin(name: String, value: Integer): Unit = {
        minMap.get(name) match {
          case Some(curr) => minMap = minMap + (name -> min(curr, value))
          case None => minMap = minMap + (name -> value)
        }
      }

      // m_new = ((n - 1.0) * m + x) / n
      def updateAvrg(name: String, value: Integer): Unit = {
        avrgMap.get(name) match {
          case Some((n, curr)) => // n is at least 1
            avrgMap = avrgMap + (name -> ((n + 1, ((n - 1.0) * curr + value) / n)))

          case None =>
            avrgMap = avrgMap + (name -> (1, value.toDouble))

        }
      }
    }

    def compute(activation: String, v: Array[Rational], nLayers: Int): Array[Rational] = activation.toUpperCase match {
      case "LOGISTIC" =>
        if (nLayers > 1)
          v.map(x => Rational.fromDouble(1 / (1 + Math.exp(-x.toDouble))))
        else
          v.map(x => Rational.min(x, zero) - Rational.fromDouble(Math.log(1 + Math.exp(-x.toDouble))))

      case "RELU" => v.map(Rational.max(_, zero))

      case "TANH" => v.map(x => Rational.fromDouble(Math.tanh(x.toDouble)))

      case "SOFTMAX" =>
        val maxEl = v.max
        val newV = v.map(x => Rational.fromDouble(Math.exp((x - maxEl).toDouble)))
        val sum = newV.foldLeft(zero)((a, x) => a.+(x))
        newV.map(_ / sum)
    }

    def predict(features: Array[Rational], activation: String = "RELU"): Rational = {
      // Parameters:
      val layers = Array(10, 20, 20, 20, 1).map(Rational.fromReal(_))
      val bias = Array(
        Array(-0.197301111871, 0.174284331353, -0.62479767815, 0.203780692992, -0.0261466221956, -1.99960992349, 0.22190172993, 0.379826835375, -0.553767354781, 0.663872096466).map(Rational.fromDouble),
        Array(0.117278557607, 0.38892001728, 0.31724307734, -0.123921426288, 0.285305741606, -0.372954296208, 0.407295175179, 0.869542766285, 0.0487017511452, 0.0709916497589, -0.172592889616, 0.952259109485, 0.43646739212, 0.0594656002804, -0.326741353266, -0.432185046356, 0.26035841993, 0.00328211287149, 0.0328842940114, -0.705949496317).map(Rational.fromDouble),
        Array(-0.0663677658794, 0.0326924521225, 0.219658119948, 0.00780745743511, 0.230673663932, 0.13203973633, 0.145919452086, -0.0186921898763, 0.185299055647, 0.558675060459, 0.637332606122, -0.182761494142, 0.0392736632062, -0.377940925096, -0.141278475016, 0.0562393468367, -0.324652017811, -0.0407595043318, 0.0316428110168, -0.39944541847).map(Rational.fromDouble),
        Array(0.536716791455, -0.00117685872671, -0.152758303785, -0.220798877633, -0.202069343484, 0.788593680104, 0.121046127096, 0.134933685895, -0.455506537158, 0.29036190151, -0.248332030153, 0.389072760128, 0.0926964394448, 0.641799905835, -0.105050389755, 0.374269471609, -0.0342593530595, 0.313552896368, 0.0619437589389, -0.358287751743).map(Rational.fromDouble),
        Array(0.966106206396).map(Rational.fromDouble))
      val weights = Array(
          Array(
            Array(-0.489533945148, -0.373832645238, 0.179890539008, -0.366101742193, -0.558478307637, -0.671390041995, -0.0310699416854, -0.3291915725, -0.174853726937, -0.0324400481851).map(Rational.fromDouble),
            Array(-0.333161108616, -0.03825334781, -0.0762616243228, -0.461165048938, 0.25551360429, 0.593344674743, -0.0886576159586, 0.0692799402001, 0.0446241838754, 0.00506615873863).map(Rational.fromDouble),
            Array(-0.113501522569, -0.215575489394, -0.187507954079, -0.0343250807321, 0.30518592468, -0.135741454613, 0.289244069785, -0.41789535908, 0.127269281028, 0.357733109212).map(Rational.fromDouble),
            Array(0.149512394967, 0.405358912527, 0.0753475159089, -0.314399290399, -0.330251110703, 0.790315522183, -0.676027728796, 0.195368648719, -0.468709060733, 0.279953552293).map(Rational.fromDouble),
            Array(0.110516993322, -0.499853637766, -0.0922954986735, -0.064432439232, 0.284894920939, -0.504692357352, 0.577373999729, 0.132442309649, 0.0584804251584, -0.075806458232).map(Rational.fromDouble),
            Array(0.14708910028, -0.148732347544, 0.0734652683703, -0.446856686416, -0.54070927395, 0.166547814357, 0.0327829724903, -0.40346076534, 0.0919892028625, -0.38403298405).map(Rational.fromDouble),
            Array(-0.0154986352644, -0.00670376347885, -0.678505192942, -0.233181847338, 0.241356297841, -0.272964712607, -0.529267963193, -0.374313997562, -0.1857883193, -0.193299569002).map(Rational.fromDouble),
            Array(-0.00186004044993, -0.587600825995, -0.0104256243906, -0.283307101592, -0.141675159156, 0.728270186189, 0.176007029056, -0.0145927014912, -0.0736717307045, -0.102157843408).map(Rational.fromDouble),
            Array(0.234721752358, -0.300507172046, -0.522382568704, 0.470709827728, -0.268355474266, 0.00592464465267, 0.0483228518543, -0.0945544524223, 0.0185474012244, -0.657978171544).map(Rational.fromDouble),
            Array(0.0395680967433, 0.194733504488, -0.468771878554, -0.00939668777092, 0.0104765115544, -0.258032218949, 1.07896685534, -0.432107442258, 0.964883126944, 0.459941277295).map(Rational.fromDouble),
            Array(-0.237682158757, -0.0743925924547, 0.968643899208, 0.0174569645458, -0.48798192088, -1.03443430208, 1.59585969963, 0.0303370489219, -0.713176824572, 0.656953332239).map(Rational.fromDouble),
            Array(-0.186112454043, 0.66605994566, 0.747251663174, 0.465070874984, -0.101282248036, 0.303982703882, 0.795487582962, 0.00575423054605, 0.276490113802, 0.822209365289).map(Rational.fromDouble),
            Array(-0.426725668898, -0.668653365153, -1.05778590467, -0.0936611459949, -0.134430805613, -2.50355702828, 1.73183618962, 0.288932356775, 0.0876293673866, 0.546301950811).map(Rational.fromDouble),
            Array(-0.191579683373, 0.0731998216826, 0.652364910999, 0.201332109545, 0.52056728757, 2.97192492259, -2.29948089326, 0.0521016434053, 0.27655909776, -1.44532229914).map(Rational.fromDouble)),
          Array(
            Array(0.164070070347, -0.033972516564, 0.146702622318, -0.194352718387, 0.323341608862, -0.287522650599, 0.0712821085728, -0.0325202920527, 0.154905231293, -0.0213509289315, 0.404662312046, 0.315903924939, 0.356368689741, -0.424264246445, -0.307247018538, -0.0206590442927, 0.20990219166, -0.128496587706, 0.0576382611054, -0.406468133098).map(Rational.fromDouble),
            Array(0.074597542144, 0.109474383467, -0.164146130339, -0.324907582739, -0.112652945894, 0.0934762111801, -0.220981468772, 0.397559400479, 0.070422573107, -0.334537612666, -0.0638393703649, -0.291279568625, 0.11485172074, -0.267351306408, 0.31379807565, 0.112072835378, -0.101115996241, -0.3409224973, 0.200179296758, 0.092705464399).map(Rational.fromDouble),
            Array(0.105309306771, 0.0831355446186, 0.105674230509, 0.225522815586, 0.129453593827, -0.526441262118, -0.0845390410448, -0.219733881377, -0.418133333807, -0.197903981881, 0.063931369433, -0.483071789877, -0.262942725587, 0.274753617136, -0.129160253068, 0.721094777556, -0.187439506848, -0.119761349397, -0.37645538123, 0.130032517735).map(Rational.fromDouble),
            Array(0.387648392563, -0.283384346384, -0.168837439398, 0.156230717247, -0.376392811735, -0.417865918582, -0.155086367521, -0.101398100908, 0.0437895042566, 0.0473341937632, -0.151910356479, -0.0664871949458, -0.221904314093, -0.265921301496, 0.248179773909, -0.322297449777, -0.0436365402497, -0.337562657732, 0.282841612147, 0.307169066807).map(Rational.fromDouble),
            Array(-0.352333895498, 0.188968178042, 0.381170700908, -0.10191790001, 0.252164913877, 0.348846612179, -0.444668347016, -0.188974319186, -0.41447208965, -0.215666956836, -0.437209809225, -0.378078638305, 0.124978958966, -0.189041057395, 0.248535879502, -0.0536034445305, 0.343684152725, -0.360539367036, 0.0347465609902, -0.149668743081).map(Rational.fromDouble),
            Array(-0.574767965445, -0.184659680132, -0.336198237873, -0.0366596438983, -0.234780570495, -0.222203241481, -0.753287895128, -1.58099311124, -0.444454058528, 0.329770359234, -0.159807912765, -0.956357719232, -0.332423190524, -0.248888292621, 0.524880222766, 0.507236746414, 0.406050274646, 0.570311359872, -0.0619791920973, 0.543316656444).map(Rational.fromDouble),
            Array(-0.346044620146, -0.113549163894, -0.122530142523, -0.422452813753, -0.355078900656, 0.202173253595, -0.315229751718, 0.275908208937, -0.116867134793, 0.254466912845, -0.166600750112, 0.19770141175, -0.506784553929, -0.261216699843, -0.426520710894, 0.632772723303, -0.221820537343, -0.334620829622, -0.19926222665, 0.0852016411861).map(Rational.fromDouble),
            Array(0.188533776512, 0.0797618667692, 0.123288058424, 0.0927900550717, -0.113199024982, -0.155621021288, 0.0531457147348, 0.167987501043, 0.374865300479, 0.288837175284, 0.0459383604436, 0.267989644039, -0.423936243275, 0.266061062206, -0.269012934037, 0.185108521127, 0.283645140693, -0.165286585336, -0.089647977297, 0.421300250481).map(Rational.fromDouble),
            Array(-0.244952361947, 0.234898472834, -0.084128357487, -0.160590079058, 0.223930502474, -0.192100538111, 0.165431119961, -0.0912029083212, 0.289594173887, 0.228854228183, -0.00211168647571, 0.15110122414, 0.376481267627, 0.225618281531, -0.519453234248, 0.606834794772, -0.193610658224, 0.109936464153, -0.085754169172, 0.201237538289).map(Rational.fromDouble),
            Array(-0.379754216376, -0.267301388994, 0.00948760817946, 0.0258564767064, 0.0315010700461, -0.165318513955, -0.390565170707, -0.0269595612357, -0.059272304309, -0.274133990972, -0.367437364936, 0.524796393396, 0.366047668618, -0.395610010828, -0.404027944836, 0.219311709672, 0.053070637042, 0.358659249462, 0.0150469327805, 0.108207915558).map(Rational.fromDouble)),
          Array(
            Array(0.0659189833646, -0.266470096242, -0.210804424754, 0.0518952406426, 0.0275639231485, -0.243989101944, 0.294161266757, 0.161178252137, 0.229616090094, -0.070434336308, 0.0207054661785, 0.167099625898, -0.37269907116, 0.030453137458, 0.0377244664847, 0.135249964947, -0.170835894117, -0.0697547710244, 0.339384807335, -0.184212565564).map(Rational.fromDouble),
            Array(0.154808908732, -0.182872305118, 0.0366251501061, 0.271664125173, 0.225499073821, -0.364588310414, -0.0268334800545, -0.0744515740687, 0.310482827028, -0.327126809133, -0.0286268055948, 0.388269509738, -0.348757462115, 0.13224102317, 0.184997411597, -0.350605953899, -0.205841585624, -0.0274951222268, -0.232211032541, 0.310625049257).map(Rational.fromDouble),
            Array(0.167416199712, -0.308633803453, 0.289529782414, -0.169877904666, -0.0834181286992, 0.0926657214528, 0.00893126617402, 0.0300610783096, 0.342026508823, -0.0681986142773, 0.0265296783925, -0.230809704044, -0.358679828061, 0.0160554415061, -0.311384482491, 0.111572677796, -0.205073972196, 0.00572127767367, 0.244095291944, 0.133098653763).map(Rational.fromDouble),
            Array(-0.05630003425, 0.140808402996, -0.140668831934, 0.316872889661, -0.147169865777, 0.0894715504285, -0.0412372386595, -0.346957992979, -0.073811088866, 0.0692366120715, -0.0205950618714, 0.338642649482, -0.287988024479, 0.194671638309, 0.0950428770181, -0.111859550761, 0.0376364998102, -0.288093368728, 0.0721399029269, -0.280693385624).map(Rational.fromDouble),
            Array(0.0693283256763, 0.256361320927, -0.0727230957939, -0.345067401007, -0.214872453658, 0.0620781498397, 0.0911644192476, 0.3481834754, 0.306158528537, -0.146833860612, -0.273454835588, 0.252027545347, 0.0200370708365, 0.360446499845, -0.332763367589, -0.116673381708, 0.176277214744, 0.0438035511514, 0.24476022527, 0.195280962686).map(Rational.fromDouble),
            Array(-0.250907946144, -0.339742689413, -0.272052057391, 0.179850490258, -0.245910209595, 0.249779926475, 0.381431126531, -0.340390841073, -0.0899681678572, -0.316980592353, -0.115740582738, -0.280766887169, -0.137595413798, 0.166143778588, -0.272982447816, 0.158182637939, -0.187393550916, -0.262919746553, 0.124495965241, 0.241717868145).map(Rational.fromDouble),
            Array(0.223139688695, -0.276700279202, 0.00830071478626, -0.203557381986, 0.189138157365, -0.317619517902, -0.265612249615, -0.313137672187, -0.166373844522, 0.0435461887167, -0.0890889371073, 0.120812197466, -0.0650824517781, 0.369601141792, 0.0415491301028, 0.181429346639, -0.262204846624, 0.43249150054, -0.192984180928, 0.0899558696146).map(Rational.fromDouble),
            Array(-0.157259667694, -0.319266563148, -0.462286219537, -0.142744379515, -0.277811014263, -0.275516848123, -0.0823781927565, 0.733731994144, -0.336278404152, 0.529844482238, 0.562544483957, -0.269625788227, 0.93201104736, -0.101807107926, -0.326053410494, -0.253817770789, -0.49801404287, -0.195805647545, -0.33882687998, -0.27754419703).map(Rational.fromDouble),
            Array(-0.265714658469, 0.201281434613, 0.1381215856, 0.280429468216, -0.016830210263, -0.198821545549, 0.201553174875, -0.239705442853, -0.0907107381576, 0.0950875005142, 0.104402127401, -0.0140759739833, 0.10568035455, -0.223943176093, -0.0147199224944, -0.143226696864, -0.255695079564, -0.333093310442, -0.230251284913, -0.0720413584355).map(Rational.fromDouble),
            Array(-0.130780402633, -0.207833276997, 0.167982832907, -0.351977892522, 0.106711948971, -0.0334039492596, -0.291207595178, -0.336386119199, -0.0358259686465, -0.57091098333, 0.166738413387, 0.421991248564, -0.0415220702742, 0.325291041376, 0.258441948846, -0.287831105164, 0.175781293552, 0.190912152683, -0.429126488873, -0.243810453038).map(Rational.fromDouble),
            Array(0.188110041737, -0.100361229238, 0.24296348402, 0.364453720983, 0.0351082827116, -0.371139970955, -0.222911035906, 0.140857374422, 0.208867402409, -0.322637807137, 0.136962637647, 0.0628772577428, -0.267921698952, -0.32153747939, 0.344017175632, 0.277135091383, -0.139973928464, -0.0821292883775, -0.301190385667, -0.239120139272).map(Rational.fromDouble),
            Array(-0.136865304473, 0.113288201238, -0.214070371507, 0.0966708150657, 0.116696377557, 0.037908321718, -0.127410037215, -0.170651310888, -0.0223341418218, 0.598462133561, 0.849200031887, -0.179630217633, 0.557652442197, -0.063787318567, -0.369689086647, -0.279362024769, -0.279262827454, -0.211963752125, -0.0655053183042, -0.33618769258).map(Rational.fromDouble),
            Array(-0.382576634302, 0.180622421965, 0.315296971945, -0.158485300079, -0.0774486553559, 0.350811232135, -0.0509717546171, -0.0258698782149, -0.335017607445, -0.0424743855757, 0.106747626304, -0.209433645512, -0.0736122743444, -0.340162489323, 0.078672178366, 0.156511173303, -0.373738958715, -0.0147502661789, -0.320975734287, 0.382940721996).map(Rational.fromDouble),
            Array(-0.0512559680102, -0.000787655979597, -0.0279617042955, 0.332871556123, 0.103934041973, 0.176862949067, 0.0882517065522, 0.0168087360562, 0.240645884526, 0.259015522161, 0.0719558075441, 0.0122630974024, -0.0684689742581, 0.00985386823315, -0.34380177032, -0.179852390197, 0.199575835161, 0.321785926329, -0.378049800655, -0.374116109298).map(Rational.fromDouble),
            Array(0.299349567878, 0.249345733947, -0.133602179693, 0.432268630987, -0.219731886709, -0.150213750397, 0.289381837649, -0.126503530797, 0.458175350474, -0.409075251613, 0.143093600064, 0.299908328482, -0.271255076007, -0.220718674867, -0.211301434707, -0.00528516139592, -0.195447863153, 0.142402579963, 0.285955437695, 0.309339621444).map(Rational.fromDouble),
            Array(-0.315778028736, -0.192038728437, 0.450764187698, -0.0485224257905, 0.150612133304, -0.324710016648, -0.218317509144, -0.30453471928, 0.338770291411, 0.0957401917626, -0.520703939881, -0.760622410031, -0.0787316184899, -0.0470757503538, 0.475153581098, -0.105775555469, 0.285147793088, -0.160576041612, 0.118757982909, -0.133962693423).map(Rational.fromDouble),
            Array(-0.0323992313682, 0.324713949786, -0.0951562638943, 0.0381210806345, -0.18317094865, -0.141868164821, -0.16635924905, -0.189231727682, 0.325717924904, -0.119390676941, 0.179396437255, 0.207340127311, 0.159176256871, -0.0138849566956, -0.279052995064, -0.149432086889, -0.241699470866, -0.375035873369, 0.0766859743369, -0.00610911924852).map(Rational.fromDouble),
            Array(-0.277273745652, -0.322060512166, -0.19736982477, 0.152838103515, -0.166753431856, -0.0433744894692, -0.123970912276, -0.0435994780899, 0.0131582537654, -0.304793904771, -0.112492104217, 0.32866384474, 0.201686864252, -0.103993869456, 0.143537105642, -0.00524742953172, -0.0897034983816, 0.297824748054, -0.0592212647895, 0.138494108338).map(Rational.fromDouble),
            Array(-0.218090687992, -0.11986284824, -0.0214789505464, 0.0804861887194, 0.180625922347, 0.262395170285, 0.161619104686, 0.0865026459356, -0.0156558500259, -0.0775952894895, 0.188409276641, -0.0218852277875, -0.253416902506, -0.189399331608, -0.201556569162, 0.240060315593, 0.38514369225, 0.236120330734, 0.158930923634, 0.0707768783629).map(Rational.fromDouble),
            Array(-0.218614651657, -0.251037355237, 0.0998780530417, -0.12600113596, -0.660453595242, -0.217040577206, 0.395471174222, -0.396706810559, 0.322760311007, -0.348267072563, -0.444915272496, -0.119338074725, -0.0911220192423, -0.412962166286, 0.324035163746, 0.210284152307, 0.0705312121783, 0.112268901257, -0.356647019879, 0.274074545619).map(Rational.fromDouble)),
          Array(
            Array(0.232500374475, -0.288100249542, -0.223345930982, -0.108932029894, 0.0950716233404, -0.153857990918, 0.0358007996668, 0.0389517444191, -0.156595867622, -0.163289277917, 0.330973418452, -0.125523757529, 0.269641407612, 0.295291383339, 0.199865413435, -0.138572803701, 0.330581698126, -0.217762049529, 0.329681076174, 0.22006286145).map(Rational.fromDouble),
            Array(0.0684255452062, 0.274630044473, -0.119000247296, -0.236082961314, -0.375003191356, -0.104918294705, -0.0492696128102, 0.175963682366, -0.242146623037, -0.0761256694432, -0.0284268926499, 0.267899908127, -0.125919137655, 0.00485723332762, -0.331238273687, -0.23713223263, -0.218042502037, -0.0972120648469, 0.189540698029, 0.0185926978094).map(Rational.fromDouble),
            Array(-0.346286152516, -0.101753806124, 0.364395138222, -0.0231773863236, -0.224489595402, -0.334555639282, 0.26400487409, -0.349883940356, -0.106932361914, 0.249316513874, -0.202066219741, -0.215216865041, 0.0549209080078, -0.468685699036, 0.169164469655, 0.141007560757, 0.35943108863, -0.193746011651, -0.0824279047731, -0.343899369739).map(Rational.fromDouble),
            Array(0.395203157762, -0.331594192741, 0.319045513407, -0.331621092034, -0.0840741700508, 0.00690901232922, 0.0401658400228, -0.0783570661279, 0.223797136246, 0.233734701639, 0.0648527878963, 0.274362597332, -0.360784959091, 0.0608479126598, -0.0421595919247, 0.396934059845, -0.267290994215, -0.213104811826, -0.0245427692823, -0.327940325028).map(Rational.fromDouble),
            Array(-0.0549954584198, -0.150590690347, 0.214122633755, 0.031709293864, -0.358417135185, 0.24796980495, 0.210896550937, 0.19437123969, 0.072701614424, 0.179689702759, -0.321957232799, 0.350723221601, -0.189310566789, 0.371654225448, -0.0413440020593, 0.261511569834, -0.0860338025079, 0.235658417678, -0.0275612828134, -0.0688525342387).map(Rational.fromDouble),
            Array(-0.0888255941484, 0.328049510472, 0.280325872514, 0.0207005026393, -0.177065663104, 0.0328057981457, 0.143482459937, 0.093039592985, 0.0822958206765, 0.206101587688, -0.182492239301, 0.249960435081, -0.154919410806, 0.317382775125, -0.13355360402, -0.192862666306, 0.215328550822, -0.29944145702, 0.199942803884, 0.0175054359458).map(Rational.fromDouble),
            Array(0.317887192532, -0.222415900398, -0.0715571357485, 0.553164186833, 0.224521157121, -0.359546346109, -0.165489599789, -0.437554581119, 0.336066962283, -0.313674316502, 0.0110150913604, 0.0948872090699, 0.342449248688, 0.0203849835546, 0.120112190444, -0.274076224369, 0.0539176989328, -0.240777624972, -0.276312247353, -0.345507587774).map(Rational.fromDouble),
            Array(0.158428106047, -0.160889879056, 0.36858628254, 0.212643012197, 0.146317780886, 0.144576352695, 0.0629374131935, -0.331574060343, 0.276572903563, 0.00949460089706, 0.0882736617762, 0.260382111471, 0.220919221278, -0.139996073384, 0.0356097998221, 0.144848583841, -0.0994011393855, 0.159722224213, -0.301976060638, 0.160714856017).map(Rational.fromDouble),
            Array(0.265258067788, 0.293445523884, 0.180807372034, 0.189754084128, -0.228813301596, 0.088977908367, 0.172527967138, 0.354448074262, -0.229315706166, -0.088935778603, -0.254327428948, -0.632657227617, -0.148085329559, -0.446055758738, -0.292680141277, 0.111594231907, 0.113142764605, 0.296842162736, -0.368356737113, -0.0614459441315).map(Rational.fromDouble),
            Array(0.357171938548, 0.0824788411555, -0.179120678508, -0.25358716278, 0.25509166641, 0.422198530158, -0.327083366806, -0.140088728429, -0.255373206046, -0.0274328453853, 0.172358883344, 0.630358328676, 0.147970269698, 0.327856198759, -0.00627091411741, 0.282584308398, -0.250533534725, -0.00948445607492, 0.0572606156334, -0.125060703102).map(Rational.fromDouble),
            Array(0.31800245162, -0.316509969585, -0.320005471512, 0.133512113223, 0.241283388615, 0.451310766485, 0.0648107815537, 0.109248586447, 0.0635301669392, 0.508476888932, 0.191881207246, 1.21198751212, 0.407998323029, 0.382992036438, 0.201059310028, 0.324256620685, -0.0977954441281, -0.207310565693, 0.0062935055545, -0.302786481979).map(Rational.fromDouble),
            Array(0.27747713675, -0.122323878028, 0.380574695546, -0.116567746465, 0.262556298826, 0.265621302392, -0.257280968955, -0.377971561592, -0.152476360549, 0.218621688721, 0.0143933055091, -0.0763053219728, -0.0175313979349, 0.0237640064151, -0.379983864074, 0.326324148832, -0.0587107137368, -0.198068910371, -0.278200329374, 0.372811988452).map(Rational.fromDouble),
            Array(0.117544009858, 0.24105729824, -0.213623129061, -0.491027588489, 0.0410495499933, 0.345055707297, 0.0187774788694, -0.456063938773, 0.348519308142, 0.139885843448, -0.385410663676, 0.434622341315, -0.0593287326856, 0.501338451645, 0.275668074537, 0.270367327229, -0.226628931405, -0.100828782527, -0.323223542566, 0.0192004057221).map(Rational.fromDouble),
            Array(-0.192174982706, 0.238291517553, -0.0324505522282, 0.0679083872571, -0.211773132285, -0.258934804512, -0.306443247717, -0.257560997605, -0.367001941287, -0.229673166931, 0.442108705769, 0.254438352099, 0.259966151962, -0.234583577372, 0.292832718518, -0.155196137428, -0.241578275697, -0.0893276668984, -0.118795228879, -0.309249378894).map(Rational.fromDouble),
            Array(-0.547755351091, 0.216404790427, 0.0419060564716, -0.263687223095, -0.381458139741, -0.227063350101, 0.220005382483, 0.382305457496, 0.184586296694, 0.145313716773, 0.394513007431, -0.463455286255, 0.305737411206, -0.198550326266, 0.139419588466, 0.181005122811, -0.0619649647995, 0.359531684369, 0.121048126018, 0.198379371503).map(Rational.fromDouble),
            Array(0.310603768462, -0.199193391313, -0.239465470647, -0.14871254505, -0.0264998067791, -0.139341888919, -0.247933487075, -0.239949929152, 0.131090965379, -0.27518438035, 0.0586008743968, 0.132013402448, 0.315747563726, -0.3052259584, -0.14706742103, 0.261374097257, 0.0627600179228, -0.0511944051326, 0.248378478074, -0.341930008342).map(Rational.fromDouble),
            Array(-0.344344467532, -0.315864711265, 0.161269914319, 0.102447736066, 0.287419437098, 0.19587377808, 0.176369520201, -0.229392538458, 0.0386924493211, -0.138562677561, -0.258006207509, -0.0117243729587, 0.13392117906, -0.127261812077, 0.342019259537, 0.168521687775, 0.0194343035543, -0.0457686031288, -0.239412250012, 0.16890290284).map(Rational.fromDouble),
            Array(0.18899320515, 0.0474751418278, 0.186338651665, 0.224251002883, 0.196840277441, 0.206760158501, -0.0629435290013, -0.00161079953965, 0.388560075143, 0.232517705527, 0.349912265424, 0.234916905246, -0.357300862539, -0.112750523958, -0.22212387944, -0.422628952905, -0.0167071793747, 0.166170786285, -0.146172014028, -0.103948865965).map(Rational.fromDouble),
            Array(-0.139223865686, -0.0200841062502, -0.187786788974, -0.147428404758, 0.156289295788, 0.35508761423, -0.311222412586, 0.126825081483, 0.027110754289, -0.243881772491, 0.21354526377, 0.106036934441, -0.0068654364539, 0.0500721196021, -0.286160502068, 0.253955375986, 0.143797100711, 0.324550838913, -0.118716732035, 0.0610079598677).map(Rational.fromDouble),
            Array(-0.250434627032, -0.255045505328, -0.191017261133, -0.167482750782, -0.128915180253, -0.010556963512, -0.23243374564, -0.197200731848, 0.326040749716, 0.128370433023, 0.125079389959, 0.066410386857, -0.192245705838, 0.348829500295, 0.269587712016, 0.164985506429, 0.389024898537, 0.0774514230504, -0.389366875863, -0.0977419827396).map(Rational.fromDouble)),
          Array(
            Array(0.852612445555).map(Rational.fromDouble),
            Array(-0.239631292578).map(Rational.fromDouble),
            Array(0.0993512634447).map(Rational.fromDouble),
            Array(0.0157976932638).map(Rational.fromDouble),
            Array(-0.24576673162).map(Rational.fromDouble),
            Array(0.768058479763).map(Rational.fromDouble),
            Array(0.0516534608655).map(Rational.fromDouble),
            Array(-0.477806813025).map(Rational.fromDouble),
            Array(-0.540319054602).map(Rational.fromDouble),
            Array(0.207557898659).map(Rational.fromDouble),
            Array(-0.49388611587).map(Rational.fromDouble),
            Array(1.18977840571).map(Rational.fromDouble),
            Array(0.313320136234).map(Rational.fromDouble),
            Array(0.86861831662).map(Rational.fromDouble),
            Array(-0.185845528368).map(Rational.fromDouble),
            Array(0.359917421024).map(Rational.fromDouble),
            Array(0.156252208364).map(Rational.fromDouble),
            Array(0.0677183229882).map(Rational.fromDouble),
            Array(-0.17279872602).map(Rational.fromDouble),
            Array(0.482763160008).map(Rational.fromDouble)))


      val tmp = new Array[Array[Rational]](layers.length + 1)
      val network = tmp.indices.map(i => {
        if (i == 0) features
        else {
          tmp.update(i, Array.fill(layers(i - 1).toInt) { zero })
          tmp(i)
        }
      }).toArray

      for {i <- 0 until (network.length - 1)} {
        for {j <- network(i + 1).indices} {
          for {l <- network(i).indices} {
            network(i + 1)(j) += network(i)(l) * weights(i)(l)(j)
          }
          network(i + 1)(j) += bias(i)(j)
        }
        if ((i + 1) < (network.length - 1))
          network(i + 1) = compute(activation, network(i + 1), network.length)
      }

      network(network.length - 1)(0)
    }

    def extractFeatures(expr: Expr, originalTree: Expr, typeConfig: TypeConfig): Array[Rational] = {

      val counter = new Counter()
      lang.TreeOps.postTraversal({
        // last statement
        case Let(id, a@ArithOperator(Seq(_, _), _), b@ArithOperator(Seq(Variable(lid), Variable(rid)), _)) =>
          val bits = typeConfig(id) match {
            case FixedPrecision(totalBits) => totalBits
            case x => throw new Exception("unknown type: " + x)
          }
          counter.update(getOpName(a), bits)

        val bitLeft = typeConfig(lid) match {
          case FixedPrecision(totalBits) => totalBits
          case x => throw new Exception("unknown type: " + x)
        }
        val bitRight = typeConfig(rid) match {
          case FixedPrecision(totalBits) => totalBits
          case x => throw new Exception("unknown type: " + x)
        }
        counter.update(getOpName(b), max(bitLeft, bitRight))

        case Let(id, a@ArithOperator(Seq(_, _), _), _) =>
          val bits = typeConfig(id) match {
            case FixedPrecision(totalBits) => totalBits
            case x => throw new Exception("unknown type: " + x)
          }
          counter.update(getOpName(a), bits)

        case Let(id, a@ElemFnc(_), _) =>
          val bits = typeConfig(id) match {
            case FixedPrecision(totalBits) => totalBits
            case x => throw new Exception("unknown type: " + x)
          }
          counter.update("*", bits) // we add the operator to + and *
          counter.update("+", bits)

        case x =>
        // for now count only binary ops
      })(expr)

      val features = ArrayBuffer[Double]()
      // extract min, max, avrg operator info
      for (op <- Seq("+", "-", "*", "/")) { //
        counter.minMap.get(op) match {
          case Some(count) =>
            features.append(count.toDouble)
            features.append(counter.maxMap(op).toDouble)
            features.append(counter.avrgMap(op)._2)
          case None =>
            features.appendAll(Array(0.0, 0.0, 0.0))
        }
      }

      // the compiler may do CSE, so we should do it here too
      val bodyCSE = transform.CompilerOptimizationPhase.commonSubexpressionElimination(expr)

      val ast_height = getASTHeight(expr)
      features.append(ast_height.toDouble)

      val ast_height_cse = getASTHeight(bodyCSE)
      features.append(ast_height_cse.toDouble)
      features.toArray.map(Rational.fromDouble)
    }

    // Features:
    val features = extractFeatures(expr, originalTree, typeConfig)
    predict(features)
  }

  def combinedCost(originalTree: Expr, expr: Expr, typeConfig: TypeConfig): Rational = {
    areaBasedCostFunction(expr, typeConfig) + mlRegressionCostFunction(originalTree, expr, typeConfig)*Rational(10000)
  }


}
