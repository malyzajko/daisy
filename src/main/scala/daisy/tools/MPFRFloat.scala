// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import org.kframework.mpfr._

import scala.math.{ScalaNumber}

object MPFRFloat {

  // if this is changed in some way, make sure you check all the places
  // where MPFRFloats have been used!
  val context = BinaryMathContext.BINARY128  // TODO, BinaryMathContext seems immutable
  val roundUpContext = new BinaryMathContext(128, java.math.RoundingMode.CEILING)
  val roundDownContext = new BinaryMathContext(128, java.math.RoundingMode.FLOOR)
  // val context = new BinaryMathContext(500, 24)
  // val context = BinaryMathContext.BINARY128
  def fromString(s: String): MPFRFloat = new MPFRFloat(new BigFloat(s, context))
  // def fromInt(i: Integer): MPFRFloat = new MPFRFloat(new BigFloat(i, context))

  def fromDouble(d: Double): MPFRFloat = new MPFRFloat(new BigFloat(d, context))

  val Pi: MPFRFloat = new MPFRFloat(BigFloat.pi(context))
  val E: MPFRFloat = new MPFRFloat(BigFloat.e(context))

  val zero: MPFRFloat = MPFRFloat.fromDouble(0.0)
  val one: MPFRFloat = MPFRFloat.fromDouble(1.0)
  val two: MPFRFloat = MPFRFloat.fromDouble(2.0)

  def sqrt(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sqrt(context))
  def sqrtDown(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sqrt(roundDownContext))
  def sqrtUp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sqrt(roundUpContext))

  def cbrt(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.cbrt(context))

  def log(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.log(context))
  def logDown(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.log(roundDownContext))
  def logUp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.log(roundUpContext))

  def exp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.exp(context))
  def expDown(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.exp(roundDownContext))
  def expUp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.exp(roundUpContext))

  def pow(x: MPFRFloat, y: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.pow(y.bf, context))
  def powDown(x: MPFRFloat, y: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.pow(y.bf, roundDownContext))
  def powUp(x: MPFRFloat,  y: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.pow(y.bf,roundUpContext))

  def powerTwo(n: Int): MPFRFloat = {
    val exp = n.toString
    pow(two, fromString(exp)) //TODO: Check if this needs pow/pow_up/pow_down
  }

  def cos(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.cos(context))

  def sin(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sin(context))
  def sinDown(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sin(roundDownContext))
  def sinUp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sin(roundUpContext))

  def sec(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sec(context))

  def csc(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.csc(context))

  def cot(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.cot(context))

  def tan(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.tan(context))

  def acos(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.acos(context))

  def acosDown(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.acos(roundDownContext))
  def acosUp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.acos(roundUpContext))

  def asin(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.asin(context))
  def asinDown(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.asin(roundDownContext))
  def asinUp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.asin(roundUpContext))

  def atan(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.atan(context))
  def atanDown(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.atan(roundDownContext))
  def atanUp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.atan(roundUpContext))


  def sinh(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sinh(context))

  def cosh(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.cosh(context))

  def tanh(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.tanh(context))

  def abs(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.abs)

  def max(x: MPFRFloat, y: MPFRFloat): MPFRFloat = new MPFRFloat(BigFloat.max(x.bf, y.bf, context))

  def min(x: MPFRFloat, y: MPFRFloat): MPFRFloat = new MPFRFloat(BigFloat.min(x.bf, y.bf, context))
}


/*
  A wrapper around the Java wrapper around the multiprecision C floats.
 */
class MPFRFloat(val bf: BigFloat) extends ScalaNumber with Ordered[MPFRFloat] {
  import MPFRFloat._


  def unary_-(): MPFRFloat = new MPFRFloat(bf.negate)
  def +(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.add(that.bf, context))
  def up_+(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.add(that.bf, roundUpContext))
  def down_+(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.add(that.bf, roundDownContext))

  def -(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.subtract(that.bf, context))
  def up_-(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.subtract(that.bf, roundUpContext))
  def down_-(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.subtract(that.bf, roundDownContext))

  def *(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.multiply(that.bf, context))
  def up_*(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.multiply(that.bf, roundUpContext))
  def down_*(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.multiply(that.bf, roundDownContext))

  def /(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.divide(that.bf, context))
  def up_/(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.divide(that.bf, roundUpContext))
  def down_/(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.divide(that.bf, roundDownContext))

  def %(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.remainder(that.bf, context))
  def up_%(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.remainder(that.bf, roundUpContext))
  def down_%(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.remainder(that.bf, roundDownContext))

  def isNaN: Boolean = bf.isNaN
  def isInfinite: Boolean = bf.isInfinite

  def toLongString: String = bf.toString
  override def toString: String = this.toDoubleString

  def toDoubleString: String = {
    val longString = bf.toString.split('.')
    // assert(longString.length == 2, "failure: " + longString.toList)
    if (longString.length == 1) {  // sth like 0e+00
      longString(0)
    } else if (longString.length == 2) {
      val decimalParts = longString(1).split("e")
      assert(decimalParts.length == 1 || decimalParts.length == 2)
      if (decimalParts.length == 1) {
        longString(0) + "." + decimalParts(0).take(17)
      } else {
        longString(0) + "." + decimalParts(0).take(17) + "e" + decimalParts(1)
      }
    } else {
      throw new Exception("longString not 1 or 2 as expected, but " + longString.length)
    }
  }
  def integerPart: Int = intValue()
  def longPart: Long = longValue()


  // Members declared in java.lang.Number
  def doubleValue(): Double = bf.doubleValue
  def floatValue(): Float = bf.floatValue
  def intValue(): Int = bf.intValue
  def longValue(): Long = bf.longValue

  // Members declared in scala.math.Ordered
  def compare(that: daisy.tools.MPFRFloat): Int = {
    Rational.fromString(this.toString).compare(Rational.fromString(that.toString)) //TODO: It seems that MPFRFloat compare method is broken!
    //this.bf.compareTo(that.bf)
  }

  // Members declared in scala.math.ScalaNumber
  def isWhole(): Boolean = new BigFloat(bf.intValue, context).equals(bf)
  def underlying(): Object = ???  // not sure what this is supposed to be

  override def equals(other: Any): Boolean = other match {

    case x: MPFRFloat => this.compare(x) == 0

    case _ => this.bf.equals(other)
  }

  override def hashCode(): Int = bf.hashCode
}