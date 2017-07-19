

package daisy
package utils

import org.kframework.mpfr._

import scala.math.{ScalaNumber}   //ScalaNumericConversions

object MPFRFloat {

  // if this is changed in some way, make sure you check all the places
  // where MPFRFloats have been used!
  val context = BinaryMathContext.BINARY128  // TODO, BinaryMathContext seems immutable
  //val context = new BinaryMathContext(500, 24)
  //val context = BinaryMathContext.BINARY128

  def fromString(s: String): MPFRFloat = new MPFRFloat(new BigFloat(s, context))
  def fromDouble(d: Double): MPFRFloat = new MPFRFloat(new BigFloat(d, context))

  val Pi: MPFRFloat = new MPFRFloat(BigFloat.pi(context))
  val E: MPFRFloat = new MPFRFloat(BigFloat.e(context))

  def sqrt(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sqrt(context))
  def cbrt(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.cbrt(context))
  def log(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.log(context))
  def exp(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.exp(context))
  def pow(x: MPFRFloat, y: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.pow(y.bf, context))
  def cos(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.cos(context))
  def sin(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sin(context))
  def sec(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.sec(context))
  def csc(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.csc(context))
  def cot(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.cot(context))
  def tan(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.tan(context))
  def acos(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.acos(context))
  def asin(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.asin(context))
  def atan(x: MPFRFloat): MPFRFloat = new MPFRFloat(x.bf.atan(context))
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
  import MPFRFloat.context

  def unary_-():MPFRFloat = new MPFRFloat(bf.negate)
  def +(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.add(that.bf, context))
  def -(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.subtract(that.bf, context))
  def *(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.multiply(that.bf, context))
  def /(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.divide(that.bf, context))

  def %(that: MPFRFloat): MPFRFloat = new MPFRFloat(this.bf.remainder(that.bf, context))

  def isNaN: Boolean = bf.isNaN
  def isInfinite: Boolean = bf.isInfinite

  def toLongString: String = bf.toString
  override def toString: String = this.toDoubleString

  def toDoubleString: String = {
    val longString = bf.toString.split('.')
    //assert(longString.length == 2, "failure: " + longString.toList)
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
      null
    }
  }

  // Members declared in java.lang.Number
  def doubleValue(): Double = bf.doubleValue
  def floatValue(): Float = bf.floatValue
  def intValue(): Int = bf.intValue
  def longValue(): Long = bf.longValue

  // Members declared in scala.math.Ordered
  def compare(that: daisy.utils.MPFRFloat): Int = this.bf.compareTo(that.bf)

  // Members declared in scala.math.ScalaNumber
  def isWhole(): Boolean = new BigFloat(bf.intValue, context).equals(bf)
  def underlying(): Object = ???  // not sure what this is supposed to be

  override def equals(any: Any) = this.bf.equals(any)
  override def hashCode(): Int = bf.hashCode
}