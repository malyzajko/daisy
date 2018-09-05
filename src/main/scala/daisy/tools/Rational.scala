// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.math.{ScalaNumber, ScalaNumericConversions}
import java.math.BigInteger

import scala.language.implicitConversions
import Rational._

object Rational {

  private val oneBigInt = new BigInt(new BigInteger("1"))
  private val zeroBigInt = new BigInt(new BigInteger("0"))
  private val twoBigInt = new BigInt(new BigInteger("2"))

  private val MAX_INT = new BigInt(new BigInteger(Int.MaxValue.toString))
  private val MAX_LONG = new BigInt(new BigInteger(Long.MaxValue.toString))

  private val bits32 = 32


  implicit def double2Rational(d: Double): Rational = fromReal(d)

  def apply(n: Int): Rational = Rational(new BigInt(new BigInteger(n.toString)), oneBigInt)

  def apply(n: Long, d: Long): Rational = {
    Rational(new BigInt(new BigInteger(n.toString)), new BigInt(new BigInteger(d.toString)))
  }

  // This constructor does all the canonicalisation and only this constructor
  // should actually create the objects.
  def apply(n: BigInt, d: BigInt): Rational = {
    // 0/0 -> 0/1
    if (n == 0 && d == 0) {
      new Rational(zeroBigInt, oneBigInt)
    }
    else if (d == 0) {
      throw DivisionByZeroException("Zero denominator is not allowed.")
    }
    else {
      // reduce fraction
      val (num, den) = formatFraction(n, d)
      // only nominator can be negative
      if(den < 0) {
        new Rational(-num, -den)
      } else {
        new Rational(num, den)
      }
    }
  }

  def fromDouble(dbl: Double): Rational = {
    val (n, d) = double2Fraction(dbl)
    Rational(n, d)
  }

  def powerTwo(exp: Int): Rational = {
    if (exp < 0) {
      Rational(oneBigInt, twoBigInt.pow(-exp))
    } else {
      Rational(twoBigInt.pow(exp), oneBigInt)
    }
  }

  def fromReal(dbl: Double): Rational = {
    val (n, d) = real2Fraction(dbl.toString)
    Rational(n, d)
  }

  def fromString(s: String): Rational = {
    val (n, d) = real2Fraction(s)
    Rational(n, d)
  }

  val zero = Rational(zeroBigInt, oneBigInt)
  val one = Rational(oneBigInt, oneBigInt)
  val two = Rational(twoBigInt, oneBigInt)


  // This computes an approximation of the square root (only), since sqrt(x)
  // is not necessarily a rational number.
  def sqrtUp(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.sqrtUp(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }
  def sqrtDown(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.sqrtDown(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }

  def sineBounded(x: Rational): (Rational, Rational) = {
    (sineDown(x), sineUp(x))
  }

  def sineUp(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.sinUp(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }

  def sineDown(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.sinDown(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }

  def expUp(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.expUp(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }
  def expDown(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.expDown(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }

  def logUp(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.logUp(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }
  def logDown(x: Rational): Rational = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.logDown(MPFRFloat.fromDouble(dblValue)).doubleValue)
    Rational.fromDouble(mpfrRes)
  }


  /*
    Constructors for rationals.
  */
  /*def apply(dbl: Double): Rational = {    // ambiguous
    val (n, d) = double2Fraction(dbl)
    Rational(n, d)
  }*/

  // Takes a string representing a real value and returns a fraction equal to it.
  // We assume this string comes directly from variable.toString, so it does not
  // have trailing zeroes, always has one decimal point, etc.
  // This works because of a property of the IEEE 754 standard that requires that
  // one can recover the exact string by going to double and back.
  private def real2Fraction(value: String): (BigInt, BigInt) = {
    // scientific notation
    if (value.contains("e") || value.contains("E")) {
      val splitExponent = value.split(Array('e', 'E'))

      val nom = new BigInt(new BigInteger(splitExponent(0).replace(".", "")))

      val splitDecimal = splitExponent(0).split('.') // this could be just 5 or 0

      val denPower = if (splitDecimal.length == 1) {
        // case e.g. 0e+00
        -splitExponent(1).toInt
      } else {
        // case 3.4e-5
        splitDecimal(1).length - splitExponent(1).toInt
      }

      // println("denPower: " + denPower)
      if (denPower > 0) {
        val den = new BigInt(new BigInteger("10").pow(denPower))
        (nom, den)
      } else {
        val den = new BigInt(new BigInteger("1"))
        val newNom = nom * new BigInt(new BigInteger("10").pow(-denPower))
        (newNom, den)
      }
    }
    // decimal notation
    else if (value.contains('.')) {
      // but not things like '3.' (those are integers and we don't like this notation)
      assert(!value.endsWith("."))

      // nominator is simply all digits without the decimal sign
      val nom = new BigInt(new BigInteger(value.replace(".", "")))
      val parts = value.split('.')

      // the denominator is 10^(number of decimal places)
      // Eva: I can't think of a better way to construct this, we cannot use
      // Longs or Ints as they may overflow
      val numDecPlaces = parts(1).length
      val den = new BigInt(new BigInteger(
        // a '1' with as many zeroes as there are decimal places
        "1" + Array.fill[String](numDecPlaces)("0").mkString("")
        ))

      (nom, den)
    }
    // integer
    else {
      (new BigInt(new BigInteger(value)), oneBigInt)
    }

  }

  // How to get the different parts is taken from the Java API documentation
  // of longBitsToDouble
  // This however, only converts FLOATING-POINT values, not the real values
  // you get from the String representation.
  def double2Fraction(double: Double): (BigInt, BigInt) = {
    val bits: Long = java.lang.Double.doubleToLongBits(double)

    if (bits == 0x7ff0000000000000L) {
      throw new ArithmeticException("cannot convert +infinity to a fraction")
    }
    if (bits == 0xfff0000000000000L) {
      throw new ArithmeticException("cannot convert -infinity to a fraction")
    }
    if (bits == 0x7ff8000000000000L) {
      throw new ArithmeticException("cannot convert NaN to a fraction")
    }

    val sign = if ((bits >> 63) == 0) 1 else -1

    val exponent = ((bits >> 52) & 0x7ffL).toInt

    val mantissa =
      if (exponent == 0) {
        (bits & 0xfffffffffffffL) << 1
      } else {
        (bits & 0xfffffffffffffL) | 0x10000000000000L
      }
    val trueExponent = exponent - 1075

    if (trueExponent > 0) {
      (twoBigInt.pow(trueExponent) * mantissa * sign, oneBigInt)
    } else {
      (new BigInt(new BigInteger((sign * mantissa).toString)), twoBigInt.pow(-trueExponent))
    }
  }


  val MIN_INT_RATIONAL = 1.0 / Int.MaxValue
  val MIN_LONG_RATIONAL = 1.0 / Long.MaxValue

  /**
    Creates a new rational number where the nominator and denominator consists
    of 32 bit integers. The number will be smaller than the original.
    This will throw and exception if the number is too large to fit into an Int.
   */
  def scaleToIntsDown(r: Rational): Rational = {

    // Too large
    if (math.abs(r.toDouble) > Int.MaxValue) {
      throw new RationalCannotBeCastToIntException(
        "Rational too big to be cast to integer rational.")
    }
    // Too small
    if (math.abs(r.toDouble) < MIN_INT_RATIONAL) {
      // Underflow
      if (r < zero) {
        Rational(-1L, Int.MaxValue.toLong)
      } else {
        zero
      }
    } else {

      val num = r.n.abs
      val den = r.d

      // Already small enough
      if (num < Int.MaxValue && den < Int.MaxValue) {
        r
      } else {

        val divN = if (num.bitLength < bits32) oneBigInt else num / MAX_INT + oneBigInt
        val divD = if (den.bitLength < bits32) oneBigInt else den / MAX_INT + oneBigInt
        val div = divN.max(divD)
        val res =
          if (r.toDouble > 0.0) {// Rounding down, so num round down, den round up
            val nn = num / div
            val dd = den / div + oneBigInt
            Rational(nn, dd)
          } else { // Rounding up, so num rnd up, den rnd down
            val nn = num / div + oneBigInt
            val dd = den / div
            Rational(-nn, dd)
          }
        assert(res.toDouble <= r.toDouble, "\nres (" + res.toDouble +
          ") is larger than before \n   (" + r.toDouble)
        res
      }
    }
  }

  /**
    Creates a new rational number where the nominator and denominator consists
    of 32 bit integers. The returned number is bigger than the original.
    This will throw and exception if the number is too large to fit into an Int.
   */
  def scaleToIntsUp(r: Rational): Rational = {
    // Too large
    if (math.abs(r.toDouble) > Int.MaxValue) {
      throw new RationalCannotBeCastToIntException(
        "Rational too big to be cast to integer rational.")
    }
    // Too small
    if (math.abs(r.toDouble) < MIN_INT_RATIONAL) {
      // Underflow
      if (r < zero) {
        Rational(-1L, Int.MaxValue.toLong)
      } else {
        zero
      }
    } else {

      val num = r.n.abs
      val den = r.d

      // Already small enough
      if (num.abs < Int.MaxValue && den < Int.MaxValue) {
        r
      } else {

        val divN = if (num.bitLength < bits32) oneBigInt else num / MAX_INT + oneBigInt
        val divD = if (den.bitLength < bits32) oneBigInt else den / MAX_INT + oneBigInt
        val div = divN.max(divD)

        val res =
          if (r.toDouble > 0.0) {// Rounding up, so num round up, den round down
            val nn = num / div + oneBigInt
            val dd = den / div
            Rational(nn, dd)
          } else {
            val nn = num / div
            val dd = den / div + oneBigInt
            Rational(-nn, dd)
          }
        assert(res.toDouble >= r.toDouble, "\nres (" + res.toDouble +
          ") is larger than before\n    (" + r.toDouble)
        res
      }
    }
  }

  def scaleToLongUp(r: Rational): Rational = {
    // Too large
    if (math.abs(r.toDouble) > Long.MaxValue) {
      throw new RationalCannotBeCastToIntException(
        "Rational too big to be cast to long integer rational.")
    }
    // Too small
    if (math.abs(r.toDouble) < MIN_LONG_RATIONAL) {
      // Underflow
      if (r < zero) {
        Rational(-1L, Long.MaxValue)
      } else {
        zero
      }
    } else {

      val num = r.n.abs
      val den = r.d

      // Already small enough
      if (num.abs < Long.MaxValue && den < Long.MaxValue) {
        r
      } else {

        val divN = if (num.bitLength < bits32) oneBigInt else num / MAX_LONG + oneBigInt
        val divD = if (den.bitLength < bits32) oneBigInt else den / MAX_LONG + oneBigInt
        val div = divN.max(divD)

        val res =
          if (r.toDouble > 0.0) {// Rounding up, so num round up, den round down
            val nn = num / div + oneBigInt
            val dd = den / div
            Rational(nn, dd)
          } else {
            val nn = num / div
            val dd = den / div + oneBigInt
            Rational(-nn, dd)
          }
        assert(res.toDouble >= r.toDouble, "\nres (" + res.toDouble +
          ") is larger than before\n    (" + r.toDouble)
        res
      }
    }
  }

  def abs(r: Rational): Rational = {
    if (r.n < 0) {
      new Rational(-r.n, r.d)
    } else {
      r
    }
  }

  def max(x: Rational, y: Rational): Rational = {
    if (x >= y) x else y
  }

  def min(x: Rational, y: Rational): Rational = {
    if (x < y) x else y
  }

  /* def maxAbs(nums: Seq[Rational]): Rational = nums match {
    case Seq(n) => abs(n)
    case _ => max(abs(nums.head), maxAbs(nums.tail))
  } */


  private def formatFraction(n: BigInt, d: BigInt): (BigInt, BigInt) = {
    val g = gcd(n.abs, d.abs)
    (n / g, d / g)
  }

  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }

  private val mathContext = new java.math.MathContext(64, java.math.RoundingMode.HALF_EVEN)
  private val mathContextUp = new java.math.MathContext(64, java.math.RoundingMode.UP)

  def niceDoubleString(d: Double): String = {
    // TODO: is there a library function maybe for this?
    def removeTrailingZeroes(s: String): String = {
      if (s.last == '0' && s.init.last != '.') { removeTrailingZeroes(s.init) } else { s }
    }
    val numString = "%.18g".format(d)
    numString.split('e') match {
      case Array(digitsOnly, exponent) =>
        removeTrailingZeroes(digitsOnly) + "e" + exponent
      case Array(digitsOnly) =>
        removeTrailingZeroes(digitsOnly)
    }
  }

  def limitSize(r: Rational): Rational =
    if (r.n.bitLength > 100) {
    Rational.fromDouble(r.doubleValue())
    } else r
}

/**
 * Rational number class.
 * The constructor is private so that only rationals in canonical form can be
 * created.
 */
class Rational private(val n: BigInt, val d: BigInt) extends ScalaNumber with ScalaNumericConversions
  with Ordered[Rational] {

  assert(d > 0, "Rational denominator negative! " + d)  // number can be negative only through nominator
  assert(math.abs(gcd(n, d).toLong) == 1, "Rational not reduced %d / %d!".format(n, d))  // fraction is reduced

  def unary_-(): Rational = Rational(-n, d)
  def +(other: Rational): Rational = {
    // This improves running time on really big testcases.  This new
    // implementation automatically computes + and - using the
    // smallest possible denominator. Instead of computing 1/3 + 1/6 =
    // 6/18 + 3/18 = 9/18 and then reducing this fraction, this
    // implementation would compute 1/3 + 1/6 = 2/6 + 1/6 and then
    // reduce this fraction.
    val gcdDen = gcd(d, other.d)
    if (gcdDen == 1) {
      Rational(n * other.d + other.n * d, d * other.d)
    } else {
      val lcm_den_s = d * other.d / gcdDen
      val lcm_den = if (lcm_den_s < 0) -lcm_den_s else lcm_den_s

      val res_n = n * (lcm_den / d) + other.n * (lcm_den / other.d)
      Rational(res_n, lcm_den)
    }
  }
  def -(other: Rational): Rational = {
    val gcdDen = gcd(d, other.d)
    if(gcdDen == 1) {
      Rational(n * other.d - other.n * d, d * other.d)
    } else {
      val lcm_den_s = d * other.d / gcdDen
      val lcm_den = if (lcm_den_s < 0) -lcm_den_s else lcm_den_s

      val res_n = n * (lcm_den / d) - other.n * (lcm_den / other.d)
      Rational(res_n, lcm_den)
    }
  }
  def *(other: Rational): Rational = Rational(n * other.n, d * other.d)
  def /(other: Rational): Rational = Rational(n * other.d, d* other.n)
  def ^(exp: Int): Rational = Rational(n.pow(exp), d.pow(exp))

  override def toString: String = niceDoubleString(this.toDouble)

  def toLongString: String = {
    val bigN = new java.math.BigDecimal(n.bigInteger, mathContext)
    val bigD = new java.math.BigDecimal(d.bigInteger, mathContext)
    bigN.divide(bigD, mathContext).toString
  }
  def toFractionString: String = "(%s)/(%s)".format(n.toString, d.toString)

  def integerPart: Int = doubleValue.toInt
  def longPart: Long = doubleValue.toLong

  def compare(other: Rational): Int = {
    val xNom = this.n * other.d
    val yNom = other.n * this.d
    xNom.compare(yNom)
  }

  def roundToInt: Int = {
    if (n >= zeroBigInt) { // positive number
      if (this - Rational(this.integerPart) < Rational(1L, 2L)) {
        this.integerPart
      } else {
        this.integerPart + 1
      }
    } else {
      if (this - Rational(this.integerPart) > Rational(1L, 2L)) {
        this.integerPart
      } else {
        this.integerPart - 1
      }
    }
  }

  def roundToLong: Long = {
    if (n >= zeroBigInt) { // positive number
      if (this - Rational(this.longPart, 1L) < Rational(1L, 2L)) {
        this.longPart
      } else {
        this.longPart + 1
      }
    } else {
      if (this - Rational(this.longPart, 1L) > Rational(1L, 2L)) {
        this.longPart
      } else {
        this.longPart - 1
      }
    }
  }

  // round value towards 0
  def toBigInt: BigInt = {
    n / d
  }

  lazy val isPowerOf2: Boolean = {
    if (n != 1 && d != 1) {
      false
    } else {
      var intPart = if (n == 1) d else n
      var divisibleByTwo = true
      while (divisibleByTwo && intPart != 1) {
        if (intPart % 2 != 0) {
          divisibleByTwo = false
        } else {
          intPart = intPart / 2
        }
      }
      divisibleByTwo
    }
  }

  override def equals(other: Any): Boolean = other match {
    case x: Rational => this.compare(x) == 0
    case x: Double => this.toDouble == x  // TODO: not ideal
    case x: Short => this.compare(Rational(x)) == 0
    case x: Char => this.compare(Rational(x)) == 0
    case x: Byte => this.compare(Rational(x)) == 0
    case x: Int => this.compare(Rational(x)) == 0
    case x: Float => this.toFloat == x
    case x: Long => this.toLong == x
    case _ => false
  }

  override def hashCode(): Int = {
    this.doubleValue.hashCode
  }

  override def byteValue(): Byte = Predef.double2Double(doubleValue).byteValue
  override def doubleValue(): Double = {
    val bigN = new java.math.BigDecimal(n.bigInteger, mathContext)
    val bigD = new java.math.BigDecimal(d.bigInteger, mathContext)
    val res = bigN.divide(bigD, mathContextUp)
    res.doubleValue
  }
  override def floatValue(): Float = {// Predef.double2Double(doubleValue).floatValue
    val bigN = new java.math.BigDecimal(n.bigInteger, mathContext)
    val bigD = new java.math.BigDecimal(d.bigInteger, mathContext)
    val res = bigN.divide(bigD, mathContextUp)
    res.floatValue
  }
  override def intValue(): Int = Predef.double2Double(doubleValue).intValue
  override def isValidByte: Boolean = false
  override def isValidChar: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidShort: Boolean = false
  override def longValue(): Long = Predef.double2Double(doubleValue).longValue
  override def shortValue(): Short = Predef.double2Double(doubleValue).shortValue
  override def toByte: Byte = doubleValue.toByte
  override def toChar: Char = doubleValue.toChar
  override def toDouble: Double = doubleValue
  override def toFloat: Float = doubleValue.toFloat
  override def toInt: Int = doubleValue.toInt
  override def toLong: Long = doubleValue.toLong
  override def toShort: Short = doubleValue.toShort
  def underlying(): AnyRef = this
  override def isWhole(): Boolean = d == 1.0
}
