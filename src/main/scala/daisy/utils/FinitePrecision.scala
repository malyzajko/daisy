
package daisy
package utils

import Rational.{max, abs, double2Fraction}
import java.math.BigInteger
//import math.BigInt.{abs => bigAbs}

object FinitePrecision {

  // Does not check for overflow, assumes this has been done before,
  // a number is representable in a given precision if the denominator is a power of two,
  // and the mantissa and exponents are within the precision's allowed ones.
  def isExactInFloats(r: Rational, prec: Precision): Boolean = {
    // if it's an integer, it's definitely representable
    if (r.isWhole) {
      true
    } else {
      prec match {
        case DoubleDouble | QuadDouble => false
        case Fixed(_) => false
        case Float32 | Float64 =>

        val nominator = r.n.abs
        val denominator = r.d.abs

        val (nomBound, denomBound) = (prec: @unchecked) match {
          case Float32 =>
            // 2^23 - 1, 2^8 -1
            (8388607l, 255l)
          case Float64 =>
            // 2^52 - 1, 2^11 -1
            (4503599627370496l, 2047l)
        }

        if (nominator <= nomBound && denominator <= denomBound) {
          val exponent: Double = math.log(denominator.toDouble) / math.log(2)

          if (exponent.isWhole) {
            // maybe the log computations isn't sound due to roundoffs, let's sanity check:
            assert(math.pow(2, exponent) == denominator)
            true
          } else {
            false
          }

        } else {
          false
        }
      }
    }
  }

  private def allPrec: List[Precision] = List(Float32, Float64, DoubleDouble, QuadDouble)

  def getUpperBound(lhs: Precision, rhs: Precision): Precision = (lhs, rhs) match {
    case (Fixed(a), Fixed(b)) if (a == b) => lhs
    case (Fixed(a), Fixed(b)) =>
      throw new Exception("mixed-precision currently unsupported for fixed-points")
    case _ =>
      if (allPrec.indexOf(lhs) <= allPrec.indexOf(rhs)) {
        rhs
      } else {
        lhs
      }
  }

  sealed abstract class Precision extends Ordered[Precision] {
    /* The range of values that are representable by this precision. */
    def range: (Rational, Rational)

    /* The smallest (absolute) value representable by this precision,
       for floating-point precisions, it's the smallest normal (and not denormal) */
    //def minNormal: Rational

    /* Computes the worst-case roundoff error of the given value */
    def absRoundoff(r: Rational): Rational

    /* Computes the worst-case roundoff error of the given range of values. */
    def absRoundoff(i: Interval): Rational = {
      absRoundoff(max(abs(i.xlo), abs(i.xhi)))
    }
  }

  case object Float32 extends Precision {
    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Float.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }
    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(java.lang.Float.MIN_NORMAL)
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }
    def absRoundoff(r: Rational): Rational = {
      // PERFORMANCE: this may not be the fastest way
      Rational.fromDouble(math.ulp(Rational.abs(r).floatValue)/2)
    }

    def compare(that: Precision): Int = that match {
      case Float32 => 0
      case Float64 => -1
      case DoubleDouble => -1
    }
  }

  case object Float64 extends Precision {
    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Double.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }

    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(java.lang.Double.MIN_NORMAL)
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }

    def absRoundoff(r: Rational): Rational = {
      Rational.fromDouble(math.ulp(Rational.abs(r).doubleValue)/2)
    }

    def compare(that: Precision): Int = that match {
      case Float32 => 1
      case Float64 => 0
      case DoubleDouble => -1
    }
  }

  case object DoubleDouble extends Precision {
    val doubleDoubleEps = Rational(new BigInt(new BigInteger("1")), new BigInt(new BigInteger("2")).pow(105))

    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Double.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }
    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(math.pow(2, -969))
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }
    // 2.0041683600089728e-292;  // = 2^(-1022 + 53) = 2^(-969)

    def absRoundoff(r: Rational): Rational = {
      doubleDoubleEps * Rational.abs(r)
    }

    def compare(that: Precision): Int = that match {
      case Float32 => 1
      case Float64 => 1
      case DoubleDouble => 0
    }
  }

  case object QuadDouble extends Precision {
    val quadDoubleEps = Rational(new BigInt(new BigInteger("1")), new BigInt(new BigInteger("2")).pow(211))

    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Double.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }
    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(math.pow(2, -863))
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }
     //1.6259745436952323e-260; // = 2^(-1022 + 3*53) = 2^(-863)

    def absRoundoff(r: Rational): Rational = {
      quadDoubleEps * Rational.abs(r)
    }
    def compare(that: Precision): Int = ???
  }

  /*
    Represents a fixed-point arithmetic precision.
    Supports a signed format with truncation as the rounding mode.
  */
  case class Fixed(bitlength: Int) extends Precision {
    // TODO: is this correct?
    val range: (Rational, Rational) =
      (Rational(-math.pow(2, bitlength - 1).toLong, 1l),
        Rational(math.pow(2, bitlength - 1).toLong - 1, 1l))

    //val minNormal: Rational = ???

    def absRoundoff(r: Rational): Rational = {
      val fracBits = fractionalBits(r)
      Rational(1, math.pow(2, fracBits).toLong)
    }

    def fractionalBits(i: Interval): Int = {
      fractionalBits(max(abs(i.xlo), abs(i.xhi)))
    }

    def fractionalBits(r: Rational): Int = {
      val intBits = bitsNeeded(math.abs(r.integerPart))
      bitlength - intBits
    }

    /**
     Returns the number of bits needed to represent the given integer.
     @param 32-bit integer
     */
    private def bitsNeeded(value: Int): Int = {
      assert(value >= 0)
      // TODO: don't we have to also subtract 1 for the sign?
      32 - Integer.numberOfLeadingZeros(value)
    }
    def compare(that: Precision): Int = that match {
      case Fixed(x) => bitlength.compare(x)
    }
  }

}