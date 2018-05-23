// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

object FinitePrecision {

  def getUpperBound(precs: Precision*): Precision = precs.max

  sealed abstract class Precision extends Ordered[Precision] {
    val range: Interval

    /* Computes the worst-case roundoff error of the given range */
    protected def _absRoundoff(i: Interval): Rational
    protected def _absTranscendentalRoundoff(i: Interval): Rational

    /* Public method that can be modified using mix-ins */
    def absRoundoff(i: Interval): Rational = {
      if (!range.includes(i)) {
        throw OverflowException("Potential overflow detected. Error bound cannot be computed.")
      }
      _absRoundoff(i)
    }
    final def absRoundoff(r: Rational): Rational = absRoundoff(Interval(r))

    /* Public method that can be modified using mix-ins */
    final def absTranscendentalRoundoff(i: Interval): Rational = {
      if (!range.includes(i)) {
        throw OverflowException("Potential overflow detected. Error bound cannot be computed.")
      }
      _absTranscendentalRoundoff(i)
    }
    final def absTranscendentalRoundoff(r: Rational): Rational = absTranscendentalRoundoff(Interval(r))

    override def compare(that: Precision): Int = (this,that) match {
      case (FloatPrecision(a), FloatPrecision(b)) => a - b
      case (FixedPrecision(a), FixedPrecision(b)) => a - b
        //if (a == b) 0 else throw new Exception("mixed-precision currently unsupported for fixed-points")
      case (FloatPrecision(_), FixedPrecision(_)) |
           (FixedPrecision(_), FloatPrecision(_)) => throw new Exception("comparing incompatible precisions")
    }

    def canRepresent(r: Rational): Boolean
  }

  sealed abstract class FloatPrecision(val order: Int) extends Precision {

    val machineEpsilon: Rational

    override def _absRoundoff(i: Interval): Rational = machineEpsilon * Interval.maxAbs(i)
    def _absTranscendentalRoundoff(i: Interval): Rational = _absRoundoff(i) * 2    // 1 ulp instead of 1.5 ulp

    val mantissa_bits, exponent_bits: Int

    // A number is representable in a given precision if the denominator is a power of two, and the numerator uses at
    // most as many bits as the mantissa can store, while the exponents is in the right range.
    def canRepresent(r: Rational): Boolean = {
      val nominator = r.n.abs
      val denominator = r.d.abs

      if (denominator.bitCount > 1) {
        false // denominator not a power of 2
      } else if (nominator.bitLength - nominator.lowestSetBit > mantissa_bits + 1 /* implicit bit */){
        false // don't have enough accuracy
      } else {
        // mantissa = nominator / 2^nominator.lowestSetBit ∈ [1,2)
        // exponent = log2(2^nominator.lowestSetBit / denominator)
        //          = nominator.lowestSetBit - log2(denominator)
        val exponent = nominator.lowestSetBit - (denominator.lowestSetBit-1)
        val max_exp = (1 << (exponent_bits-1)) - 1
        val min_exp = - max_exp + 1
        min_exp <= exponent && exponent <= max_exp
      }
    }
  }

  object FloatPrecision {
    def unapply(arg: FloatPrecision): Option[Int] = Some(arg.order)
  }

  sealed trait DenormalCheck extends FloatPrecision {
    val minNormal: Rational
    private lazy val denormalRange = Interval.+/-(minNormal)
    val denormalsError: Rational

    override def absRoundoff(i: Interval): Rational = {
      if (denormalRange.includes(i) && i != Interval.zero) {
        //throw DenormalRangeException("Range containing only denormals detected. " +
        //  "Error bound cannot be computed")
        denormalsError
      } else {
        super.absRoundoff(i)
      }
    }
  }

  // IEEE half-precision
  case object Float16 extends FloatPrecision(16) with DenormalCheck {

    override val machineEpsilon: Rational = Rational.powerTwo(-11)

    override val (mantissa_bits, exponent_bits): (Int, Int) = (10, 5)

    override val range: Interval = Interval.+/-(Rational(65504))

    override val minNormal: Rational = Rational.powerTwo(-14)

    override val denormalsError: Rational = Rational.powerTwo(-25)
  }

  case object Float32 extends FloatPrecision(32) with DenormalCheck {

    override val machineEpsilon: Rational = Rational.powerTwo(-24)

    override val (mantissa_bits, exponent_bits): (Int, Int) = (23, 8)

    override val range: Interval = Interval.+/-(Float.MaxValue.toDouble)

    override val minNormal: Rational = java.lang.Float.MIN_NORMAL.toDouble

    override val denormalsError: Rational = Rational.powerTwo(-150)

    // PERFORMANCE: this may not be the fastest way
    // also potential error: math.ulp(float.minNormal)/2 = 0.0
    override def _absRoundoff(i: Interval): Rational =
      Rational.fromDouble(math.ulp(1.0.floatValue)/2)*Interval.maxAbs(i)
      //Rational.fromDouble(math.ulp(Math.nextUp(Interval.maxAbs(i).floatValue))) / Rational(2)
  }

  case object Float64 extends FloatPrecision(64) with DenormalCheck {

    override val machineEpsilon: Rational = Rational.powerTwo(-53)

    override val (mantissa_bits, exponent_bits): (Int, Int) = (52, 11)

    override val range: Interval = Interval.+/-(Double.MaxValue)

    override val minNormal: Rational = java.lang.Double.MIN_NORMAL

    override val denormalsError: Rational = Rational.powerTwo(-1075)

    override def _absRoundoff(i: Interval): Rational =
      Rational.fromDouble(math.ulp(1.0)/2)*Interval.maxAbs(i)
      //Rational.fromDouble(math.ulp(Math.nextUp(Interval.maxAbs(i).doubleValue))) / Rational(2)

  }

  case object DoubleDouble extends FloatPrecision(128) {

    override val toString = "Quad"

    override val machineEpsilon: Rational = Rational.powerTwo(-113)

    lazy override val (mantissa_bits, exponent_bits): (Int, Int) = ???

    override val range: Interval = Interval.+/-(Double.MaxValue)

    override def canRepresent(r: Rational): Boolean = false
    // 2.0041683600089728e-292 = 2^(-1022 + 53) = 2^(-969)
    // override val minNormal: Rational = Rational.powerTwo(-969)
  }

  case object QuadDouble extends FloatPrecision(256) {

    override val machineEpsilon: Rational = Rational.powerTwo(-211)

    lazy override val (mantissa_bits, exponent_bits): (Int, Int) = ???

    override val range: Interval = Interval.+/-(Double.MaxValue)

    override def canRepresent(r: Rational): Boolean = false
    // 1.6259745436952323e-260 = 2^(-1022 + 3*53) = 2^(-863)
    // override val minNormal: Rational = Rational.powerTwo(-863)
  }

  object FixedPrecision {
    // Returns the number of bits needed to represent the given integer.
    def integerBitsNeeded(r: Rational): Int = {
      val value = Rational.abs(r).integerPart

      // Integer.num...(n) returns 0 for all negative numbers, and >= 1 for all positive numbers.
      // Using 33 so bitsNeeded(2^a-1) = a+1, where we need the extra bit to represent -(2^a-1) (+bonus -2^a)
      33 - Integer.numberOfLeadingZeros(value)
    }
  }

  /*
    Represents a fixed-point arithmetic precision.
    Supports a signed format with truncation as the rounding mode.
   */
  case class FixedPrecision(bitlength: Int) extends Precision {
    import FixedPrecision._
    override val toString = "Fixed" + bitlength

    override def _absRoundoff(i: Interval): Rational = Rational.powerTwo(-fractionalBits(i))
    override def _absTranscendentalRoundoff(i: Interval): Rational = ???

    def fractionalBits(i: Interval): Int = fractionalBits(Interval.maxAbs(i))
    def fractionalBits(r: Rational): Int = bitlength - integerBitsNeeded(r)

    // Range is -2^(bitlength-1), 2^(bitlength-1) - 1
    override val range: Interval =
      Interval(-Rational.powerTwo(bitlength-1), Rational.powerTwo(bitlength-1) - Rational(1))

    // TODO
    override def canRepresent(r: Rational) = false
  }
}
