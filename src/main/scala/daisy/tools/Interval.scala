// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import Rational._

object Interval {

  def maxAbs(i: Interval): Rational = max(abs(i.xlo), abs(i.xhi))

  def minAbs(i: Interval): Rational = {
    assert(!i.includes(Rational.zero))
    min(abs(i.xlo), abs(i.xhi))
  }

  def minAbsWithZero(i: Interval): Rational = {
    if(i.includes(Rational.zero)) Rational.zero
    else min(abs(i.xlo), abs(i.xhi))
  }

  def apply(r: Rational): Interval = Interval(r, r)
  def apply(i: Interval): Interval = i

  def +/-(r: Rational) = Interval(-r,r)

  val zero: Interval = Interval(0)

  // TODO good precision?
  val pi = Interval(Rational.fromString("3.141592653589793238"),
                    Rational.fromString("3.141592653589793239"))
}

case class PartialInterval(xlo: Option[Rational], xhi: Option[Rational])

case class Interval(xlo: Rational, xhi: Rational) extends RangeArithmetic[Interval] {
  assert(xlo <= xhi, "interval lower bound cannot be bigger than upper bound")

  import Interval.pi

  // private val zero = Rational(0.0)

  // def this(aa: RationalForm) = this(aa.interval._1, aa.interval._2)
  // def this(aa: FixedForm) = this(aa.qInterval._1, aa.qInterval._2)
  // def this(i: Interval) = this(Rational(i.xlo), Rational(i.xhi))

  val mid: Rational = xlo/two + xhi/two
  val width: Rational = abs(xhi - xlo)
  val radius: Rational = width / two

  def isPointRange: Boolean = xlo == xhi

  // for compatibility with RangeArithmetic
  def toInterval: Interval = this

  /* Adds an error of magnitude r */
  def +/-(r: Rational): Interval = this + Interval(-r, r)

  def addConstraint(e: Set[lang.Trees.Expr]): Interval = this

  def unary_- = Interval(-xhi, -xlo)

  def +(other: Interval): Interval = {
    Interval(xlo + other.xlo, xhi + other.xhi)
  }

  def -(other: Interval): Interval = {
    Interval(xlo - other.xhi, xhi - other.xlo)
  }

  def *(y: Interval): Interval = y match {
    case Interval(ylo, yhi) =>
      // TODO: test via unit test that this works and that we need this
      if (xlo == zero && xhi == zero) {
        Interval(zero, zero)
      } else if (xlo >= zero) {
        if (ylo >= zero) {
          Interval(xlo * ylo, xhi * yhi)
        } else if (yhi <= zero) {
          Interval(xhi * ylo, xlo * yhi)
        } else {
          Interval(xhi * ylo, xhi * yhi)
        }
      }
      else if (xhi <= zero) {
        if (ylo >= zero) {
          Interval(xlo * yhi, xhi * ylo)
        } else if (yhi <= zero) {
          Interval(xhi * yhi, xlo * ylo)
        } else {
          Interval(xlo * yhi, xlo * ylo)
        }
      }
      else {
        if (ylo >= zero) {
          Interval(xlo * yhi, xhi * yhi)
        } else if (yhi <= zero) {
          Interval(xhi * ylo, xlo * ylo)
        } else {
          val a = min(xlo * yhi, xhi * ylo)
          val b = max(xlo * ylo, xhi * yhi)
          Interval(a, b)
        }
      }
  }

  // the lazy version
  def *(r: Rational): Interval = this * Interval(r, r)

  // 0/ 0 is undefined, and will also throw a DivisionByZeroException
  def /(y: Interval): Interval = y match {
    case Interval(ylo, yhi) =>

      // if(xlo == zero && ylo == zero) Interval(zero, zero)

      if (ylo >= zero) {
        if (xlo >= zero) {
          Interval(xlo / yhi, xhi/ ylo)
        } else if (xhi <= zero) {
          Interval(xlo / ylo, xhi / yhi)
        } else {
          Interval(xlo / ylo, xhi / ylo)
        }
      }
      else if (yhi <= zero) {
        if (xlo >= zero) {
          Interval(xhi / yhi, xlo / ylo)
        } else if (xhi <= zero) {
          Interval(xhi / ylo, xlo / yhi)
        } else {
          Interval(xhi / yhi, xlo / yhi)
        }
      } else {
        throw DivisionByZeroException("trying to divide by interval containing 0")
      }

  }

  def ^(n: Int): Interval = {
    if (n % 2 == 0 && includes(zero)) {
      // even powers non-monotonic over - / +
      Interval(zero, Interval.maxAbs(this) ^ n)
    } else if (n % 2 == 0 && xhi < zero) {
      // even powers monotonically falling over - / -
      Interval(xhi ^ n, xlo ^ n)
    } else {
      // odd powers, and even powers over + / + monotonically rising
      Interval(xlo ^ n, xhi ^ n)
    }
  }

  /**
   * Compares 2 intervals by width
   * @param i2 the second interval to compare with
   * @return
   */
  def >(i2: Interval): Boolean = {
    this.width > i2.width
  }

  /**
   * Checks whether 2 intervals are equal
   * @param y - interval to compare with
   * @return true/ false
   */
  def equals(y: Interval): Boolean = {
    this.xlo == y.xlo && this.xhi == y.xhi
  }

  def inverse: Interval = Interval(one, one) / this

  def squareRoot: Interval = {
    if (xlo < zero) {
      throw NegativeSqrtException("Trying to take the sqrt of a negative number!")
    }
    Interval(sqrtDown(xlo), sqrtUp(xhi))
  }

  def union(y: Interval): Interval = {
    Interval(min(this.xlo, y.xlo), max(this.xhi, y.xhi))
  }

  def toBigString: String = "[%.18g, %.18g]".format(xlo.toDouble, xhi.toDouble)
  override def toString: String = "[%s, %s]".format(xlo.toDouble, xhi.toDouble)

  // divide the initial interval for two and return the tuple
  def getBinaryDivision: (Interval, Interval) = (Interval(this.xlo, this.mid), Interval(this.mid, this.xhi))

  /**
   * Divides the interval to predefined number of subintervals
   * @param limit - amount of subintervals to get
   * @return list of subintervals
   */
  def divide(limit: Integer): List[Interval] = {
    val step = this.width./(Rational(limit))
    List.tabulate(limit)(n => Interval(this.xlo +
      (step * (Rational(n))), this.xlo + (step * (Rational(n + 1)))))
  }

  /**
   * Extends interval by predefined parameter.
   * E.g. [0].extendBy(0.1)
   * will return interval [-0.1, 0.1]
   * @param parameter positive value for which the bounds of original interval will be changed
   * @return extended interval
   */
  def extendBy(parameter: Rational): Interval =
    Interval(this.xlo - (parameter), this.xhi + (parameter))

  /** Reduces the Interval bounds to [-pi, pi]
   *
   * Note that the resulting values form not necessarily an interval.
   *
   * It also returns the indices of the quarters of the sine wave in which the
   * end points are.
   */
  def reduceRange() : ((Rational, Rational), (BigInt, BigInt)) = {
    // lower bound >= 0 -> subtract over-approximation of pi...
    // lower bound < 0 -> subtract under-approximation of pi...
    // ... and for the widened interval add the opposite one
    val piForLo = if (xlo >= 0.0) (pi.xhi, pi.xlo) else (pi.xlo, pi.xhi)

    // upper bound >= 0 -> subtract under-approximation of pi...
    // upper bound < 0 -> subtract over-approximation of pi...
    // ... and for the widened interval add the opposite one
    val piForHi = if (xhi >= 0.0) (pi.xlo, pi.xhi) else (pi.xhi, pi.xlo)

    // Returns the representative y of x in [-b, b] and the number of times 2*b
    // has been subtracted from x to obtain y.
    // TODO this becomes inaccurate for intervals that are far from 0 because of
    // the imprecision of the rational pi
    def reduceTo(x: Rational, b: Rational) = {
      val k = Rational((x / b).toBigInt, 1)
      val sign = if (k >= 0) 1 else -1
      val l = Rational(((k + sign) / 2).toBigInt, 1)
      val res = x - l * 2 * b
      (res, l)
    }

    val (rLo, rLoFact) = reduceTo(xlo, piForLo._1)
    val (rHi, rHiFact) = reduceTo(xhi, piForHi._1)

    // find an over-approximation of any interval that could lead to the
    // resulting values with the given uncertainty of pi
    val rounded = Interval(rLo + rLoFact * 2 * piForLo._2, rHi + rHiFact * 2 * piForHi._2)

    val piLo = if (rounded.xlo >= 0.0) pi.xhi else pi.xlo
    val piHi = if (rounded.xhi >= 0.0) pi.xlo else pi.xhi

    // computes n such that n*(pi/2) <= rounded.x__ <= (n+1)*(pi/2)
    var posLo = ((rounded.xlo * 2) / piLo).toBigInt
    if (rounded.xlo < 0) posLo -= 1
    var posHi = ((rounded.xhi * 2) / piHi).toBigInt
    if (rounded.xhi < 0) posHi -= 1
    // has to be based on rounded (instead of this) as the imprecision of pi can
    // make the bounded values be in different quarters of the sine wave as they
    // should be, resulting in unsound results if not properly handled

    ((rLo, rHi), (posLo, posHi))
  }

  /** Over-approximating implementation of the trigonometric sine function
   */
  def sine: Interval = {
    def mod(a: BigInt, b: BigInt): BigInt = {(a % b + b) % b}

    val ((yloBounded, yhiBounded), (posLo, posHi)) = this.reduceRange()
    if (posHi - posLo >= 4) {
      // the interval is larger than 2 pi -> no information
      Interval(-1, 1)
    } else {
      // In which quarters of the sine wave are we?
      //
      // |  _|_  |   |   |
      // | / | \ |   |   |
      // |/  |  \|   |   |
      // |   |   |   |   |
      // |   |   |\  |  /|
      // |   |   | \_|_/ |
      // |   |   |   |   |
      //   0   1   2   3
      //
      (mod(posLo, 4).toInt, mod(posHi, 4).toInt) match {
        case (0, 3) | (2, 1) => Interval(-1, 1) // both -1 and 1 contained
        case (0, 1) | (0, 2) | (3, 1) | (3, 2) => { // 1, but not -1 contained
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          Interval(min(min(yll, ylh), min(yhl, yhh)), 1)
        }
        case (1, 0) | (1, 3) | (2, 0) | (2, 3) => { // -1, but not 1 contained
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          Interval(-1, max(max(yll, ylh), max(yhl, yhh)))
        }
        case _ => { // monotone part
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          Interval(min(min(yll, ylh), min(yhl, yhh)), max(max(yll, ylh), max(yhl, yhh)))
        }
      }
    }
  }

  /** Over-approximating implementation of the trigonometric cosine function
   */
  def cosine: Interval = {
    val conv = (pi / Interval(2, 2)) - this
    conv.sine
  }

  def tangent: Interval = {
    this.sine / this.cosine
  }

  def arcsine: Interval = {
    if (xlo < -one || xhi > one) {
      throw new ArcOutOfBoundsException("Trying to compute arcsine of: " + this)
    }
    Interval(arcsineDown(xlo), arcsineUp(xhi))
  }

  def arccosine: Interval = {
    if (xlo < -one || xhi > one) {
      throw new ArcOutOfBoundsException("Trying to compute arccosine of: " + this)
    }
    Interval(arccosineDown(xhi), arccosineUp(xlo))   // negative derivative
  }

  def arctangent: Interval = {
    Interval(arctanDown(xlo), arctanUp(xhi))
  }

  def exp: Interval = {
    Interval(expDown(xlo), expUp(xhi))
  }

  def log: Interval = {
    if (xlo <= zero) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }
    Interval(logDown(xlo), logUp(xhi))
  }

  @inline
  def includes(that: Interval): Boolean = this.xlo <= that.xlo && that.xhi <= this.xhi

  @inline
  def includes(r: Rational): Boolean = xlo <= r && r <= xhi

  def isNonNegative: Boolean = this.xlo >= Rational.zero

  def isPowerOf2: Boolean = this.xlo.equals(this.xhi) && this.xhi.isPowerOf2

}
