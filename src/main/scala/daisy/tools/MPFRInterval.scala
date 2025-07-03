// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.collection.immutable.Seq

import MPFRFloat.{zero => fzero, _}

object MPFRInterval {

  def maxAbs(i: MPFRInterval): MPFRFloat = max(abs(i.xlo), abs(i.xhi))

  def minAbs(i: MPFRInterval): MPFRFloat = min(abs(i.xlo), abs(i.xhi))

  def apply(r: Rational): MPFRInterval = r.toMPFRInterval

  def apply(r: MPFRFloat): MPFRInterval = MPFRInterval(r, r)

  def apply(i: MPFRInterval): MPFRInterval = i

  def apply(i: Interval): MPFRInterval = {
    val lo = i.xlo.toMPFRInterval.xlo
    val hi = i.xhi.toMPFRInterval.xhi
    MPFRInterval(lo, hi)
  }

  def +/-(r: MPFRFloat) = MPFRInterval(-r,r)

  def +/-(r: Rational) = MPFRInterval(Interval(-r,r))

  val zero: MPFRInterval = MPFRInterval(fzero)

  val pi = MPFRInterval(MPFRFloat.fromString("3.141592653589793238"),
                    MPFRFloat.fromString("3.141592653589793239"))

  def union(ints: Set[MPFRInterval]): MPFRInterval = MPFRInterval(ints.minBy(_.xlo).xlo, ints.maxBy(_.xhi).xhi) //.tail.foldLeft(ints.head)({case (acc, x) => acc.union(x)})

  /**
   * Returns a set of integers that lay inside a rational interval. By default rounds outwards
   * @param x interval
   * @return set of integers
   */
  def integersIn(x: MPFRInterval, outwards:Boolean = true): Set[Int] = {
    // if the bounds are integers, take their exact values, otherwise round outwards
    if (outwards) {
      val from = x.xlo.intValue()
      val tto = if (x.xhi.isWhole()) x.xhi.intValue() else x.xhi.intValue() + 1
      Set.from(from to tto)
    } else {
      // round inwards // todo ever needed?
      val from = if (x.xlo.isWhole()) x.xlo.intValue() else x.xlo.intValue() + 1
      val tto = if (x.xhi.isWhole()) x.xhi.intValue() else x.xhi.intValue() // todo ?? - 1
      Set.from(from to tto)
    }
  }

  def intersect(es: Seq[MPFRInterval]): Option[MPFRInterval] = {
    val head: Option[MPFRInterval] = Some(es.head)
    es.tail.foldLeft(head)({
      case (None, _) => None
      case (Some(acc), i) => acc.intersect(i)
    })
  }
}

case class PartialMPFRInterval(xlo: Option[MPFRFloat], xhi: Option[MPFRFloat])


case class MPFRInterval(xlo: MPFRFloat, xhi: MPFRFloat) extends RangeArithmetic[MPFRInterval] {
  assert(xlo <= xhi, f"interval lower bound cannot be bigger than upper bound ${xlo.toLongString} > ${xhi.toLongString}")

  import MPFRInterval.pi

  // no specific rounding should be fine here, is used for subdivision
  val mid: MPFRFloat = xlo/two + xhi/two

  val width: MPFRFloat = xhi up_- xlo
  // val radius: MPFRFloat = width up_/ two

  def isPointRange: Boolean = xlo == xhi

  // for compatibility with RangeArithmetic
  def toInterval: Interval = Interval(Rational.fromString(xlo.toString),
      Rational.fromString(xhi.toString))

  /* Adds an error of magnitude r */
  def +/-(r: MPFRFloat): MPFRInterval = this + MPFRInterval(-r, r)

  def +/-(r: Rational): MPFRInterval = {
    assert(r >= Rational.zero)
    val mpfr = MPFRFloat.fromString(r.toStringUp)
    this + MPFRInterval(-mpfr, mpfr)
  }

  def addConstraint(e: Set[lang.Trees.Expr]): MPFRInterval = this

  def unary_- = MPFRInterval(-xhi, -xlo)

  def +(other: MPFRInterval): MPFRInterval = {
    MPFRInterval(xlo down_+ other.xlo, xhi up_+ other.xhi)
  }

  def -(other: MPFRInterval): MPFRInterval = {
    MPFRInterval(xlo down_- other.xhi, xhi up_- other.xlo)
  }

  def *(y: MPFRInterval): MPFRInterval = y match {
    case MPFRInterval(ylo, yhi) =>

      if (xlo == fzero && xhi == fzero) {
        MPFRInterval(fzero, fzero)
      } else if (xlo >= fzero) {
        if (ylo >= fzero) {
          MPFRInterval(xlo down_* ylo, xhi up_* yhi)
        } else if (yhi <= fzero) {
          MPFRInterval(xhi down_* ylo, xlo up_* yhi)
        } else {
          MPFRInterval(xhi down_* ylo, xhi up_* yhi)
        }
      }
      else if (xhi <= fzero) {
        if (ylo >= fzero) {
          MPFRInterval(xlo down_* yhi, xhi up_* ylo)
        } else if (yhi <= fzero) {
          MPFRInterval(xhi down_* yhi, xlo up_* ylo)
        } else {
          MPFRInterval(xlo down_* yhi, xlo up_* ylo)
        }
      }
      else {
        if (ylo >= fzero) {
          MPFRInterval(xlo down_* yhi, xhi up_* yhi)
        } else if (yhi <= fzero) {
          MPFRInterval(xhi down_* ylo, xlo up_* ylo)
        } else {
          val a = min(xlo down_* yhi, xhi down_* ylo)
          val b = max(xlo up_* ylo, xhi up_* yhi)
          MPFRInterval(a, b)
        }
      }
  }

  // the lazy version
  def *(r: MPFRFloat): MPFRInterval = this * MPFRInterval(r, r)

  def *(r: Rational): MPFRInterval = this * MPFRInterval(r)

  // 0/ 0 is undefined, and will also throw a DivisionByZeroException
  def /(y: MPFRInterval): MPFRInterval = y match {
    case MPFRInterval(ylo, yhi) =>

      // if(xlo == zero && ylo == zero) Interval(zero, zero)
      if (ylo >= fzero) {
        if (xlo >= fzero) {
          MPFRInterval(xlo down_/ yhi, xhi up_/ ylo)
        } else if (xhi <= fzero) {
          MPFRInterval(xlo down_/ ylo, xhi up_/ yhi)
        } else {
          MPFRInterval(xlo down_/ ylo, xhi up_/ ylo)
        }
      }
      else if (yhi <= fzero) {
        if (xlo >= fzero) {
          MPFRInterval(xhi down_/ yhi, xlo up_/ ylo)
        } else if (xhi <= fzero) {
          MPFRInterval(xhi down_/ ylo, xlo up_/ yhi)
        } else {
          MPFRInterval(xhi down_/ yhi, xlo up_/ yhi)
        }
      } else {
        throw DivisionByZeroException("trying to divide by interval containing 0")
      }
  }

  def ^(n: Int): MPFRInterval = {
    val p =  MPFRFloat.fromString(n.toString)
    if (n % 2 == 0 && includes(fzero)) {
      // even powers non-monotonic over - / +
      MPFRInterval(fzero, powUp(MPFRInterval.maxAbs(this), p))
    } else if (p % two == fzero && xhi < fzero) {
      // even powers monotonically falling over - / -
      MPFRInterval(powDown(xhi, p), powUp(xlo, p))
    } else {
      // odd powers, and even powers over + / + monotonically rising
      MPFRInterval(powDown(xlo, p), powUp(xhi, p))
    }
  }

  /**
   * Compares 2 intervals by width
   * @param i2 the second interval to compare with
   * @return
   */
  def >(i2: MPFRInterval): Boolean = {
    this.width > i2.width
  }

  /**
   * Checks whether 2 intervals are equal
   * @param y - interval to compare with
   * @return true/ false
   */
  def equals(y: MPFRInterval): Boolean = {
    this.xlo == y.xlo && this.xhi == y.xhi
  }

  def inverse: MPFRInterval = MPFRInterval(one) / this

  def squareRoot: MPFRInterval = {
    if (xlo < fzero) {
      throw NegativeSqrtException("Trying to take the sqrt of a negative number!")
    }
    MPFRInterval(sqrtDown(xlo), sqrtUp(xhi))
  }

  def union(y: MPFRInterval): MPFRInterval = {
    MPFRInterval(min(this.xlo, y.xlo), max(this.xhi, y.xhi))
  }

  def toBigString: String = "[%.18g, %.18g]".format(xlo.toLongString, xhi.toLongString)
  override def toString: String = "[%s, %s]".format(xlo.toString, xhi.toString)

  // divide the initial interval for two and return the tuple
  def getBinaryDivision: (MPFRInterval, MPFRInterval) =
    (MPFRInterval(this.xlo, this.mid), MPFRInterval(this.mid, this.xhi))

  /**
   * Divides the interval to predefined number of subintervals
   * @param limit - amount of subintervals to get
   * @return list of subintervals
   */
  def divide(limit: Integer): Seq[MPFRInterval] = {
    val step = this.width / (MPFRFloat.fromString(limit.toString))

    var subintervals: Seq[MPFRInterval] = Seq()
    var i = 0
    var lowerBound = xlo
    while (i < (limit - 1)) {
      val upperBound = lowerBound + step
      subintervals = subintervals :+ MPFRInterval(lowerBound, upperBound)
      lowerBound = upperBound
      i = i + 1
    }
    // so that the smallest and largest bound are consistent with the original
    subintervals = subintervals :+ MPFRInterval(lowerBound, xhi)
    assert(subintervals.length == limit)
    subintervals
  }

  /**
   * Extends interval by predefined parameter.
   * E.g. [0].extendBy(0.1)
   * will return interval [-0.1, 0.1]
   * @param parameter positive value for which the bounds of original interval will be changed
   * @return extended interval
   */
  def extendBy(parameter: MPFRFloat): MPFRInterval =
    MPFRInterval(this.xlo down_- parameter, this.xhi up_+ parameter)

  /** Reduces the Interval bounds to [-pi, pi]
   *
   * Note that the resulting values form not necessarily an interval.
   *
   * It also returns the indices of the quarters of the sine wave in which the
   * end points are.
   */
  def reduceRange() : ((MPFRFloat, MPFRFloat), (BigInt, BigInt)) = {
    // lower bound >= 0 -> subtract over-approximation of pi...
    // lower bound < 0 -> subtract under-approximation of pi...
    // ... and for the widened interval add the opposite one
    val piForLo = if (xlo >= fzero) (pi.xhi, pi.xlo) else (pi.xlo, pi.xhi)

    // upper bound >= 0 -> subtract under-approximation of pi...
    // upper bound < 0 -> subtract over-approximation of pi...
    // ... and for the widened interval add the opposite one
    val piForHi = if (xhi >= fzero) (pi.xlo, pi.xhi) else (pi.xhi, pi.xlo)
    // Returns the representative y of x in [-b, b] and the number of times 2*b
    // has been subtracted from x to obtain y.
    def reduceTo(x: MPFRFloat, b: MPFRFloat): (MPFRFloat, MPFRFloat) = {
      val k: Int = (x / b).intValue
      val sign = if (k >= 0) 1 else -1
      val l = (k + sign) / 2
      val res = x - MPFRFloat.fromString((l * 2).toString) * b
      (res, MPFRFloat.fromString(l.toString))
    }

    val (rLo, rLoFact) = reduceTo(xlo, piForLo._1)
    val (rHi, rHiFact) = reduceTo(xhi, piForHi._1)

    // find an over-approximation of any interval that could lead to the
    // resulting values with the given uncertainty of pi
    val rounded = MPFRInterval(rLo down_+ (rLoFact down_* two down_* piForLo._2),
      rHi up_+ (rHiFact up_* two up_* piForHi._2))

    val piLo = if (rounded.xlo >= fzero) pi.xhi else pi.xlo
    val piHi = if (rounded.xhi >= fzero) pi.xlo else pi.xhi

    // computes n such that n*(pi/2) <= rounded.x__ <= (n+1)*(pi/2)
    var posLo = BigInt(((rounded.xlo down_* two) down_/ piLo).intValue)
    if (rounded.xlo < fzero) posLo -= 1
    var posHi = BigInt(((rounded.xhi up_* two) up_/ piHi).intValue)
    if (rounded.xhi < fzero) posHi -= 1
    // has to be based on rounded (instead of this) as the imprecision of pi can
    // make the bounded values be in different quarters of the sine wave as they
    // should be, resulting in unsound results if not properly handled

    ((rLo, rHi), (posLo, posHi))
  }

  /** Over-approximating implementation of the trigonometric sine function
   */
  def sine: MPFRInterval = {
    def mod(a: BigInt, b: BigInt): BigInt = {(a % b + b) % b}

    val ((yloBounded, yhiBounded), (posLo, posHi)) = this.reduceRange()

    if (posHi - posLo >= 4) {
      // the interval is larger than 2 pi -> no information
      MPFRInterval(-one, one)
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
        case (0, 3) | (2, 1) => MPFRInterval(-one, one) // both -1 and 1 contained
        case (0, 1) | (0, 2) | (3, 1) | (3, 2) => { // 1, but not -1 contained
          val (yll, ylh) = (sinDown(yloBounded), sinUp(yloBounded))
          val (yhl, yhh) = (sinDown(yhiBounded), sinUp(yhiBounded))
          MPFRInterval(min(min(yll, ylh), min(yhl, yhh)), one)
        }
        case (1, 0) | (1, 3) | (2, 0) | (2, 3) => { // -1, but not 1 contained
          val (yll, ylh) = (sinDown(yloBounded), sinUp(yloBounded))
          val (yhl, yhh) = (sinDown(yhiBounded), sinUp(yhiBounded))
          MPFRInterval(-one, max(max(yll, ylh), max(yhl, yhh)))
        }
        case _ => { // monotone part
          val (yll, ylh) = (sinDown(yloBounded), sinUp(yloBounded))
          val (yhl, yhh) = (sinDown(yhiBounded), sinUp(yhiBounded))
          MPFRInterval(min(min(yll, ylh), min(yhl, yhh)),
                       max(max(yll, ylh), max(yhl, yhh)))
        }
      }
    }
  }

  /** Over-approximating implementation of the trigonometric cosine function
   */
  def cosine: MPFRInterval = {
    val conv = (pi / MPFRInterval(two, two)) - this
    conv.sine
  }

  def tangent: MPFRInterval = {
    this.sine / this.cosine
  }

  def arccosine: daisy.tools.MPFRInterval = ???
  def arcsine: daisy.tools.MPFRInterval = ???

  def arctangent: daisy.tools.MPFRInterval = {
    MPFRInterval(atanDown(xlo), atanUp(xhi))
  }

  def exp: MPFRInterval = {
    MPFRInterval(expDown(xlo), expUp(xhi))
  }

  def log: MPFRInterval = {
    if (xlo <= fzero) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }
    MPFRInterval(logDown(xlo), logUp(xhi))
  }

  @inline
  def includes(that: MPFRInterval): Boolean = this.xlo <= that.xlo && that.xhi <= this.xhi

  @inline
  def includes(r: MPFRFloat): Boolean = xlo <= r && r <= xhi

  def isNonNegative: Boolean = this.xlo >= fzero

  def intersect(that: MPFRInterval): Option[MPFRInterval] = {
    val newLo = max(that.xlo, this.xlo)
    val newHi = min(that.xhi, this.xhi)
    if (newLo > newHi)
      None
    else
      Some(MPFRInterval(newLo, newHi))
  }

  // def isPowerOf2: Boolean = this.xlo.equals(this.xhi) && this.xhi.isPowerOf2

}

// exp log sq
