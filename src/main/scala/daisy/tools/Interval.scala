// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import Rational._
import daisy.lang.Identifiers.Identifier

object Interval {

  def maxAbs(i: Interval): Rational = {
    max(abs(i.xlo), abs(i.xhi))
  }

  def apply(r: Rational): Interval = Interval(r, r)
}

case class PartialInterval(xlo: Option[Rational], xhi: Option[Rational])

case class Interval(xlo: Rational, xhi: Rational) extends RangeArithmetic[Interval] {
  assert(xlo <= xhi, "interval lower bound cannot be bigger than upper bound")
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

  def unary_-(): Interval = Interval(-xhi, -xlo)

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
        null // Interval(zero, zero) //dummy
      }

  }

  def ^(n: Interval): Interval = {
    assert(n.xlo > Rational.one)
    //    System.out.println(s"to the power of $n int? " + n.xlo.isValidInt)
    //    assert(n.xlo.isValidInt && n.xhi.isValidInt)
    // fixme remove this assertion when we will allow expressions in power
    assert(n.xlo == n.xhi)
    var x = this
    for (i <- 1 until n.xlo.intValue()){
      x = x * this
    }
    x
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

}