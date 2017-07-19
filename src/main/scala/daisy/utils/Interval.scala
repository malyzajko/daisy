/* Copyright 2009-2015 EPFL, Lausanne */

package daisy
package utils

import Rational._

object Interval {

  def maxAbs(i: Interval): Rational = {
    max(abs(i.xlo), abs(i.xhi))
  }

  def apply(r: Rational): Interval = Interval(r, r)
}

case class PartialInterval(xlo: Option[Rational], xhi: Option[Rational])

case class Interval(xlo: Rational, xhi: Rational) extends RangeArithmetic[Interval] {
  assert(xlo <= xhi, "interval lower bound cannot be bigger than upper bound")
  //private val zero = Rational(0.0)

  //def this(aa: RationalForm) = this(aa.interval._1, aa.interval._2)
  //def this(aa: FixedForm) = this(aa.qInterval._1, aa.qInterval._2)
  //def this(i: Interval) = this(Rational(i.xlo), Rational(i.xhi))

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
      if(xlo == zero && xhi == zero) Interval(zero, zero)
      else if(xlo >= zero) {
        if(ylo >= zero) Interval(xlo * ylo, xhi * yhi)
        else if(yhi <= zero) Interval(xhi * ylo, xlo * yhi)
        else Interval(xhi * ylo, xhi * yhi)
      }
      else if(xhi <= zero) {
        if(ylo >= zero) Interval( xlo * yhi, xhi * ylo)
        else if(yhi <= zero) Interval(xhi * yhi, xlo * ylo)
        else Interval(xlo * yhi, xlo * ylo)
      }
      else {
        if(ylo >= zero) Interval(xlo * yhi, xhi * yhi)
        else if(yhi <= zero) Interval(xhi * ylo, xlo * ylo)
        else {
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

      //if(xlo == zero && ylo == zero) Interval(zero, zero)

      if(ylo >= zero) {
        if(xlo >= zero) Interval(xlo / yhi, xhi/ ylo)
        else if(xhi <= zero) Interval(xlo / ylo, xhi / yhi)
        else Interval( xlo / ylo, xhi / ylo)
      }
      else if(yhi <= zero) {
        if(xlo >= zero) Interval(xhi / yhi, xlo / ylo)
        else if(xhi <= zero) Interval(xhi / ylo, xlo / yhi)
        else Interval( xhi / yhi, xlo / yhi)
      } else {
        throw DivisionByZeroException("trying to divide by interval containing 0")
        null //Interval(zero, zero) //dummy
      }

  }

  def inverse: Interval = Interval(one, one) / this

  def squareRoot: Interval = {
    if (xlo < zero) {
      throw NegativeSqrtException("Trying to take the sqrt of a negative number!")
    }
    Interval(sqrtDown(xlo), sqrtUp(xhi))
  }

  def union(y: Interval) = {
    val temp = Interval(min(this.xlo, y.xlo), max(this.xhi, y.xhi))
    temp
  }

  def toBigString = "[%.18g, %.18g]".format(xlo.toDouble, xhi.toDouble)
  override def toString = "[%s, %s]".format(xlo.toDouble, xhi.toDouble)

}