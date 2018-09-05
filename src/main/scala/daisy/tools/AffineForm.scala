// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.collection.immutable.Seq
import utils.UniqueCounter

import Rational.{zero => rzero, _}

private[tools] case class Deviation(mgnt: Rational, index: Int) {
  def unary_-(): Deviation = Deviation(-mgnt, index)
  def +(y: Deviation): Deviation = {
    assert(this.index == y.index)
    Deviation(this.mgnt + y.mgnt, index)
  }
  def -(y: Deviation): Deviation = {
    assert(this.index == y.index)
    Deviation(this.mgnt - y.mgnt, index)
  }
  def *(factor: Rational): Deviation = {
    Deviation(this.mgnt * factor, index)
  }
  def isZero: Boolean = (mgnt == Rational.zero)

  override def toString: String = "%s(%d)".format(mgnt.toString, index)
}

// to be called with nextGlobal
private[tools] object AffineIndex extends UniqueCounter[Int]

object AffineForm {

  def apply(i: Interval): AffineForm = {
    val a = i.xlo
    val b = i.xhi
    if (a == b) {
      AffineForm(a, Seq[Deviation]())
    } else {
      val un = (b - a)/ Rational(2)
      AffineForm(a + un, Seq(Deviation(un, AffineIndex.nextGlobal)))
    }
  }


  def apply(r: Rational): AffineForm = AffineForm(r, Seq[Deviation]())

  def +/-(x: Rational): AffineForm = {
    AffineForm(Rational.zero, Seq(Deviation(x, AffineIndex.nextGlobal)))
  }

  val zero: AffineForm = AffineForm(Rational.zero, Seq())
}


case class AffineForm(x0: Rational, noise: Seq[Deviation]) extends RangeArithmetic[AffineForm] {

  if (noise.size > 200) {
    System.err.println("200 noise terms")
  }

  lazy val radius: Rational = sumAbsQueue(noise)

  lazy val toInterval: Interval = {
    val rad = radius
    Interval(x0 - rad, x0 + rad)
  }

  /**
    Adds a new error/noise term with magniture r,
    and leaves all else as before.
   */
  def :+(r: Rational): AffineForm = {
    AffineForm(x0: Rational, noise :+ Deviation(r, AffineIndex.nextGlobal))
  }
  // for compatibility with RangeArithmetic
  def +/-(r: Rational): AffineForm = this :+ r

  def addConstraint(e: Set[lang.Trees.Expr]): AffineForm = this

  def unary_-(): AffineForm = AffineForm(-x0, noise.map(-_))

  def +(y: AffineForm): AffineForm =
    AffineForm(this.x0 + y.x0, addQueues(this.noise, y.noise))

  def -(y: AffineForm): AffineForm =
    AffineForm(this.x0 - y.x0, subtractQueues(this.noise, y.noise))

  def *(y: AffineForm): AffineForm = {
    var z0 = this.x0 * y.x0

    // z0Addition is not necessarily used, depending on which fnc you use
    val (z0Addition, delta) = multiplyQueuesOptimized(this.noise, y.noise)
    z0 += z0Addition
    var newTerms: Seq[Deviation] = multiplyQueuesAndMerge(this.x0, this.noise, y.x0, y.noise)
    if(delta != 0) {
      newTerms :+= Deviation(delta, AffineIndex.nextGlobal)
    }
    AffineForm(z0, newTerms)
  }

  def *(r: Rational): AffineForm = {
    AffineForm(r * x0, multiplyQueue(noise, r))
  }

  /**
    Computes the inverse of this AffineForm as a linear approximation.
   */
  def inverse: AffineForm = {
    val (xlo, xhi) = (toInterval.xlo, toInterval.xhi)

    // if (xlo <= Rational(0.0) && xhi >= Rational(0.0))
    //  throw OutOfDomainException("Possible division by zero: " + toString)

    if(noise.size == 0) { // exact
      val inv = one/x0
      AffineForm(inv, Seq[Deviation]())
    } else {

      /* Calculate the inverse */
      val a = min(abs(xlo), abs(xhi))
      val b = max(abs(xlo), abs(xhi))

      val alpha = Rational(-1) / (b * b)

      val dmax = (one / a) - (alpha * a)
      val dmin = (one / b) - (alpha * b)

      var zeta = (dmin / two) + (dmax / two)
      if (xlo < rzero) zeta = -zeta
      val delta = max(zeta - dmin, dmax - zeta)

      val z0 = alpha * this.x0 + zeta

      var newTerms = multiplyQueue(noise, alpha)
      if(delta != rzero) newTerms :+= Deviation(delta, AffineIndex.nextGlobal)
      AffineForm(z0, newTerms)
    }
  }

  /**
    Computes x/y as x * (1/y).
   */
  def /(y: AffineForm): AffineForm = {
    this * y.inverse
  }

  def ^(_n: Int): AffineForm = {
    var x = this
    var n = _n - 1
    while (n > 0){
      if (n % 2 == 1) {
        n -= 1
        x *= this
      } else {
        n /= 2
        x = x * x
      }
    }
    x
  }

  def squareRoot: AffineForm = {
    val (a, b) = (toInterval.xlo, toInterval.xhi)

    if (b < rzero) {
     throw NegativeSqrtException("Sqrt of negative number: " + toString)
    }
    // if (a < rzero) a = rzero  //soft policy

    /* if(noise.size == 0) { //exact
      val sqrt = x0.sqrt
      //val maxError = ...  can we get the error on this?
      val maxError = zero
      return new XRationalForm(sqrt, new Queue[Deviation](Deviation(newIndex, maxError)))
    } */

    val alpha = Rational(1L, 2L) / sqrtUp(b)
    val dmin = sqrtDown(a) - (alpha * a)
    val dmax = sqrtUp(b) - (alpha * b)

    val zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    unaryOp(x0, noise, alpha, zeta, delta)
  }

  /** Min-range based approximating implementation of sine
   */
  def sine: AffineForm = {
    val (l, u) = (toInterval.xlo, toInterval.xhi)

    val intsol = toInterval.sine
    if (intsol.xlo == -1 || intsol.xhi == 1) {
      // we are not in a monotone part of sine, so we use interval results
      AffineForm(intsol)
    } else {
      // compute intervals enclosing the sine value at the ending points
      val aInt = Interval(l, l).sine
      val bInt = Interval(u, u).sine

      // choose the end points a, b of the intervals that maximize abs(a-b)
      val mp = MonotonicityPhase.getMonotonicityPhaseSine(Interval(l, u))
      assert(!(mp.isInstanceOf[Mixed]))

      val a = (mp: @unchecked) match {
        case Rising() => aInt.xlo
        case Falling() => aInt.xhi
      }
      val b = (mp: @unchecked) match {
        case Rising() => bInt.xhi
        case Falling() => bInt.xlo
      }

      // Compute slope of approximation. If an inflection point of sine is
      // included in the interval, use -1 or 1 to over-approximate.
      val alpha =
        if (a < 0 && b > 0) { // rising inflection point included
          one
        } else if (a > 0 && b < 0) { // falling inflection point included
          - one
        } else {
          // heuristic: Choose the ending point c that is farther from the
          // x-axis for computing the slope alpha. This might yield better
          // accuracy.
          val chooseA = abs(a) >= abs(b)
          val c = if (chooseA) l else u

          // cosine is the derivative of sine
          val slopeInt = Interval(c, c).cosine

          // Decide whether to round the slope up or down for soundness. It
          // should be rounded such that there is a larger distance between the
          // resulting line and the sine curve at the other interval end point.
          // The first component is a check for concavity as the second
          // derivative of sin(x) is -sin(x)
          // Note: this is a more verbose formulation of ((a > 0) == chooseA)
          (a > 0, chooseA) match {
            case (true, true) | (false, false) => slopeInt.xhi
            case (true, false) | (false, true) => slopeInt.xlo
          }
        }

      // compute the y-intercept
      val z1 = a - alpha * l
      val z2 = b - alpha * u
      val zeta = computeZeta(z1, z2)

      // compute the maximal deviation
      val delta = max(abs(z1 - zeta), abs(z2 - zeta))

      // apply the linear approximation to the input
      unaryOp(x0, noise, alpha, zeta, delta)
    }
  }

  def cosine: AffineForm = {
    val conv = (AffineForm(Interval.pi) / AffineForm(Interval(2, 2))) - this
    conv.sine
  }

  def tangent: AffineForm = {
    this.sine / this.cosine
  }

  /** Min-range based linear approximation of the exp() function
   */
  def exp: AffineForm = {
    val (a, b) = (toInterval.xlo, toInterval.xhi)

    // Take slope of the left ending point of the interval (which is smaller),
    // probably results in better ranges.
    // Round it down to be sound for convex functions such as exp.
    val alpha = expDown(a)
    val dmin = expDown(a) - (alpha * a)
    val dmax = expUp(b) - (alpha * b)

    val zeta = computeZeta(dmin, dmax)
    val delta = max(abs(dmin - zeta), abs(dmax - zeta))
    unaryOp(x0, noise, alpha, zeta, delta)
  }

  def log: AffineForm = {
    val (a, b) = (toInterval.xlo, toInterval.xhi)

    if (a <= rzero) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }

    // Take slope of the right ending point of the interval (which is smaller),
    // probably results in better ranges.
    // Round it down to be sound for concave functions such as log.
    val alpha = one / b
    val dmin = logDown(a) - (alpha * a)
    val dmax = logUp(b) - (alpha * b)

    val zeta = computeZeta(dmin, dmax)
    val delta = max(abs(dmin - zeta), abs(dmax - zeta))
    unaryOp(x0, noise, alpha, zeta, delta)
  }

  override def toString: String = "[%s,%s]".format(toInterval.xlo.toDouble, toInterval.xhi.toDouble)
  def toSmallString: String = "[%.3f,%.3f]".format(toInterval.xlo.toDouble, toInterval.xhi.toDouble)
  def toBigString: String = "[%.18f,%.18f]".format(toInterval.xlo.toDouble, toInterval.xhi.toDouble)

  def longString: String =
    "%s +/- %s".format(x0.toString, noise.mkString(", "))

  def detailString: String = x0.toDouble + " +/- " + radius.toDouble

  private def computeZeta(dmin: Rational, dmax: Rational): Rational = {
    dmin / two +  dmax / two
  }

  private def computeDelta(zeta: Rational, dmin: Rational, dmax: Rational): Rational = {
    max(zeta - dmin,  dmax - zeta)
  }

  // Int.MaxValue is necessary for correctness, as we compare indices
  private val dummyDev = Deviation(rzero, Int.MaxValue)

  private def sumAbsQueue(queue: Seq[Deviation]): Rational = {
    var sum = rzero
    val iter = queue.iterator
    while(iter.hasNext) {
      sum += Rational.abs(iter.next.mgnt)
    }
    sum
  }

  private def addQueues(xn: Seq[Deviation], yn: Seq[Deviation]): Seq[Deviation] = {
    var deviation: Seq[Deviation] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: Deviation) => deviation :+= xi
    val fy = (yi: Deviation) => deviation :+= yi

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  xi + yi
      if (!zi.isZero) deviation :+= zi
    }
    DoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  private def subtractQueues(xn: Seq[Deviation], yn: Seq[Deviation]): Seq[Deviation] = {
    var deviation: Seq[Deviation] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: Deviation) => deviation :+= xi
    val fy = (yi: Deviation) => deviation :+= -yi

    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi =  xi - yi
      if (!zi.isZero) deviation :+= zi
    }
    DoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  private def multiplyQueuesAndMerge(a: Rational, xqueue: Seq[Deviation], b: Rational,
    yqueue: Seq[Deviation]): Seq[Deviation] = {
    var deviation = Seq[Deviation]()
    val iterX = xqueue.iterator
    val iterY = yqueue.iterator

    val fx = (dev: Deviation) => {
      val zi = dev * b
      if (!zi.isZero) deviation :+= zi
    }
    val fy = (dev: Deviation) => {
      val zi = dev * a
      if (!zi.isZero) deviation :+= zi
    }
    val fCouple = (xi: Deviation, yi: Deviation) => {
      val zi = xi * b + yi * a
      if (!zi.isZero) deviation :+= zi
    }
    DoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  // private def multiplyQueuesSimple(xqueue: Seq[Deviation], yqueue: Seq[Deviation]): (Rational, Rational) = {
  //   val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))
  //   var zqueue = rzero
  //   var z0Addition = rzero

  //   var i = 0
  //   while (i < indices.length) {
  //     val iInd = indices(i)
  //     // quadratic
  //     val xi = xqueue.find((d: Deviation) => d.index == iInd) match {
  //       case Some(d) => d.mgnt; case None => Rational(0) }
  //     val yi = yqueue.find((d: Deviation) => d.index == iInd) match {
  //       case Some(d) => d.mgnt; case None => Rational(0) }
  //     val zii = xi * yi
  //     // z0Addition += zii / Rational(2.0)
  //     if (zii != 0) zqueue += abs(zii)

  //     var j = i + 1
  //     while (j < indices.length) {
  //       val jInd = indices(j)
  //       val xj = xqueue.find((d: Deviation) => d.index == jInd) match {
  //         case Some(d) => d.mgnt; case None => Rational(0) }
  //       val yj = yqueue.find((d: Deviation) => d.index == jInd) match {
  //       case Some(d) => d.mgnt; case None => Rational(0) }
  //       val zij = xi * yj + xj * yi
  //       if (zij != 0) zqueue += abs(zij)
  //       j += 1
  //     }
  //     i += 1
  //   }
  //   (z0Addition, zqueue)
  // }


  // Does a smarter computation of the quadratic terms
  private def multiplyQueuesOptimized(xqueue: Seq[Deviation], yqueue: Seq[Deviation]): (Rational, Rational) = {
    val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))
    var zqueue = rzero
    var z0Addition = rzero

    var i = 0
    while (i < indices.length) {
      val iInd = indices(i)
      // quadratic
      val xi = xqueue.find((d: Deviation) => d.index == iInd) match {
        case Some(d) => d.mgnt; case None => rzero }
      val yi = yqueue.find((d: Deviation) => d.index == iInd) match {
        case Some(d) => d.mgnt; case None => rzero }
      val zii = xi * yi
      z0Addition += zii / two
      if (zii != 0) zqueue += abs(zii / two)

      var j = i + 1
      while (j < indices.length) {
        val jInd = indices(j)
        val xj = xqueue.find((d: Deviation) => d.index == jInd) match {
          case Some(d) => d.mgnt; case None => rzero }
        val yj = yqueue.find((d: Deviation) => d.index == jInd) match {
        case Some(d) => d.mgnt; case None => rzero }
        val zij = xi * yj + xj * yi
        if (zij != 0) zqueue += abs(zij)
        j += 1
      }
      i += 1
    }
    (z0Addition, zqueue)
  }

  private def mergeIndices(x: Set[Int], y: Set[Int]): Array[Int] = {
    val set = x ++ y
    val list = set.toList.sorted
    list.toArray
  }

  // Do this with some functional thing?
  private def getIndices(q: Seq[Deviation]): collection.immutable.Set[Int] = {
    var i = 0
    var set = new collection.immutable.HashSet[Int]()
    while (i < q.size) {
      set += q(i).index
      i += 1
    }
    set
  }

  private def multiplyQueue(queue: Seq[Deviation], factor: Rational): Seq[Deviation] = {
    var deviation = Seq[Deviation]()
    val iter = queue.iterator
    while(iter.hasNext) {
      val xi = iter.next
      val zi = xi * factor
      if (!zi.isZero) deviation :+= zi
    }
    deviation
  }

  private def unaryOp(x0: Rational, noise: Seq[Deviation], alpha: Rational, zeta: Rational,
    delta: Rational): AffineForm = {

    val z0 = alpha * x0 + zeta
    var deviation = multiplyQueue(noise, alpha)

    if (delta != rzero) deviation :+= Deviation(delta, AffineIndex.nextGlobal)
    AffineForm(z0, deviation)
  }
}


// This is probably not the most efficient way, but it's tried and tested.
object DoubleQueueIterator {

  def iterate(iterX: Iterator[Deviation], iterY: Iterator[Deviation],
    dummy: Deviation, fx: (Deviation) => Unit, fy: (Deviation) => Unit,
    fCouple: (Deviation, Deviation) => Unit): Unit = {
    var xi: Deviation = if (iterX.hasNext) iterX.next else dummy
    var yi: Deviation = if (iterY.hasNext) iterY.next else dummy

    var i = 0
    while ((iterX.hasNext || iterY.hasNext)) {
      i = i + 1
      if(xi.index < yi.index) {
        fx(xi)
        xi = if (iterX.hasNext) iterX.next else dummy
      }
      else if (yi.index < xi.index) {
        fy(yi)
        yi = if (iterY.hasNext) iterY.next else dummy
      }
      else {
        fCouple(xi, yi)
        xi = if (iterX.hasNext) iterX.next else dummy
        yi = if (iterY.hasNext) iterY.next else dummy
      }
    }
    if (xi.index == yi.index) {
      if (xi != dummy) {
        fCouple(xi, yi)
        xi = dummy
        yi = dummy
      }
    }
    else if (xi.index < yi.index) {
      if (xi != dummy) {fx(xi); xi = dummy}
      if (yi != dummy) {fy(yi); yi = dummy}
    }
    else if (yi.index < xi.index) {
      if (yi != dummy) {fy(yi); yi = dummy}
      if (xi != dummy) {fx(xi); xi = dummy}
    }
  }
}
