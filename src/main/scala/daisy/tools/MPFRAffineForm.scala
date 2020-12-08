// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.collection.immutable.Seq
import utils.UniqueCounter
import MPFRFloat.{zero => fzero,_}
import MPFRInterval.{zero => izero, _}

private[tools] case class MPFRDeviation(mgnt: MPFRInterval, index: Int) {
  def unary_- = MPFRDeviation(-mgnt, index)
  def +(y: MPFRDeviation): MPFRDeviation = {
    assert(this.index == y.index)
    MPFRDeviation(this.mgnt + y.mgnt, index)
  }
  def -(y: MPFRDeviation): MPFRDeviation = {
    assert(this.index == y.index)
    MPFRDeviation(this.mgnt - y.mgnt, index)
  }
  // def *(factor: MPFRFloat): MPFRDeviation = {
  //   MPFRDeviation(this.mgnt * factor, index)
  // }
  def *(factor: MPFRInterval): MPFRDeviation = {
    MPFRDeviation(this.mgnt * factor, index)
  }
  def isZero: Boolean = (mgnt == izero)

  override def toString: String = "%s(%d)".format(mgnt.toString, index)
}

// to be called with nextGlobal
private[tools] object MPFRAffineIndex extends UniqueCounter[Int]

object MPFRAffineForm {

  def apply(i: MPFRInterval): MPFRAffineForm = {
    val a = i.xlo
    val b = i.xhi
    if (a == b) {
      MPFRAffineForm(MPFRInterval(a), Seq[MPFRDeviation]())
    } else {
      val un = (b - a)/ two
      MPFRAffineForm(MPFRInterval(a + un),
        Seq(MPFRDeviation(MPFRInterval(un), MPFRAffineIndex.nextGlobal)))
    }
  }

  def apply(r: MPFRFloat): MPFRAffineForm =
    MPFRAffineForm(MPFRInterval(r), Seq[MPFRDeviation]())

  def apply(r: Rational): MPFRAffineForm =
    MPFRAffineForm(MPFRInterval(r), Seq[MPFRDeviation]())

  // def +/-(x: MPFRInterval): MPFRAffineForm = {
  //   MPFRAffineForm(MPFRInterval(MPFRFloat.zero), Seq(MPFRDeviation(x, MPFRAffineIndex.nextGlobal)))
  // }

  def apply(i: Interval): MPFRAffineForm = MPFRAffineForm(MPFRInterval(i))

  def +/-(x: MPFRFloat): MPFRAffineForm = {
    MPFRAffineForm(MPFRInterval(MPFRFloat.zero),
      Seq(MPFRDeviation(MPFRInterval(x), MPFRAffineIndex.nextGlobal)))
  }

  def +/-(x: Rational): MPFRAffineForm = {
    MPFRAffineForm(MPFRInterval(MPFRFloat.zero),
      Seq(MPFRDeviation(MPFRInterval(x), MPFRAffineIndex.nextGlobal)))
  }

  val zero: MPFRAffineForm = MPFRAffineForm(MPFRInterval.zero, Seq())

  def newFormWithPacking(x0: MPFRInterval, noise: Seq[MPFRDeviation]): MPFRAffineForm = {
    if ((noise.size) > 200) {
      //println("packing!")
      MPFRAffineForm(x0, packNoiseTerms(noise))
    } else {
      MPFRAffineForm(x0, noise)
    }
  }

  private def packNoiseTerms(queue: Seq[MPFRDeviation]): Seq[MPFRDeviation] = {

    // only need doubles here:
    val mgnts: Seq[Double] = queue.map(x => maxAbs(x.mgnt).doubleValue)

    val sum = mgnts.sum
    val avrg = sum / mgnts.size

    // compute std
    val squaredDiff = mgnts.map(x => (x - avrg) * (x - avrg))
    val stdDev = math.sqrt(squaredDiff.sum / mgnts.size)

    // compact all deviations that are smaller than threshold
    val threshold: MPFRFloat = MPFRFloat.fromString((avrg + stdDev).toString)
    val (newQueue, tooSmallQueue) = queue.partition(x => maxAbs(x.mgnt) > threshold)
    // need to sum up the *absolute* values for soundness
    val newNoiseMgnt = tooSmallQueue.map(_.mgnt).foldLeft(MPFRInterval.zero)((acc, x) => acc + MPFRInterval(maxAbs(x)))

    newQueue :+ MPFRDeviation(newNoiseMgnt, MPFRAffineIndex.nextGlobal)
  }
}


case class MPFRAffineForm(x0: MPFRInterval, noise: Seq[MPFRDeviation]) extends RangeArithmetic[MPFRAffineForm]{
  import MPFRAffineForm.newFormWithPacking

  lazy val radius: MPFRFloat = sumAbsQueue(noise)
  lazy val toMPFRInterval: MPFRInterval = {
    MPFRInterval(x0.xlo down_- radius, x0.xhi up_+ radius)
  }

  lazy val toInterval: Interval = {
    Interval(Rational.fromString(toMPFRInterval.xlo.toString),
      Rational.fromString(toMPFRInterval.xhi.toString))
  }

  /**
    Adds a new error/noise term with magniture r,
    and leaves all else as before.
   */
  def :+(r: MPFRInterval): MPFRAffineForm = {
    MPFRAffineForm(x0, noise :+ MPFRDeviation(r, MPFRAffineIndex.nextGlobal))
  }

  def +/-(r: MPFRInterval): MPFRAffineForm = this :+ r
  // for compatibility with RangeArithmetic
  def +/-(r: Rational): MPFRAffineForm =  this :+ MPFRInterval(r)

  def addConstraint(e: Set[lang.Trees.Expr]): MPFRAffineForm = this

  def unary_- = MPFRAffineForm(-x0, noise.map(-_))

  def +(y: MPFRAffineForm): MPFRAffineForm =
    newFormWithPacking(this.x0 + y.x0, addQueues(this.noise, y.noise))

  def -(y: MPFRAffineForm): MPFRAffineForm =
    newFormWithPacking(this.x0 - y.x0, subtractQueues(this.noise, y.noise))

  def *(y: MPFRAffineForm): MPFRAffineForm = {
    var z0 = this.x0 * y.x0
    //println("M-z0: " + z0)

    // z0Addition is not necessarily used, depending on which fnc you use
    val (z0Addition, delta) = multiplyQueuesOptimized(this.noise, y.noise)
    //println("M-z0Addition: " + z0Addition + ", delta: " + delta)
    z0 = z0 + z0Addition
    var newTerms: Seq[MPFRDeviation] = multiplyQueuesAndMerge(this.x0, this.noise, y.x0, y.noise)
    if(delta != 0) {
      newTerms :+= MPFRDeviation(delta, MPFRAffineIndex.nextGlobal)
    }
    newFormWithPacking(z0, newTerms)
  }

  def *(r: MPFRFloat): MPFRAffineForm = {
    val factor = MPFRInterval(r)
    MPFRAffineForm(x0 * factor, multiplyQueue(noise, factor))
  }

  // for compatibility with RangeArithmetic
  def *(r: Rational): MPFRAffineForm = {
    val factor = MPFRInterval(r)
    MPFRAffineForm(x0 * factor, multiplyQueue(noise, factor))
  }

  /**
    Computes the inverse of this MPFRAffineForm as a linear approximation.
   */
  def inverse: MPFRAffineForm = {
    val ione = MPFRInterval(one)

    if(noise.size == 0) { // exact
      val inv = ione / x0
      MPFRAffineForm(inv, Seq[MPFRDeviation]())
    } else {

      val (xlo, xhi) = (toMPFRInterval.xlo, toMPFRInterval.xhi)
      /* Calculate the inverse */
      val a = min(abs(xlo), abs(xhi))
      val b = max(abs(xlo), abs(xhi))
      val alpha = MPFRInterval(-one / (b * b))

      val dmax = (ione / MPFRInterval(a)) - (MPFRInterval(a) * alpha)
      val dmin = (ione / MPFRInterval(b)) - (MPFRInterval(b) * alpha)

      var zeta = computeZeta(dmin, dmax)
      if (xlo < fzero) zeta = -zeta

      val delta = computeDelta(zeta, dmin, dmax)
      unaryOp(this.x0, this.noise, alpha, zeta, delta)
    }
  }

  /**
    Computes x/y as x * (1/y).
   */
  def /(y: MPFRAffineForm): MPFRAffineForm = {
    this * y.inverse
  }

  def ^(_n: Int): MPFRAffineForm = {
    var x = this
    var n = _n - 1
    while (n > 0){
      if (n % 2 == 1) {
        n -= 1
        x = x * this
      } else {
        n /= 2
        x = x * x
      }
    }
    x
  }

  def squareRoot: MPFRAffineForm = {
    val (a, b) = (toMPFRInterval.xlo, toMPFRInterval.xhi)

    if (b < fzero) {
     throw NegativeSqrtException("Sqrt of negative number: " + toString)
    }
    // if (a < fzero) a = fzero  //soft policy

    /* if(noise.size == 0) { //exact
      val sqrt = x0.sqrt
      //val maxError = ...  can we get the error on this?
      val maxError = zero
      return new XRationalForm(sqrt, new Queue[MPFRDeviation](MPFRDeviation(newIndex, maxError)))
    } */

    val alpha = MPFRInterval(MPFRFloat.fromString("0.5") / sqrtUp(b))
    val dmin = MPFRInterval(a).squareRoot - (MPFRInterval(a) * alpha)
    val dmax = MPFRInterval(b).squareRoot - (MPFRInterval(b) * alpha)

    val zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    unaryOp(this.x0, this.noise, alpha, zeta, delta)
  }

  /** Min-range based approximating implementation of sine
   */
  def sine: MPFRAffineForm = {
    val (l, u) = (toMPFRInterval.xlo, toMPFRInterval.xhi)

    val intsol = toMPFRInterval.sine // calling sine function of MPFRInterval

    if (intsol.xlo == -one || intsol.xhi == one) {
      // we are not in a monotone part of sine, so we use interval results
      MPFRAffineForm(intsol)
    } else {
      // compute intervals enclosing the sine value at the ending points
      val aInt = MPFRInterval(l, l).sine
      val bInt = MPFRInterval(u, u).sine

      // choose the end points a, b of the intervals that maximize abs(a-b)
      val mp = MonotonicityPhase.getMonotonicityPhaseSine(MPFRInterval(l, u))
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
        if (a < fzero && b > fzero) { // rising inflection point included
          MPFRInterval(one)
        } else if (a > fzero && b < fzero) { // falling inflection point included
          MPFRInterval(-one)
        } else {
          // heuristic: Choose the ending point c that is farther from the
          // x-axis for computing the slope alpha. This might yield better
          // accuracy.
          val chooseA = abs(a) >= abs(b)
          val c = if (chooseA) l else u

          // cosine is the derivative of sine
          val slopeInt = MPFRInterval(c, c).cosine

          // Decide whether to round the slope up or down for soundness. It
          // should be rounded such that there is a larger distance between the
          // resulting line and the sine curve at the other interval end point.
          // The first component is a check for concavity as the second
          // derivative of sin(x) is -sin(x)
          // Note: this is a more verbose formulation of ((a > 0) == chooseA)
          (a > MPFRFloat.zero, chooseA) match {
            case (true, true) | (false, false) => MPFRInterval(slopeInt.xhi)
            case (true, false) | (false, true) => MPFRInterval(slopeInt.xlo)
          }
        }

      // compute the y-intercept
      val z1 = MPFRInterval(a) - MPFRInterval(l) * alpha
      val z2 = MPFRInterval(b) - MPFRInterval(u) * alpha
      val zeta = computeZeta(z1, z2)

      // compute the maximal MPFRDeviation
      val delta = max(maxAbs(z1 - zeta), maxAbs(z2 - zeta))

      // apply the linear approximation to the input
      unaryOp(this.x0, this.noise, alpha, zeta, MPFRInterval(delta))
    }
  }

  def cosine: MPFRAffineForm = {
    val conv = (MPFRAffineForm(MPFRInterval.pi / MPFRInterval(two))) - this
    conv.sine
  }

  def tangent: MPFRAffineForm = {
    this.sine / this.cosine
  }

  def arccosine: daisy.tools.MPFRAffineForm = ???
  def arcsine: daisy.tools.MPFRAffineForm = ???
  def arctangent: daisy.tools.MPFRAffineForm = ???

  /** Min-range based linear approximation of the exp() function
   */
  def exp: MPFRAffineForm = {
    val (a, b) = (toMPFRInterval.xlo, toMPFRInterval.xhi)

    // Take slope of the left ending point of the interval (which is smaller),
    // probably results in better ranges.
    // Round it down to be sound for convex functions such as exp.
    val alpha = MPFRInterval(expDown(a))
    val dmin = MPFRInterval(a).exp - (MPFRInterval(a) * alpha)
    val dmax = MPFRInterval(b).exp - (MPFRInterval(b) * alpha)

    val zeta = computeZeta(dmin, dmax)
    val delta = max(maxAbs(dmin - zeta), maxAbs(dmax - zeta))
    unaryOp(x0, noise, alpha, zeta, MPFRInterval(delta))
  }

  def log: MPFRAffineForm = {
    val (a, b) = (toMPFRInterval.xlo, toMPFRInterval.xhi)

    if (a <= fzero) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }

    // Take slope of the right ending point of the interval (which is smaller),
    // probably results in better ranges.
    // Round it down to be sound for concave functions such as log.
    val alpha = MPFRInterval(one / b)
    val dmin = MPFRInterval(a).log - (MPFRInterval(a) * alpha)
    val dmax = MPFRInterval(b).log - (MPFRInterval(b) * alpha)

    val zeta = computeZeta(dmin, dmax)
    val delta = max(maxAbs(dmin - zeta), maxAbs(dmax - zeta))
    unaryOp(x0, noise, alpha, zeta, MPFRInterval(delta))
  }

  override def toString: String = "[%s,%s]".format(toMPFRInterval.xlo.doubleValue(), toMPFRInterval.xhi.doubleValue())
  def toSmallString: String = "[%.3f,%.3f]".format(toMPFRInterval.xlo.doubleValue(), toMPFRInterval.xhi.doubleValue())
  def toBigString: String = "[%.18f,%.18f]".format(toMPFRInterval.xlo.doubleValue(), toMPFRInterval.xhi.doubleValue())

  def longString: String =
    "%s +/- %s".format(x0.toString, noise.mkString(", "))

  def detailString: String = x0.toString + " +/- " + radius.doubleValue()

  private def computeZeta(dmin: MPFRInterval, dmax: MPFRInterval): MPFRInterval = {
    val itwo = MPFRInterval(two)
    dmin / itwo +  dmax / itwo
  }

  private def computeDelta(zeta: MPFRInterval, dmin: MPFRInterval, dmax: MPFRInterval): MPFRInterval = {
    MPFRInterval(max((zeta - dmin).xhi, (dmax - zeta).xhi))
  }

  // Int.MaxValue is necessary for correctness, as we compare indices
  private val dummyDev = MPFRDeviation(izero, Int.MaxValue)

  // going outwards: always taking hi
  private def sumAbsQueue(queue: Seq[MPFRDeviation]): MPFRFloat = {
    var sum = fzero
    val iter = queue.iterator
    while(iter.hasNext) {
      sum = sum up_+ MPFRInterval.maxAbs(iter.next().mgnt)
    }
    sum
  }

  private def addQueues(xn: Seq[MPFRDeviation], yn: Seq[MPFRDeviation]): Seq[MPFRDeviation] = {
    var deviation: Seq[MPFRDeviation] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: MPFRDeviation) => deviation :+= xi
    val fy = (yi: MPFRDeviation) => deviation :+= yi

    val fCouple = (xi: MPFRDeviation, yi: MPFRDeviation) => {
      val zi =  xi + yi
      if (!zi.isZero) deviation :+= zi
    }
    MPFRDoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  private def subtractQueues(xn: Seq[MPFRDeviation], yn: Seq[MPFRDeviation]): Seq[MPFRDeviation] = {
    var deviation: Seq[MPFRDeviation] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: MPFRDeviation) => deviation :+= xi
    val fy = (yi: MPFRDeviation) => deviation :+= -yi

    val fCouple = (xi: MPFRDeviation, yi: MPFRDeviation) => {
      val zi =  xi - yi
      if (!zi.isZero) deviation :+= zi
    }
    MPFRDoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  private def multiplyQueuesAndMerge(a: MPFRInterval, xqueue: Seq[MPFRDeviation], b: MPFRInterval,
    yqueue: Seq[MPFRDeviation]): Seq[MPFRDeviation] = {
    var deviation = Seq[MPFRDeviation]()
    val iterX = xqueue.iterator
    val iterY = yqueue.iterator

    val fx = (dev: MPFRDeviation) => {
      val zi = dev * b
      if (!zi.isZero) deviation :+= zi
    }
    val fy = (dev: MPFRDeviation) => {
      val zi = dev * a
      if (!zi.isZero) deviation :+= zi
    }
    val fCouple = (xi: MPFRDeviation, yi: MPFRDeviation) => {
      val zi = xi * b + yi * a
      if (!zi.isZero) deviation :+= zi
    }
    MPFRDoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  // private def multiplyQueuesSimple(xqueue: Seq[MPFRDeviation], yqueue: Seq[MPFRDeviation]): (Rational, Rational) = {
  //   val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))
  //   var zqueue = fzero
  //   var z0Addition = fzero
  //   var i = 0
  //   while (i < indices.length) {
  //     val iInd = indices(i)
  //     // quadratic
  //     val xi = xqueue.find((d: MPFRDeviation) => d.index == iInd) match {
  //       case Some(d) => d.mgnt; case None => Rational(0) }
  //     val yi = yqueue.find((d: MPFRDeviation) => d.index == iInd) match {
  //       case Some(d) => d.mgnt; case None => Rational(0) }
  //     val zii = xi * yi
  //     // z0Addition += zii / Rational(2.0)
  //     if (zii != 0) zqueue += abs(zii)

  //     var j = i + 1
  //     while (j < indices.length) {
  //       val jInd = indices(j)
  //       val xj = xqueue.find((d: MPFRDeviation) => d.index == jInd) match {
  //         case Some(d) => d.mgnt; case None => Rational(0) }
  //       val yj = yqueue.find((d: MPFRDeviation) => d.index == jInd) match {
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
  private def multiplyQueuesOptimized(xqueue: Seq[MPFRDeviation], yqueue: Seq[MPFRDeviation]): (MPFRInterval, MPFRInterval) = {
    val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))
    var zqueue = izero
    var z0Addition = izero

    // println("M-xqueue: " + xqueue)
    // println("M-yqueue: " + yqueue)

    var i = 0
    while (i < indices.length) {
      val iInd = indices(i)
      //println("M-index: " + iInd)
      // quadratic
      val xi = xqueue.find((d: MPFRDeviation) => d.index == iInd) match {
        case Some(d) => d.mgnt; case None => izero }
      val yi = yqueue.find((d: MPFRDeviation) => d.index == iInd) match {
        case Some(d) => d.mgnt; case None => izero }

      val zii = xi * yi
      z0Addition = z0Addition + (zii / MPFRInterval(two))
      if (zii != 0) {
        zqueue = zqueue + MPFRInterval(maxAbs(zii / MPFRInterval(two)))
      }

      var j = i + 1
      while (j < indices.length) {
        val jInd = indices(j)
        val xj = xqueue.find((d: MPFRDeviation) => d.index == jInd) match {
          case Some(d) => d.mgnt; case None => izero }
        val yj = yqueue.find((d: MPFRDeviation) => d.index == jInd) match {
        case Some(d) => d.mgnt; case None => izero }
        val zij = xi * yj + xj * yi
        if (zij != izero) zqueue = zqueue + MPFRInterval(maxAbs(zij))
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
  private def getIndices(q: Seq[MPFRDeviation]): collection.immutable.Set[Int] = {
    var i = 0
    var set = new collection.immutable.HashSet[Int]()
    while (i < q.size) {
      set += q(i).index
      i += 1
    }
    set
  }

  private def multiplyQueue(queue: Seq[MPFRDeviation], factor: MPFRInterval): Seq[MPFRDeviation] = {
    var deviation = Seq[MPFRDeviation]()
    val iter = queue.iterator
    while(iter.hasNext) {
      val xi = iter.next()
      val zi = xi * factor
      if (!zi.isZero) deviation :+= zi
    }
    deviation
  }

  private def unaryOp(x0: MPFRInterval, noise: Seq[MPFRDeviation], alpha: MPFRInterval, zeta: MPFRInterval,
    delta: MPFRInterval): MPFRAffineForm = {

    val z0 = x0 * alpha + zeta
    var deviation = multiplyQueue(noise, alpha)

    if (delta != izero) deviation :+= MPFRDeviation(delta, MPFRAffineIndex.nextGlobal)
    MPFRAffineForm(z0, deviation)
  }


}


// This is probably not the most efficient way, but it's tried and tested.
object MPFRDoubleQueueIterator {

  def iterate(iterX: Iterator[MPFRDeviation], iterY: Iterator[MPFRDeviation],
    dummy: MPFRDeviation, fx: (MPFRDeviation) => Unit, fy: (MPFRDeviation) => Unit,
    fCouple: (MPFRDeviation, MPFRDeviation) => Unit): Unit = {
    var xi: MPFRDeviation = if (iterX.hasNext) iterX.next() else dummy
    var yi: MPFRDeviation = if (iterY.hasNext) iterY.next() else dummy

    var i = 0
    while ((iterX.hasNext || iterY.hasNext)) {
      i = i + 1
      if(xi.index < yi.index) {
        fx(xi)
        xi = if (iterX.hasNext) iterX.next() else dummy
      }
      else if (yi.index < xi.index) {
        fy(yi)
        yi = if (iterY.hasNext) iterY.next() else dummy
      }
      else {
        fCouple(xi, yi)
        xi = if (iterX.hasNext) iterX.next() else dummy
        yi = if (iterY.hasNext) iterY.next() else dummy
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
