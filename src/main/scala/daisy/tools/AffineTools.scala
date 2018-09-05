package daisy
package tools

import scala.collection.immutable.Seq

import Rational.{zero => rzero, one => rone, _}

trait NoiseTerm[T] { self: T =>

  def unary_-(): T
  def +(other: T): T
  def -(other: T): T
  def *(r: Rational): T
  val index: Int
  val mgnt: Rational
  def isZero: Boolean
}

trait AffineTools[T <: NoiseTerm[T]] {

  val dummyDev: T

  def sumAbsQueue(queue: Seq[T]): Rational = {
    var sum = rzero
    val iter = queue.iterator
    while(iter.hasNext) {
      sum += Rational.abs(iter.next.mgnt)
    }
    sum
  }

  def computeZeta(dmin: Rational, dmax: Rational): Rational = {
    dmin / two +  dmax / two
  }

  def computeDelta(zeta: Rational, dmin: Rational, dmax: Rational): Rational = {
    max(zeta - dmin,  dmax - zeta)
  }

  def mergeIndices(x: Set[Int], y: Set[Int]): Array[Int] = {
    val set = x ++ y
    val list = set.toList.sorted
    list.toArray
  }

  // Do this with some functional thing?
  def getIndices(q: Seq[T]): collection.immutable.Set[Int] = {
    var i = 0
    var set = new collection.immutable.HashSet[Int]()
    while (i < q.size) {
      set += q(i).index
      i += 1
    }
    set
  }


  def addQueues(xn: Seq[T], yn: Seq[T]): Seq[T] = {
    var deviation: Seq[T] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: T) => { deviation :+= xi; val x = 0 }
    val fy = (yi: T) => { deviation :+= yi; val x = 0 }

    val fCouple = (xi: T, yi: T) => {
      val zi =  xi + yi
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    DoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  def subtractQueues(xn: Seq[T], yn: Seq[T]): Seq[T] = {
    var deviation: Seq[T] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: T) => { deviation :+= xi; val x = 0 }
    val fy = (yi: T) => { deviation :+= -yi; val x = 0 }

    val fCouple = (xi: T, yi: T) => {
      val zi =  xi - yi
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    DoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }



  object DoubleQueueIterator {

    def iterate(iterX: Iterator[T], iterY: Iterator[T],
      dummy: T, fx: (T) => Unit, fy: (T) => Unit, fCouple: (T, T) => Unit): Unit = {

      var xi: T = if (iterX.hasNext) iterX.next else dummy
      var yi: T = if (iterY.hasNext) iterY.next else dummy

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


}