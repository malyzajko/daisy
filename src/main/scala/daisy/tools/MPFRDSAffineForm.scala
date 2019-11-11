// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import lang.Identifiers._
import utils.UniqueCounter
import MPFRFloat.{zero => fzero, one => fone, two => ftwo, _}
import MPFRInterval.{zero => izero, _}

private[tools] case class MPFRDSDeviation(mgnt: MPFRInterval, index: Int, dependency: Set[Int], distrib: MPFRDSInterval) {

  assert(dependency.contains(index))
  val x = MPFRInterval +/- fone
  assert((MPFRInterval +/- fone).includes(distrib.toMPFRInterval))  // noise terms are in [-1, 1]

  def unary_-(): MPFRDSDeviation = MPFRDSDeviation(-mgnt, index, dependency, distrib)

  def +(y: MPFRDSDeviation): MPFRDSDeviation = {
    assert(this.index == y.index && this.dependency == y.dependency && this.distrib == y.distrib,
      "this dependency: " + this.dependency + ", y dependency: " + y.dependency + ", this.distrib: " + this.distrib +
      ", y.distrib: " + y.distrib)
    MPFRDSDeviation(this.mgnt + y.mgnt, index, dependency, distrib)
  }
  def -(y: MPFRDSDeviation): MPFRDSDeviation = {
    assert(this.index == y.index && this.dependency == y.dependency && this.distrib == y.distrib)
    MPFRDSDeviation(this.mgnt - y.mgnt, index, dependency, distrib)
  }
  def *(factor: MPFRInterval): MPFRDSDeviation = {
    MPFRDSDeviation(this.mgnt * factor, index, dependency, distrib)
  }

  def isZero: Boolean = (mgnt == MPFRInterval.zero)

  //override def toString: String = "%s(%d)".format(mgnt.toString, index)
  override def toString: String = "[%s, %d, %s, %s]".format(mgnt.toString, index, dependency.toString, distrib.toString)
}

// to be called with nextGlobal
private[tools] object MPFRDSAffineIndex extends UniqueCounter[Int]

object MPFRDSAffineForm {

  def apply(i: MPFRInterval, ds: MPFRDSInterval): MPFRDSAffineForm = {
    val a = i.xlo
    val b = i.xhi
    if (a == b) {
      MPFRDSAffineForm(MPFRInterval(a), Seq[MPFRDSDeviation]())
    } else {
      val un = (b - a)/ ftwo
      val fresh = MPFRDSAffineIndex.nextGlobal
      MPFRDSAffineForm(MPFRInterval(a + un), Seq(MPFRDSDeviation(MPFRInterval(un), fresh, Set(fresh), ds)))
    }
  }

  def applyInputDependencyOnGaussian (input: Map[Identifier, (MPFRInterval, MPFRFloat, MPFRDSInterval)]) : Map[Identifier, MPFRDSAffineForm] = {
    var depNew = Set[Int]()
    var fresh  = List[Int]()
    var depDsAffineInput = collection.mutable.Map[Identifier, MPFRDSAffineForm]()
    val noOfElem = input.size
    for (i <- 0 until noOfElem) {
      fresh = fresh:+  MPFRDSAffineIndex.nextGlobal
      depNew = depNew + fresh(i)
    }
    var j = 0 // the loop variable has to be initialized outside of the loop!!!
    for ((id, (range, weight, ds)) <- input) { /*do something with (input,weight)*/
      val a = range.xlo
      val b = range.xhi
      if (a == b) {
        depDsAffineInput += (id -> MPFRDSAffineForm(MPFRInterval(a), Seq[MPFRDSDeviation]()))
      } else {
        val un = (b - a)/ ftwo
        depDsAffineInput += (id -> MPFRDSAffineForm(MPFRInterval(a + un), Seq(MPFRDSDeviation(MPFRInterval(un), fresh(j), depNew, ds))))
      }
      j = j+1
    }
    depDsAffineInput.toMap
  }

  def applyInputDependency (input: Map[Identifier, MPFRInterval], ds: MPFRDSInterval) : Map[Identifier, MPFRDSAffineForm] = {
    var depNew = Set[Int]()
    var fresh  = List[Int]()
    var depDsAffineInput = collection.mutable.Map[Identifier, MPFRDSAffineForm]()
    val noOfElem = input.size
    for (i <- 0 until noOfElem) {
      fresh = fresh:+  MPFRDSAffineIndex.nextGlobal
      depNew = depNew + fresh(i)
    }
    var j = 0   // the loop variable has to be initialized outside of the loop!!!
    for ((id, range) <- input) {
      val a = range.xlo
      val b = range.xhi
      if (a == b) {
        depDsAffineInput += (id -> MPFRDSAffineForm(MPFRInterval(a), Seq[MPFRDSDeviation]()))
      } else {
        val un = (b - a)/ ftwo
        depDsAffineInput += (id -> MPFRDSAffineForm(MPFRInterval(a + un), Seq(MPFRDSDeviation(MPFRInterval(un), fresh(j), depNew, ds))))
      }
      j = j+1
    }
    depDsAffineInput.toMap
  }

  def apply(r:  MPFRFloat): MPFRDSAffineForm = MPFRDSAffineForm(MPFRInterval(r), Seq[MPFRDSDeviation]())
  def apply(i:  MPFRInterval): MPFRDSAffineForm = MPFRDSAffineForm(i, Seq[MPFRDSDeviation]())

  def apply(r: MPFRFloat, mgnt: MPFRFloat, dsinterval: MPFRDSInterval): MPFRDSAffineForm = {
    val fresh = MPFRDSAffineIndex.nextGlobal
    MPFRDSAffineForm(MPFRInterval(r), Seq(MPFRDSDeviation(MPFRInterval(mgnt), fresh, Set(fresh), dsinterval)))
  }

  // TODO: subject to reconsider if this is good assumption:
  //    assuming independency of noise terms
  def +/-(x: Rational, dist: MPFRDSInterval): MPFRDSAffineForm = {
    val fresh = MPFRDSAffineIndex.nextGlobal
     MPFRDSAffineForm(izero, Seq(MPFRDSDeviation(MPFRInterval(x), fresh,
        Set(fresh), dist)))
  }
  def +/-(x: MPFRFloat, dist: MPFRDSInterval): MPFRDSAffineForm = {
    val fresh = MPFRDSAffineIndex.nextGlobal
     MPFRDSAffineForm(izero, Seq(MPFRDSDeviation(MPFRInterval(x), fresh,
        Set(fresh), dist)))
  }

}


case class MPFRDSAffineForm(x0:  MPFRInterval, noise: Seq[MPFRDSDeviation]) extends RangeArithmetic[MPFRDSAffineForm] {
  if (noise.size > 200) {
    System.err.println("200 noise terms")
  }
  // Int.MaxValue is necessary for correctness, as we compare indices
  val dummyDev = MPFRDSDeviation(izero, Int.MaxValue, Set(Int.MaxValue), MPFRDSInterval(izero, fone))
  lazy val radius: MPFRFloat = sumAbsQueue(noise)
  lazy val toMPFRInterval: MPFRInterval = {
    val rad = radius
    MPFRInterval(x0.xlo down_- rad, x0.xhi up_+ rad)
  }

  lazy val toInterval: Interval = {
    Interval(Rational.fromString(toMPFRInterval.xlo.toString),
      Rational.fromString(toMPFRInterval.xhi.toString))
  }

  def addConstraint(e: Set[lang.Trees.Expr]): MPFRDSAffineForm = this


  //Greedy algorithm
  lazy val toDSStruct: MPFRDSInterval = {
    var affineForm = new ListBuffer[MPFRDSDeviation]
    var clusterSum =  new ListBuffer[MPFRDSInterval]
    var sum = MPFRDSInterval(MPFRInterval(x0), fone)
    for (i <- 0 to noise.length-1) {
      affineForm += noise(i)
    }
    // Greedily adding the independent variables
    for (i <- noise) {
      if (affineForm.contains(i)) {
        val iter1 = affineForm.iterator
        val iter2 = iter1 take 1
        val firstTerm = iter2.next
        var sumIndep = firstTerm.distrib * firstTerm.mgnt
        var accumDep = firstTerm.dependency    //accumulated dependencies
        affineForm -= firstTerm
        while (iter1.hasNext) {
          val next = iter1.next
          if (accumDep.intersect(next.dependency).isEmpty) {
          // independent case
            sumIndep = sumIndep i_+ (next.distrib * next.mgnt)
            accumDep = accumDep ++ next.dependency
            affineForm -= next
          }
        }
        clusterSum += sumIndep
      }
    }
    sum = clusterSum.foldLeft(sum)((a, b) => a.d_+(b, verbose = false)) //Adding dependent terms
    clusterSum.clear
    sum

  }

  lazy val allDependencies: Set[Int] = {

    var dep = Set[Int]()
    val iter = noise.iterator
    while(iter.hasNext) {
      val next = iter.next
      dep = dep ++ next.dependency
    }
    dep
  }
  /**
    Adds a new error/noise term with magniture r,
    and leaves all else as before.
    This is only needed when AA is used to track errors.
   */

   def :+(r: MPFRInterval): MPFRDSAffineForm = {
    val fresh = MPFRDSAffineIndex.nextGlobal
    val ds = MPFRDSInterval(MPFRInterval(fone), fone)

    MPFRDSAffineForm(x0: MPFRInterval, noise :+ MPFRDSDeviation(r, fresh, Set(fresh), ds)) //TODO: please check this
   }

  def :+(mgnt: MPFRInterval, dsi: MPFRDSInterval, dependencies: Set[Int]): MPFRDSAffineForm = {

    val fresh = MPFRDSAffineIndex.nextGlobal
    MPFRDSAffineForm(x0: MPFRInterval, noise :+ MPFRDSDeviation(mgnt, fresh, dependencies + fresh, dsi))

  }

  def +/-(r: MPFRInterval): MPFRDSAffineForm = this :+ r
  // for compatibility with RangeArithmetic
  def +/-(r: Rational): MPFRDSAffineForm =  this :+ MPFRInterval(r)

  def unary_-(): MPFRDSAffineForm = {
    var newTerms: Seq[MPFRDSDeviation] = Seq.empty
    var iter = noise.iterator
    while(iter.hasNext) {
      newTerms :+= - iter.next  // just flip the sign
    }
    MPFRDSAffineForm(-x0, newTerms)
  }

  def +(y: MPFRDSAffineForm): MPFRDSAffineForm = {
    MPFRDSAffineForm(this.x0 + y.x0, addQueues(this.noise, y.noise))
  }

  def -(y: MPFRDSAffineForm): MPFRDSAffineForm = {
    MPFRDSAffineForm(this.x0 - y.x0, subtractQueues(this.noise, y.noise))
  }

  def *(y: MPFRDSAffineForm): MPFRDSAffineForm = {

    if ((!(y.noise).isEmpty) & (!(this.noise).isEmpty)) {
    // linear part, as before
      var newTerms: Seq[MPFRDSDeviation] = multiplyLinearPart(this.x0, this.noise, y.x0, y.noise)
      // non-linear part

      val (z0Addition, nonlinMgnt, nonlinDS) = multiplyQueuesOptimized(this.noise, y.noise)

      val nonlinDS_normalized = (nonlinDS * (MPFRInterval(fone) / nonlinMgnt))
      // new central value
      val z0 = this.x0 * y.x0 + z0Addition
      if(nonlinMgnt != izero) {
        val freshIndex = MPFRDSAffineIndex.nextGlobal
        // the new term has unknown dependency with all other noise terms
        val freshDependency = Set(freshIndex) ++ this.allDependencies ++ y.allDependencies

        newTerms :+= MPFRDSDeviation(nonlinMgnt, freshIndex, freshDependency, nonlinDS_normalized)
      }
      MPFRDSAffineForm(z0, newTerms)
    }
    else if ((y.noise).isEmpty) this * y.x0
    else y * this.x0

  }


  def *(r: Rational): MPFRDSAffineForm = {
    val factor = MPFRInterval(r)
    MPFRDSAffineForm(x0 * factor, multiplyQueue(noise, factor))
  }

  def *(r: MPFRFloat): MPFRDSAffineForm = {
    val factor = MPFRInterval(r)
    MPFRDSAffineForm(x0 * factor, multiplyQueue(noise, factor))
  }
  def *(r: MPFRInterval): MPFRDSAffineForm = {
    MPFRDSAffineForm(x0 * r, multiplyQueue(noise, r))
  }

    //Computes the inverse of this DSAffineForm as a linear approximation.
  def inverse: MPFRDSAffineForm = {
    val ione = MPFRInterval(fone)
    val (xlo, xhi) = (toMPFRInterval.xlo, toMPFRInterval.xhi)
    if (xlo <= fzero && xhi >= fzero)
      throw DivisionByZeroException("Possible division by zero: " + toString)
    if(noise.size == 0) { // exact
      val inv = ione / x0
      MPFRDSAffineForm(inv, Seq[MPFRDSDeviation]())
    } else {

      /* Calculate the inverse */
      val a = min(abs(xlo), abs(xhi))
      val b = max(abs(xlo), abs(xhi))

      val alpha = MPFRInterval(-fone / (b * b))

      val dmax = (ione / MPFRInterval(a)) - (MPFRInterval(a) * alpha)
      val dmin = (ione / MPFRInterval(b)) - (MPFRInterval(b) * alpha)

      var zeta = computeZeta(dmin, dmax)
      if (xlo < fzero) zeta = -zeta
      val delta = MPFRInterval(max((zeta - dmin).xhi, (dmax - zeta).xhi))


      val z0 = alpha * this.x0 + zeta

      var newTerms = multiplyQueue(noise, alpha)
      //val ds = this.distrib

      if(delta != izero) {
        val freshIndex = MPFRDSAffineIndex.nextGlobal
      // the new term has unknown dependency with all other noise terms

        val inverseDependency = Set(freshIndex) ++ this.allDependencies
        val dsStructure = MPFRDSInterval(ione, fone)
        newTerms :+= MPFRDSDeviation(delta, freshIndex, inverseDependency, dsStructure)
      }
      MPFRDSAffineForm(z0, newTerms)
    }
  }

  def /(y: MPFRDSAffineForm): MPFRDSAffineForm = {
    if ((y.noise).isEmpty) this * (MPFRInterval(fone) / y.x0)
    else this * y.inverse
  }

  def ^(_n: Int): MPFRDSAffineForm = ??? /*{
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
  } */

  def squareRoot: MPFRDSAffineForm = ???/*{
    var (a, b) = (toInterval.xlo, toInterval.xhi)

    if (b < rzero) {
     throw NegativeSqrtException("Sqrt of negative number: " + toString)
    }
    // if (a < rzero) a = rzero  //soft policy

    /* if(noise.size == 0) { //exact
      val sqrt = x0.sqrt
      //val maxError = ...  can we get the error on this?
      val maxError = zero
      return new XRationalForm(sqrt, new Queue[DSDeviation](DSDeviation(newIndex, maxError)))
    } */

    val alpha = Rational(1L, 2L) / sqrtUp(b)
    val dmin = sqrtDown(a) - (alpha * a)
    val dmax = sqrtUp(b) - (alpha * b)

    val zeta = computeZeta(dmin, dmax)
    val delta = computeDelta(zeta, dmin, dmax)
    unaryOp(x0, noise, alpha, zeta, delta)
  } */

  /** Min-range based approximating implementation of sine
   */
  def sine: MPFRDSAffineForm = ??? /*{
    val (l, u) = (toInterval.xlo, toInterval.xhi)

    val intsol = toInterval.sine
    if (intsol.xlo == -1 || intsol.xhi == 1) {
      // we are not in a monotone part of sine, so we use interval results
      DSAffineForm(intsol)
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
  } */

  def cosine: MPFRDSAffineForm = ??? /*{
    val conv = (DSAffineForm(Interval.pi) / DSAffineForm(Interval(2, 2))) - this
    conv.sine
  }*/

  def tangent: MPFRDSAffineForm = ??? /*{
    this.sine / this.cosine
  }*/

  /** Min-range based linear approximation of the exp() function
   */
  def exp: MPFRDSAffineForm = {
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

  def log: MPFRDSAffineForm = {
    val (a, b) = (toMPFRInterval.xlo, toMPFRInterval.xhi)

    if (a <= fzero) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }

    // Take slope of the right ending point of the interval (which is smaller),
    // probably results in better ranges.
    // Round it down to be sound for concave functions such as log.
    val alpha = MPFRInterval(fone / b)
    val dmin = MPFRInterval(a).log - (MPFRInterval(a) * alpha)
    val dmax = MPFRInterval(b).log - (MPFRInterval(b) * alpha)

    val zeta = computeZeta(dmin, dmax)
    val delta = max(maxAbs(dmin - zeta), maxAbs(dmax - zeta))
    unaryOp(x0, noise, alpha, zeta, MPFRInterval(delta))

  }

  //creates joint union
  def union(y: MPFRDSAffineForm): MPFRDSAffineForm = { ???

    /*def normalize( newX0: MPFRInterval)(d: MPFRDSDeviation ): MPFRDSDeviation = {

      val negDiff = maxAbs(newX0 - (y.x0 - d.mgnt))
      val posDiff = maxAbs(newX0 - (y.x0 + d.mgnt))

      def newCoord(coord: MPFRInterval, newMagnitude: MPFRInterval): 
        MPFRInterval = ( (y.x0 - newX0) + coord * d.mgnt) / newMagnitude

      if(negDiff > posDiff)
        DSDeviation(negDiff, d.index, d.dependency, DSInterval(d.distrib.dsi.map( x => (  Interval( newCoord(x._1.xlo, negDiff), newCoord(x._1.xhi, negDiff) ) , x._2) )))
        else
        DSDeviation(posDiff, d.index, d.dependency, DSInterval(d.distrib.dsi.map( x => (  Interval( newCoord(x._1.xlo, posDiff), newCoord(x._1.xhi, posDiff) ) , x._2) )))
    }

    if( this.x0 == y.x0 )
      DSAffineForm( this.x0,  addQueues(this.noise, y.noise) )

    else
      DSAffineForm( this.x0, addQueues(this.noise, y.noise.map(normalize(this.x0))))*/

  }

  //returns max of two DSAffine forms
  def maximizingUnion(y: MPFRDSAffineForm): MPFRDSAffineForm = {

    ???

  }
  def arccosine: daisy.tools.MPFRDSAffineForm = ???

  def arcsine: daisy.tools.MPFRDSAffineForm = ???

  def arctangent: daisy.tools.MPFRDSAffineForm = ???

  override def toString: String = "<[%s, %s], \n%s>".format(toMPFRInterval.xlo.doubleValue(), toMPFRInterval.xhi.doubleValue(), noise.mkString("\n"))
  def toSmallString: String = "[%.3f,%.3f]".format(toMPFRInterval.xlo.doubleValue(), toMPFRInterval.xhi.doubleValue())
  def toBigString: String = "[%.18f,%.18f]".format(toMPFRInterval.xlo.doubleValue(), toMPFRInterval.xhi.doubleValue())

  def longString: String =
    "%s +/- %s".format(x0.toString, noise.mkString(", "))

  def detailString: String = x0 + " +/- " + MPFRInterval(radius)

  // computes the linear portion of the multiplication
  private def multiplyLinearPart(a: MPFRInterval, xqueue: Seq[MPFRDSDeviation], b: MPFRInterval,
    yqueue: Seq[MPFRDSDeviation]): Seq[MPFRDSDeviation] = {
    var deviation = Seq[MPFRDSDeviation]()
    val iterX = xqueue.iterator
    val iterY = yqueue.iterator

    val fx = (dev: MPFRDSDeviation) => {
      val zi = dev * b
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    val fy = (dev: MPFRDSDeviation) => {
      val zi = dev * a
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    val fCouple = (xi: MPFRDSDeviation, yi: MPFRDSDeviation) => {
      val zi = xi * b + yi * a
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    MPFRDSDoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  // Does an optimized computation of the nonlinear part of multiplication
  private def multiplyQueuesOptimized(xqueue: Seq[MPFRDSDeviation], yqueue: Seq[MPFRDSDeviation]):
    (MPFRInterval, MPFRInterval, MPFRDSInterval) = {
    //println("\nInside NonLinear part")
    val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))
    val indepIndices = Set[Int]()  // TODO: find mutually independent indices

    // magnitude of nonlinear part
    var zqueue = izero
    var z0Addition = izero
    // DS of nonlinear part
    var indepDS_square = MPFRDSInterval(izero, fone)
    var depDS_square = MPFRDSInterval(izero, fone)
    var depDS = MPFRDSInterval(izero, fone)
    var i = 0
    while (i < indices.length) {
      val iInd = indices(i)

      // square part magnitude
      val (xi, xiDev) = xqueue.find((d: MPFRDSDeviation) => d.index == iInd) match {
        case Some(d) => (d.mgnt, d); case None => (izero, null) }
      val (yi, yiDev) = yqueue.find((d: MPFRDSDeviation) => d.index == iInd) match { //Getting None for y in case of division by rational converted to DSAffineForm
        case Some(d) => (d.mgnt, d); case None => (izero, null) }
      val zii = xi * yi
      if (!isZero(zii)) {
        z0Addition = z0Addition + (zii / MPFRInterval(ftwo))
        zqueue = zqueue + MPFRInterval(maxAbs(zii / MPFRInterval(ftwo)))
      }
      // square part DS
      if (!isZero(zii)) {
        if (indepIndices.contains(i)) {
          assert(xiDev.distrib == yiDev.distrib)

        // val factor = zii
        // val squareDS = square(xi.distrib)
          indepDS_square = (indepDS_square i_+ (xiDev.distrib.square * zii))

        } else {
          //assert(xiDev.distrib == yiDev.distrib)

          depDS_square = (depDS_square d_+ (xiDev.distrib.square * zii))
        }
      }
      var j = i + 1
      while (j < indices.length) {
        val jInd = indices(j)

        val (xj, xjDev) = xqueue.find((d: MPFRDSDeviation) => d.index == jInd) match {
          case Some(d) => (d.mgnt, d); case None => (izero, null) }

        val (yj, yjDev) = yqueue.find((d: MPFRDSDeviation) => d.index == jInd) match { //Getting None for y in case of division by rational converted to DSAffineForm
          case Some(d) => (d.mgnt, d); case None => (izero, null) }

        // non-square magnitude
        val zij = xi * yj + xj * yi
        if (zij!= izero) {
          zqueue += MPFRInterval(maxAbs(zij))
        }

        // non-square DS
        if (zij!= izero) {
        //if (zij == 0) { // some j has to be there
          assert(xjDev == null || yjDev == null || xjDev.distrib == yjDev.distrib)


          val iDistrib = if (xiDev != null) xiDev.distrib else yiDev.distrib
          val jDistrib = if (xjDev != null) xjDev.distrib else yjDev.distrib

          depDS = depDS d_+ ((iDistrib d_* jDistrib) * zij) //it gives [0, 0] as interval if zij == 0
        }

        j = j + 1
      }
      i = i + 1
    }

    // TODO: check this, is the first addition dependent or independent?

    val finalDS = ((indepDS_square d_+ depDS_square) * MPFRFloat.fromDouble(0.5)) d_+ depDS
    (z0Addition, zqueue, finalDS)
  }

  private def computeZeta(dmin: MPFRInterval, dmax: MPFRInterval): MPFRInterval = {
    val itwo = MPFRInterval(ftwo)
    dmin / itwo +  dmax / itwo
  }
   private def computeDelta(zeta: MPFRInterval, dmin: MPFRInterval, dmax: MPFRInterval): MPFRInterval = {
    MPFRInterval(max((zeta - dmin).xhi, (dmax - zeta).xhi))
  }

  private def mergeIndices(x: Set[Int], y: Set[Int]): Array[Int] = {
    val set = x ++ y
    val list = set.toList.sorted
    list.toArray
  }

  // Do this with some functional thing?
  private def getIndices(q: Seq[MPFRDSDeviation]): collection.immutable.Set[Int] = {
    var i = 0
    var set = new collection.immutable.HashSet[Int]()
    while (i < q.size) {
      set += q(i).index
      i += 1
    }
    set
  }

  private def addQueues(xn: Seq[MPFRDSDeviation], yn: Seq[MPFRDSDeviation]): Seq[MPFRDSDeviation] = {
    var deviation: Seq[MPFRDSDeviation] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: MPFRDSDeviation) => deviation :+= xi
    val fy = (yi: MPFRDSDeviation) => deviation :+= yi

    val fCouple = (xi: MPFRDSDeviation, yi: MPFRDSDeviation) => {
      val zi =  xi + yi
      if (!zi.isZero) deviation :+= zi
    }
    MPFRDSDoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }

  private def subtractQueues(xn: Seq[MPFRDSDeviation], yn: Seq[MPFRDSDeviation]): Seq[MPFRDSDeviation] = {
    var deviation: Seq[MPFRDSDeviation] = Seq.empty
    val iterX = xn.iterator
    val iterY = yn.iterator

    val fx = (xi: MPFRDSDeviation) => deviation :+= xi
    val fy = (yi: MPFRDSDeviation) => deviation :+= -yi

    val fCouple = (xi: MPFRDSDeviation, yi: MPFRDSDeviation) => {
      val zi =  xi - yi
      if (!zi.isZero) deviation :+= zi
    }
    MPFRDSDoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }


   private def multiplyQueue(queue: Seq[MPFRDSDeviation], factor: MPFRInterval): Seq[MPFRDSDeviation] = {
     var deviation = Seq[MPFRDSDeviation]()
     val iter = queue.iterator
     while(iter.hasNext) {
       val xi = iter.next
       val zi = xi * factor
       if (!zi.isZero) deviation :+= zi
     }
     deviation
   }

   private def sumAbsQueue(queue: Seq[MPFRDSDeviation]): MPFRFloat = {
    var sum = fzero
    val iter = queue.iterator
    while(iter.hasNext) {
      sum = sum up_+ MPFRInterval.maxAbs(iter.next.mgnt)
    }
    sum
  }

   private def unaryOp(x0: MPFRInterval, noise: Seq[MPFRDSDeviation], alpha: MPFRInterval, zeta: MPFRInterval, delta: MPFRInterval): 
     MPFRDSAffineForm = {

    val z0 = alpha * x0 + zeta
    var deviation = multiplyQueue(noise, alpha)
    val freshIndex = MPFRAffineIndex.nextGlobal
    val dep = Set(freshIndex) ++ this.allDependencies
        val dsStructure = MPFRDSInterval(MPFRInterval(fone), fone)

    if (delta != izero) deviation :+= MPFRDSDeviation(delta, freshIndex, dep, dsStructure)
    MPFRDSAffineForm(z0, deviation)
  }

}
object MPFRDSDoubleQueueIterator {

  def iterate(iterX: Iterator[MPFRDSDeviation], iterY: Iterator[MPFRDSDeviation],
    dummy: MPFRDSDeviation, fx: (MPFRDSDeviation) => Unit, fy: (MPFRDSDeviation) => Unit,
    fCouple: (MPFRDSDeviation, MPFRDSDeviation) => Unit): Unit = {
    var xi: MPFRDSDeviation = if (iterX.hasNext) iterX.next else dummy
    var yi: MPFRDSDeviation = if (iterY.hasNext) iterY.next else dummy

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
