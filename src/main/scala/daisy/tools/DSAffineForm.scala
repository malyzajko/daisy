// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools


import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import lang.Identifiers._

import utils.UniqueCounter
import Rational.{zero => rzero, one => rone, _}


private[tools] case class DSDeviation(mgnt: Rational, index: Int, dependency: Set[Int],
  distrib: DSInterval) extends NoiseTerm[DSDeviation] {
  //println("printing from DSDeviation")
  //println(dependency)
  //println(index)
  assert(dependency.contains(index))
  assert((Interval +/- rone).includes(distrib.toInterval))  // noise terms are in [-1, 1]

  def unary_-(): DSDeviation = DSDeviation(-mgnt, index, dependency, distrib)

  def +(y: DSDeviation): DSDeviation = {
    assert(this.index == y.index && this.dependency == y.dependency && this.distrib == y.distrib,
      "this dependency: " + this.dependency + ", y dependency: " + y.dependency + ", this.distrib: " + this.distrib +
      ", y.distrib: " + y.distrib)
    DSDeviation(this.mgnt + y.mgnt, index, dependency, distrib)
  }
  def -(y: DSDeviation): DSDeviation = {
    assert(this.index == y.index && this.dependency == y.dependency && this.distrib == y.distrib)
    DSDeviation(this.mgnt - y.mgnt, index, dependency, distrib)
  }
  def *(factor: Rational): DSDeviation = {
    DSDeviation(this.mgnt * factor, index, dependency, distrib)
  }
  def isZero: Boolean = (mgnt == Rational.zero)

  //override def toString: String = "%s(%d)".format(mgnt.toString, index)
  override def toString: String = "[%s, %d, %s, %s]".format(mgnt.toString, index, dependency.toString, distrib.toString)
}

// to be called with nextGlobal
private[tools] object DSAffineIndex extends UniqueCounter[Int]

object DSAffineForm {

  def apply(i: Interval, ds: DSInterval): DSAffineForm = {
    val a = i.xlo
    val b = i.xhi
    if (a == b) {
      DSAffineForm(a, Seq[DSDeviation]())
    } else {
      val un = (b - a)/ Rational(2)
      val fresh = DSAffineIndex.nextGlobal
      DSAffineForm(a + un, Seq(DSDeviation(un, fresh, Set(fresh), ds)))
    }
  }

  def applyInputDependencyOnGaussian (input: Map[Identifier, (Interval, Rational, DSInterval)]) : Map[Identifier, DSAffineForm] = {
    var depNew = Set[Int]()
    var fresh  = List[Int]()
    var depDsAffineInput = collection.mutable.Map[Identifier, DSAffineForm]()
    val noOfElem = input.size
    for (i <- 0 until noOfElem) {
      fresh = fresh:+  DSAffineIndex.nextGlobal
      depNew = depNew + fresh(i)
    }
    var j = 0 // the loop variable has to be initialized outside of the loop!!!
    for ((id, (range, weight, ds)) <- input) { /*do something with (input,weight)*/
      val a = range.xlo
      val b = range.xhi
      if (a == b) {
        depDsAffineInput += (id -> DSAffineForm(a, Seq[DSDeviation]()))
      } else {
        val un = (b - a)/ Rational(2)
        depDsAffineInput += (id -> DSAffineForm(a + un, Seq(DSDeviation(un, fresh(j), depNew, ds))))
      }
      j = j+1
    }
    depDsAffineInput.toMap
  }

  def applyInputDependency (input: Map[Identifier, Interval], ds: DSInterval) : Map[Identifier, DSAffineForm] = {
    var depNew = Set[Int]()
    var fresh  = List[Int]()
    var depDsAffineInput = collection.mutable.Map[Identifier, DSAffineForm]()
    val noOfElem = input.size
    for (i <- 0 until noOfElem) {
      fresh = fresh:+  DSAffineIndex.nextGlobal
      depNew = depNew + fresh(i)
    }
    var j = 0   // the loop variable has to be initialized outside of the loop!!!
    for ((id, range) <- input) {
      val a = range.xlo
      val b = range.xhi
      if (a == b) {
        depDsAffineInput += (id -> DSAffineForm(a, Seq[DSDeviation]()))
      } else {
        val un = (b - a)/ Rational(2)
        depDsAffineInput += (id -> DSAffineForm(a + un, Seq(DSDeviation(un, fresh(j), depNew, ds))))
      }
      j = j+1
    }
    depDsAffineInput.toMap
  }

  def apply(r: Rational): DSAffineForm = DSAffineForm(r, Seq[DSDeviation]())

  // def +/-(x: Rational): DSAffineForm = {
  //   DSAffineForm(Rational.zero, Seq(DSDeviation(x, DSAffineIndex.nextGlobal)))
  // }

  //val zero: DSAffineForm = DSAffineForm(Rational.zero, Seq())
}


case class DSAffineForm(x0: Rational, noise: Seq[DSDeviation]) extends RangeArithmetic[DSAffineForm]
  with AffineTools[DSDeviation] {
  if (noise.size > 200) {
    System.err.println("200 noise terms")
  }
  // Int.MaxValue is necessary for correctness, as we compare indices
  val dummyDev = DSDeviation(rzero, Int.MaxValue, Set(Int.MaxValue), DSInterval(Interval(rzero), rone))

  lazy val radius: Rational = sumAbsQueue(noise)

  lazy val toInterval: Interval = {
    val rad = radius
    Interval(x0 - rad, x0 + rad)
  }

  def addConstraint(e: Set[lang.Trees.Expr]): DSAffineForm = this

  // concretization to not the simple interval, but to a distribution

  // naive implementation: we let the deviations handle the dependency
  // to be more optimal, we should be maximizing independent additions
  /*lazy val toDSStruct: StructDS = {

    // TODO: check this starting term
    var sum = StructDS(Interval(x0), rone)
    var accumDep = Set[Int]()    //accumulated dependencies

    val iter = noise.iterator
    while(iter.hasNext) {
      val next = iter.next
      if (accumDep.intersect(next.dependency).isEmpty) {
        // independent case
        //println(s"toDSStruct: Indep of $accumDep and ${next.dependency}")
        //indepOperations += 1
        sum = Indep_+(sum, mult(next.distrib, next.mgnt))
      } else {

        // there is some dependency
        //depOperations += 1
        //println(s"toDSStruct: Length of sum: ${sum.length}, length of next: ${next.distrib.length}")
        //println(s"Sum before dependent op: $sum")
        sum = Dep_+(sum, mult(next.distrib, next.mgnt), verbose = false)

      }
      accumDep = accumDep ++ next.dependency
    }
    //println("\nExiting toDSStruct")
    sum
  }*/


  //Greedy algorithm
  lazy val toDSStruct: DSInterval = {
    //println("\nEntering toDSStruct")
    var affineForm = new ListBuffer[DSDeviation]
    var clusterSum =  new ListBuffer[DSInterval]
    var sum = DSInterval(Interval(x0), rone)
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
    //println("\nExiting toDSStruct")
    sum

  }

  // lazy val toDSStruct: StructDS = {
  //   //println("\nEntering toDSStruct")
  //   var affineForm = new ListBuffer[DSDeviation]
  //   var clusterSum =  new ListBuffer[StructDS]
  //   var sum = StructDS(Interval(x0), rone)
  //   for (i <- 0 to noise.length-1) {
  //     affineForm += noise(i)
  //   }
  //   // Greedily adding the independent variables
  //   for (i <- noise) {
  //     if (affineForm.contains(i)) {
  //       val iter1 = affineForm.iterator
  //       val iter2 = iter1 take 1
  //       val firstTerm = iter2.next
  //       var sumIndep = mult(firstTerm.distrib, firstTerm.mgnt)
  //       var accumDep = firstTerm.dependency    //accumulated dependencies
  //       affineForm -= firstTerm
  //       while (iter1.hasNext) {
  //         val next = iter1.next
  //         if (accumDep.intersect(next.dependency).isEmpty) {
  //         // independent case
  //           println(s"Indep of $accumDep and ${next.dependency}")
  //           sumIndep = Indep_+(sumIndep, mult(next.distrib, next.mgnt))
  //           accumDep = accumDep ++ next.dependency
  //           affineForm -= next
  //         }
  //       }
  //       clusterSum += sumIndep
  //     }
  //   }
  //   println(s"After accumulating independent terms:\n $clusterSum")
  //   sum = clusterSum.foldLeft(sum)((a, b) => Dep_+(a, b, verbose = true)) //Adding dependent terms
  //   clusterSum.clear
  //   //println("\nExiting toDSStruct")
  //   sum

  // }

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
   def :+(r: Rational): DSAffineForm = {
    val fresh = DSAffineIndex.nextGlobal
    val ds = DSInterval(Interval(-rone, rone), rone)

    DSAffineForm(x0: Rational, noise :+ DSDeviation(r, fresh, Set(fresh), ds)) //TODO: please check this
   }

  // for compatibility with RangeArithmetic
  def +/-(r: Rational): DSAffineForm = this :+ r

  def unary_-(): DSAffineForm = {
    //println("\n Entering unary negation")

    var newTerms: Seq[DSDeviation] = Seq.empty
    var iter = noise.iterator
    while(iter.hasNext) {
      newTerms :+= - iter.next  // just flip the sign
    }
    val res = DSAffineForm(-x0, newTerms)
    //println(s"\n Printing the value after unary negation: $res")
    //println(s"\n Exiting unary negation")
    res
  }

  def +(y: DSAffineForm): DSAffineForm = {
    DSAffineForm(this.x0 + y.x0, addQueues(this.noise, y.noise))
  }

  def -(y: DSAffineForm): DSAffineForm = {
    //println("\nEntering DS Affine subtraction")
    val res = DSAffineForm(this.x0 - y.x0, subtractQueues(this.noise, y.noise))
    //println("\nExiting DS subtraction")
    res

  }


  def *(r: Rational): DSAffineForm = {
    //println("\n Entering rational multiplication")
    val res = DSAffineForm(r * x0, multiplyQueue(noise, r))
    //println("\n Exiting rational multiplication")
    res

  }

  def *(y: DSAffineForm): DSAffineForm = {
    //println("\n Entering affine multiplication")

    val firstVal = this
    //println(s"\n Printing the variables which are getting multiplied: $firstVal , $y")
    if ((!(y.noise).isEmpty) & (!(this.noise).isEmpty)) {

    // linear part, as before
      var newTerms: Seq[DSDeviation] = multiplyLinearPart(this.x0, this.noise, y.x0, y.noise)
      // non-linear part

      val (z0Addition, nonlinMgnt, nonlinDS) = multiplyQueuesOptimized(this.noise, y.noise)
      val nonlinDS_normalized = (nonlinDS * (rone / nonlinMgnt))

      // new central value
      val z0 = this.x0 * y.x0 + z0Addition



      if(nonlinMgnt != 0) {
        val freshIndex = DSAffineIndex.nextGlobal
        // the new term has unknown dependency with all other noise terms
        val freshDependency = Set(freshIndex) ++ this.allDependencies ++ y.allDependencies
        newTerms :+= DSDeviation(nonlinMgnt, freshIndex, freshDependency, nonlinDS_normalized)
      }
      //println(s"\n Number of dependent operations: $depOp; Number of inependent operations: $indepOp inside multiplication")

      val res = DSAffineForm(z0, newTerms)
      //println("\n Exiting multiplication")
      res


    }
    else if ((y.noise).isEmpty) this * y.x0
    else y * this.x0

  }

    //Computes the inverse of this DSAffineForm as a linear approximation.

  def inverse: DSAffineForm = {
    //println("\n Inside inverse")

    val (xlo, xhi) = (toInterval.xlo, toInterval.xhi)
    if (xlo <= rzero && xhi >= rzero)
      throw DivisionByZeroException("Possible division by zero: " + toString)
    if(noise.size == 0) { // exact
      val inv = rone/x0
      DSAffineForm(inv, Seq[DSDeviation]())
    } else {

      /* Calculate the inverse */
      val a = min(abs(xlo), abs(xhi))
      val b = max(abs(xlo), abs(xhi))

      val alpha = Rational(-1) / (b * b)

      val dmax = (rone / a) - (alpha * a)
      val dmin = (rone / b) - (alpha * b)

      var zeta = (dmin / two) + (dmax / two)
      if (xlo < rzero) zeta = -zeta
      val delta = max(zeta - dmin, dmax - zeta)

      val z0 = alpha * this.x0 + zeta

      var newTerms = multiplyQueue(noise, alpha)
      //val ds = this.distrib

      if(delta != rzero) {
        val freshIndex = DSAffineIndex.nextGlobal
      // the new term has unknown dependency with all other noise terms

        val inverseDependency = Set(freshIndex) ++ this.allDependencies
        val dsStructure = DSInterval(Interval(-rone, rone), rone)
        newTerms :+= DSDeviation(delta, freshIndex, inverseDependency, dsStructure)
      }
      DSAffineForm(z0, newTerms)
    }
  }

  /**
    Computes x/y as x * (1/y).
   */

  def /(y: DSAffineForm): DSAffineForm = {
    //this * y.inverse
    //println("\n Inside division")

    if ((y.noise).isEmpty) this * (Rational.fromReal(1.0) / y.x0)
    else this * y.inverse
  }

  def ^(_n: Int): DSAffineForm = ??? /*{
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

  def squareRoot: DSAffineForm = ???/*{
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
  def sine: DSAffineForm = ??? /*{
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

  def cosine: DSAffineForm = ??? /*{
    val conv = (DSAffineForm(Interval.pi) / DSAffineForm(Interval(2, 2))) - this
    conv.sine
  }*/

  def tangent: DSAffineForm = ??? /*{
    this.sine / this.cosine
  }*/

  /** Min-range based linear approximation of the exp() function
   */
  def exp: DSAffineForm = {
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

  def log: DSAffineForm = {
    var (a, b) = (toInterval.xlo, toInterval.xhi)

    if (a <= rzero) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }

    // Take slope of the right ending point of the interval (which is smaller),
    // probably results in better ranges.
    // Round it down to be sound for concave functions such as log.
    val alpha = rone / b
    val dmin = logDown(a) - (alpha * a)
    val dmax = logUp(b) - (alpha * b)

    val zeta = computeZeta(dmin, dmax)
    val delta = max(abs(dmin - zeta), abs(dmax - zeta))
    unaryOp(x0, noise, alpha, zeta, delta)
  }

  override def toString: String = "<[%s, %s], \n%s>".format(toInterval.xlo.toDouble, toInterval.xhi.toDouble, noise.mkString("\n"))
  def toSmallString: String = "[%.3f,%.3f]".format(toInterval.xlo.toDouble, toInterval.xhi.toDouble)
  def toBigString: String = "[%.18f,%.18f]".format(toInterval.xlo.toDouble, toInterval.xhi.toDouble)

  def longString: String =
    "%s +/- %s".format(x0.toString, noise.mkString(", "))

  def detailString: String = x0.toDouble + " +/- " + radius.toDouble

  // computes the linear portion of the multiplication
  private def multiplyLinearPart(a: Rational, xqueue: Seq[DSDeviation], b: Rational,
    yqueue: Seq[DSDeviation]): Seq[DSDeviation] = {
    //println("\nInside Linear part")
    var deviation = Seq[DSDeviation]()
    val iterX = xqueue.iterator
    val iterY = yqueue.iterator

    val fx = (dev: DSDeviation) => {
      val zi = dev * b
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    val fy = (dev: DSDeviation) => {
      val zi = dev * a
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    val fCouple = (xi: DSDeviation, yi: DSDeviation) => {
      val zi = xi * b + yi * a
      if (!zi.isZero) deviation :+= zi
      val x = 0
    }
    DoubleQueueIterator.iterate(iterX, iterY, dummyDev, fx, fy, fCouple)
    assert(!iterX.hasNext && !iterY.hasNext)
    deviation
  }



  // Does an optimized computation of the nonlinear part of multiplication
  private def multiplyQueuesOptimized(xqueue: Seq[DSDeviation], yqueue: Seq[DSDeviation]):
    (Rational, Rational, DSInterval) = {
    //println("\nInside NonLinear part")

    val indices = mergeIndices(getIndices(xqueue), getIndices(yqueue))

    val indepIndices = Set[Int]()  // TODO: find mutually independent indices

    // magnitude of nonlinear part
    var zqueue = rzero
    var z0Addition = rzero

    // DS of nonlinear part
    var indepDS_square = DSInterval(Interval(rzero), rone)
    var depDS_square = DSInterval(Interval(rzero), rone)
    var depDS = DSInterval(Interval(rzero), rone)

    var i = 0
    while (i < indices.length) {
      val iInd = indices(i)

      // square part magnitude
      val (xi, xiDev) = xqueue.find((d: DSDeviation) => d.index == iInd) match {
        case Some(d) => (d.mgnt, d); case None => (rzero, null) }
      val (yi, yiDev) = yqueue.find((d: DSDeviation) => d.index == iInd) match { //Getting None for y in case of division by rational converted to DSAffineForm
        case Some(d) => (d.mgnt, d); case None => (rzero, null) }
      val zii = xi * yi
      if (zii != 0) {
        z0Addition += abs(zii) / two
        zqueue += abs(zii / two)
      }

      // square part DS

      if (zii != rzero) {

        if (indepIndices.contains(i)) {
          assert(xiDev.distrib == yiDev.distrib)
        // val factor = zii
        // val squareDS = square(xi.distrib)
          indepDS_square = (indepDS_square i_+ (xiDev.distrib.square * zii))

        } else {

          assert(xiDev.distrib == yiDev.distrib)

          depDS_square = (depDS_square d_+ (xiDev.distrib.square * zii))
        }
      }

      var j = i + 1
      while (j < indices.length) {
        val jInd = indices(j)

        val (xj, xjDev) = xqueue.find((d: DSDeviation) => d.index == jInd) match {
          case Some(d) => (d.mgnt, d); case None => (rzero, null) }
        val (yj, yjDev) = yqueue.find((d: DSDeviation) => d.index == jInd) match { //Getting None for y in case of division by rational converted to DSAffineForm
        case Some(d) => (d.mgnt, d); case None => (rzero, null) }

        // non-square magnitude
        val zij = xi * yj + xj * yi
        if (zij != 0) zqueue += abs(zij)

        // non-square DS
        if (zij != 0) {
        //if (zij == 0) { // some j has to be there
          assert(xjDev == null || yjDev == null || xjDev.distrib == yjDev.distrib)


          val iDistrib = if (xiDev != null) xiDev.distrib else yiDev.distrib
          val jDistrib = if (xjDev != null) xjDev.distrib else yjDev.distrib

          depDS = depDS d_+ ((iDistrib d_* jDistrib) * zij) //it gives [0, 0] as interval if zij == 0
        }

        j += 1
      }
      i += 1
    }

    // TODO: check this, is the first addition dependent or independent?
    val finalDS = ((indepDS_square d_+ depDS_square) * Rational(1, 2)) d_+ depDS
    (z0Addition, zqueue, finalDS)
  }

   private def multiplyQueue(queue: Seq[DSDeviation], factor: Rational): Seq[DSDeviation] = {
     var deviation = Seq[DSDeviation]()
     val iter = queue.iterator
     while(iter.hasNext) {
       val xi = iter.next
       val zi = xi * factor
       if (!zi.isZero) deviation :+= zi
     }
     deviation
   }
   private def unaryOp(x0: Rational, noise: Seq[DSDeviation], alpha: Rational, zeta: Rational, delta: Rational): DSAffineForm = {

    val z0 = alpha * x0 + zeta
    var deviation = multiplyQueue(noise, alpha)
    val freshIndex = AffineIndex.nextGlobal
    val dep = Set(freshIndex) ++ this.allDependencies
        val dsStructure = DSInterval(Interval(-rone, rone), rone)

    if (delta != rzero) deviation :+= DSDeviation(delta, freshIndex, dep, dsStructure)
    DSAffineForm(z0, deviation)
  }

}
