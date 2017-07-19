
package daisy
package utils

import scala.collection.immutable.Seq

import lang.Trees._
import lang.TreeOps.isBooleanTerm
import lang.Constructors._
import lang.Identifiers._
import lang.Types.RealType
import Rational._
import solvers.Z3Solver



object SMTRange {

  def toConstraints(v: Variable, i: Interval): Set[Expr] = {
    Set(
      LessEquals(RealLiteral(i.xlo), v), LessEquals(v, RealLiteral(i.xhi))
      )
  }

  def apply(v: Variable, i: Interval): SMTRange = {
    // no tightening necessary here as there are no additional constraints
    SMTRange(i, v, toConstraints(v, i))

  }

  def apply(r: Rational): SMTRange = {
    // no tightening necessary here as there are no additional constraints
    SMTRange(Interval(r, r), RealLiteral(r), Set[Expr]())

  }

  def apply(v: Variable, i: Interval, precondition: Expr): SMTRange = {
    val constrs: Set[Expr] = precondition match {
      case And(exprs) => exprs.toSet
      case x if isBooleanTerm(x) => Set(x)
      case _ =>
        System.err.println("Calling SMTRange with a non-term constraint." +
          "It will be ignored.")
        Set[Expr]()
    }

    val allConstraints = constrs ++ toConstraints(v, i)
    val tightRange = tightenBounds(i, v, allConstraints)
    SMTRange(tightRange, v, allConstraints)
  }

  // This needs to be populated before we can use SMTRange
  // It is needed for the reporter inside the solver.
  // This is not an ideal solution, but I don't want every SMTRange
  // to carry this around.
  // Perhaps this can be also done with implicits?
  //var context: Context = null


  // TODO: this separate reporter is suboptimal, does it even know
  // about the debug sections?
  // what happens if we have to reporter sticking around?
  //val context = Context(new DefaultReporter(Set()), Seq(), Seq())
  var reporter = new DefaultReporter(Set())
  implicit val debugSection = DebugSectionSMTRange

  val checkTightness = false

  /**
    Computes a tight range of t, given constraints constrs from the starting
    interval i. Note that i is only used as a strating point, and is NOT
    converted to constraint; you have to do this yourself before.
    Will default to the given bounds in i, if necessary.

    @param i safe interval within which to search for tight bounds
    @param t arithmetic expression to be evaluated
    @param constrs constraints on t
   */
  def tightenBounds(i: Interval, t: Expr, constrs: Set[Expr]): Interval = {

    // TODO: massage arithmetic

    if (lang.TreeOps.size(t) == 1) {
      reporter.warning("Calling tightenRange on simple expression: " + t)
    } else {
      //print(lang.TreeOps.size(t) + " ")
    }

    val condition = and(constrs.toSeq: _*)
    val precisionThreshold = Rational.fromReal(0.01)
    val loopThreshold = 10

    def findLowerBound(lo: Rational, hi: Rational, loopCount: Int): Rational = {

      if (hi - lo <= precisionThreshold || loopCount >= loopThreshold) {
        lo
      } else {

        val mid = lo + (hi - lo) / two

        val solverQuery = And(condition, LessThan(t, RealLiteral(mid)))
        reporter.debug(s"\nloop count: $loopCount, [$lo, $hi], mid: ${mid}")
        reporter.debug(s"solverQuery: $solverQuery")

        // TODO: make this less inefficient, we are creating a new solver each time.
        val res = Z3Solver.checkSat(solverQuery)

        res match {
          case Some(false) =>  findLowerBound(mid, hi, loopCount + 1) //UNSAT
          case Some(true) =>   findLowerBound(lo, mid, loopCount + 1) //SAT
          case _ =>
            Z3Solver.unknownCounter += 1
            lo       //timeout, return save option
        }

      }
    }

    def findUpperBound(lo: Rational, hi: Rational, loopCount: Int): Rational = {

      if (hi - lo <= precisionThreshold || loopCount >= loopThreshold) {
        hi
      } else {

        val mid = lo + (hi - lo) / two

        val solverQuery = And(condition, LessThan(RealLiteral(mid), t))
        reporter.debug(s"\nloop count: $loopCount, [$lo, $hi], mid: ${mid}")
        reporter.debug(s"solverQuery: $solverQuery")

        // TODO: can we re-use the solver?
        val res = Z3Solver.checkSat(solverQuery)
        res match {
          case Some(false) =>  findUpperBound(lo, mid, loopCount + 1) //UNSAT
          case Some(true) =>   findUpperBound(mid, hi, loopCount + 1) //SAT
          case _ =>
            //reporter.warning("Z3 returns unknown")
            Z3Solver.unknownCounter += 1
            hi       //timeout, return save option
        }

      }
    }

    /* Rationale:
      if expr < lowerBound + prec is SAT, it means that if we only slightly
      tighten the bound, then it is already SAT, and hence the bound is already tight.
    */
    def lowerBoundIsTight: Boolean = {
      val solverQuery = And(condition, LessThan(t, RealLiteral(i.xlo + precisionThreshold)))
      val res = Z3Solver.checkSat(solverQuery)
      res match {
        case Some(true) => true  // SAT
        case Some(false) => false          // UNSAT
        case _ =>   // timeout or unknown probably
          //reporter.warning("Z3 returns unknown")
            Z3Solver.unknownCounter += 1
          false
      }
    }

    def upperBoundIsTight: Boolean = {
      val solverQuery = And(condition, GreaterThan(t, RealLiteral(i.xhi - precisionThreshold)))
      val res = Z3Solver.checkSat(solverQuery)
      res match {
        case Some(true) => true  // SAT
        case Some(false) => false          // UNSAT and timeout
        case _ => // timeout
          //reporter.warning("Z3 returns unknown")
            Z3Solver.unknownCounter += 1
          false
      }
    }

    reporter.debug(s"Going to find lower bound starting from [${i.xlo}, ${i.xhi}].")

    // it reduces the # of calls to Z3, but not the overall time
    // TODO: it seems that these calls are expensive, figure out why
    val lowerBound = if (checkTightness && lowerBoundIsTight) {
        reporter.info("lower bound is tight")
        i.xlo
      } else {
        findLowerBound(i.xlo, i.xhi, 0)
      }

    //val lowerBound = findLowerBound(i.xlo, i.xhi, 0)

    reporter.debug(s"Going to find upper bound starting from [${i.xlo}, ${i.xhi}].")

    val upperBound = if (checkTightness && upperBoundIsTight) {
        reporter.info("upper bound is tight")
        i.xhi
      } else {
        findUpperBound(i.xlo, i.xhi, 0)
      }


    //val upperBound = findUpperBound(i.xlo, i.xhi, 0)


    Interval(lowerBound, upperBound)
  }
}

/**
  A range representation whose `interval` value is tightened with
  the help of an SMT solver, taking into account correlations from
  the `tree` and possible additional `constraints`.

  The interval should always be the tightened interval, hence the constructor
  is private so that we can control this. Since we probably need all the tight
  intervals, a lazy approach won't gain us much.

  // TODO: consider not calling tightenBounds, if Z3 has timed out before,
  i.e. this information has to be tracked

  @param interval the tight(!) interval of the tree
  @param tree computation history, i.e. arithmetic expression
  @param constraints additional constraints on the range
 */
case class SMTRange private(interval: Interval, tree: Expr, constraints: Set[Expr]) extends RangeArithmetic[SMTRange] {
  import SMTRange._

  assert(constraints.forall(isBooleanTerm(_)))

  def toInterval: Interval = {
    interval  // is already tight
  }

  def +/-(r: Rational): SMTRange = ???

  def unary_-(): SMTRange = {
    // negation does not affect tightness
    SMTRange(-interval, UMinus(tree), constraints)
  }

  def +(y: SMTRange): SMTRange = {

    val newTree = Plus(this.tree, y.tree)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval + y.interval, newTree, newConstraints)

    SMTRange(newInterval, newTree, newConstraints)

  }

  def -(y: SMTRange): SMTRange = {

    val newTree = Minus(this.tree, y.tree)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval - y.interval, newTree, newConstraints)
    SMTRange(newInterval, newTree, newConstraints)
  }

  def *(y: SMTRange): SMTRange = {
    val newTree = Times(this.tree, y.tree)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval * y.interval, newTree, newConstraints)
    SMTRange(newInterval, newTree, newConstraints)
  }

  def *(r: Rational): SMTRange = {
    ???
  }

  def /(y: SMTRange): SMTRange = {
    val newTree = Division(this.tree, y.tree)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval / y. interval, newTree, newConstraints)
    SMTRange(newInterval, newTree, newConstraints)
  }

  def inverse: SMTRange = {
    val newTree = Division(RealLiteral(one), this.tree)
    //val newConstraints = this.constraints
    // no tightening here, add if needed...
    val newInterval = Interval(Rational.one) / this.interval
    SMTRange(newInterval, newTree, this.constraints)
  }

  def squareRoot: SMTRange = {

    val newTree = Variable(FreshIdentifier("#sqrt", RealType, true))  //always shows unique ID
    val newCondition: Set[Expr] = Set(Equals(Times(newTree, newTree), this.tree), LessEquals(RealLiteral(zero), newTree))
    val newConstraints = mergeConstraints(this.constraints, newCondition)
    val newInterval = tightenBounds(this.interval.squareRoot, newTree, newConstraints)
    SMTRange(newInterval, newTree, newConstraints)
  }

  // function for now, later may be inlined
  @inline
  private def mergeConstraints(set1: Set[Expr], set2: Set[Expr]): Set[Expr] = {

    set1 ++ set2

  }
}