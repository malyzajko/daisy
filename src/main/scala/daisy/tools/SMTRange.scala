// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Trees._
import lang.TreeOps.isBooleanTerm
import lang.Constructors._
import lang.Identifiers._
import lang.Types.RealType
import Rational._
import solvers.Solver



object SMTRange {

  val lowPrecision = Rational.fromReal(0.01)
  val lowLoopThreshold = 20
  val highPrecision = Rational.fromReal(0.00000000000000000001)
  val highLoopThreshold = 100
  implicit val debugSection = DebugSectionTools

  def toConstraints(v: Variable, i: Interval): Set[Expr] = {
    Set(
      LessEquals(RealLiteral(i.xlo), v), LessEquals(v, RealLiteral(i.xhi))
      )
  }

  def apply(r: Rational, precondition: Expr): SMTRange = {
    // no tightening necessary here as the bounds are already tight
    SMTRange(Interval(r, r), RealLiteral(r), precondition, Set[Expr]())
  }

  def apply(i: Interval, precondition: Expr): SMTRange = {
    // to make the range evaluation work in presence of if-else stmts
    SMTRange(Variable(FreshIdentifier("tmp" + System.currentTimeMillis, RealType)), i, precondition)
  }

  def apply(v: Variable, i: Interval, precondition: Expr): SMTRange = {
    val tightRange = tightenBounds(i, v, precondition, Set.empty, lowPrecision, lowLoopThreshold)
    SMTRange(tightRange, v, precondition, Set.empty)
  }

  /**
    The constraints are not used to generate a precondition. Instead, the
    trivial precondition 'true' is used.
    This constructor is used in [[daisy.analysis.RelativeErrorPhase.evaluateSMTComplete]].
   */
  def apply(v: Expr, i: Interval, constraints: Set[Expr]): SMTRange = {
    val extra: Set[Expr] = Set(
      LessEquals(RealLiteral(i.xlo), v), LessEquals(v, RealLiteral(i.xhi))
    )

    reporter.debug("evaluate complete expression")
    val allConstraints = constraints ++ extra
    val tightRange = tightenBounds(i, v, BooleanLiteral(true), allConstraints, highPrecision, highLoopThreshold)
    SMTRange(tightRange, v, BooleanLiteral(true), allConstraints)
  }

  // This needs to be populated before we can use SMTRange
  // It is needed for the reporter inside the solver.
  // This is not an ideal solution, but I don't want every SMTRange
  // to carry this around.
  // Perhaps this can be also done with implicits?
  // var context: Context = null


  // TODO: this separate reporter is suboptimal, does it even know
  // about the debug sections?
  // what happens if we have to reporter sticking around?
  // val context = Context(new DefaultReporter(Set()), Seq(), Seq())
  var reporter = new DefaultReporter(Set())

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
  def tightenBounds(i: Interval, t: Expr, precond: Expr, constrs: Set[Expr], precisionThreshold: Rational,
    loopThreshold: Int, checkVars: Boolean = false): Interval = {
    //    reporter.warning(s"Tighten the bounds on $t")
    // TODO: massage arithmetic

    val constrsSeq = precond +: constrs.toSeq
    // reporter.info(s"tightening with $constrsSeq of $t")
    val condition = and(constrsSeq: _*)
    // val precisionThreshold = Rational.fromReal(0.01)
    // val loopThreshold = 10

    def findLowerBound(lo: Rational, hi: Rational, loopCount: Int): Rational = {

      if (hi - lo <= precisionThreshold || loopCount >= loopThreshold) {
        lo
      } else {

        var mid = lo + (hi - lo) / two

        // Make numerator and denominator smaller to avoid unsound dReal results.
        // It should not matter in which direction to round here.
        try {
          mid = scaleToLongUp(mid)
        } catch {
          case e: RationalCannotBeCastToIntException =>
            reporter.warning("Cannot reasonably shorten fraction, resorting to sound lower bound")
            return lo
        }

        val solverQuery = And(condition, LessThan(t, RealLiteral(mid)))
        reporter.debug(s"\nloop count: $loopCount, [$lo, $hi], mid: ${mid}")
        reporter.debug(s"solverQuery: $solverQuery")

        // TODO: make this less inefficient, we are creating a new solver each time.
        val res = Solver.checkSat(solverQuery)
        //        reporter.warning(s"Result for the query is $res")
        res match {
          case Some(false) =>  findLowerBound(mid, hi, loopCount + 1) // UNSAT
          case Some(true) =>   findLowerBound(lo, mid, loopCount + 1) // SAT
          case _ =>
            Solver.unknownCounter += 1
            reporter.debug("Solver returns unknown. TIMEOUT")
            lo       // timeout, return save option
        }

      }
    }

    def findUpperBound(lo: Rational, hi: Rational, loopCount: Int): Rational = {

      if (hi - lo <= precisionThreshold || loopCount >= loopThreshold) {
        hi
      } else {

        var mid = lo + (hi - lo) / two

        // Make numerator and denominator smaller to avoid unsound dReal results.
        // It should not matter in which direction to round here.
        try {
          mid = scaleToLongUp(mid)
        } catch {
          case e: RationalCannotBeCastToIntException =>
            reporter.warning("Cannot reasonably shorten fraction, resorting to sound lower bound")
            return hi
        }

        val solverQuery = And(condition, LessThan(RealLiteral(mid), t))
        reporter.debug(s"\nloop count: $loopCount, [$lo, $hi], mid: ${mid}")
        reporter.debug(s"solverQuery: $solverQuery")

        val res = Solver.checkSat(solverQuery)
        res match {
          case Some(false) =>  findUpperBound(lo, mid, loopCount + 1) // UNSAT
          case Some(true) =>   findUpperBound(mid, hi, loopCount + 1) // SAT
          case _ =>
            Solver.unknownCounter += 1
            reporter.debug("Solver returns unknown. TIMEOUT")
            hi       // timeout, return save option
        }

      }
    }

    /* Rationale:
      if expr < lowerBound + prec is SAT, it means that if we only slightly
      tighten the bound, then it is already SAT, and hence the bound is already tight.
     */
    def lowerBoundIsTight: Boolean = {
      val solverQuery = And(condition, LessThan(t, RealLiteral(i.xlo + precisionThreshold)))
      val res = Solver.checkSat(solverQuery)
      res match {
        case Some(true) => true  // SAT
        case Some(false) => false          // UNSAT
        case _ =>   // timeout or unknown probably
          Solver.unknownCounter += 1
          reporter.debug("Solver returns unknown. TIMEOUT")
          false
      }
    }

    def upperBoundIsTight: Boolean = {
      val solverQuery = And(condition, GreaterThan(t, RealLiteral(i.xhi - precisionThreshold)))
      val res = Solver.checkSat(solverQuery)
      res match {
        case Some(true) => true  // SAT
        case Some(false) => false          // UNSAT and timeout
        case _ => // timeout
          Solver.unknownCounter += 1
          reporter.debug("Solver returns unknown. TIMEOUT")
          false
      }
    }



    if (lang.TreeOps.size(t) == 1 && !checkVars) {
      //reporter.warning("Calling tightenRange on simple expression: " + t)
      // TODO what to do?
      i
    } else {
      // print(lang.TreeOps.size(t) + " ")


      reporter.debug(s"Going to find lower bound starting from [${i.xlo}, ${i.xhi}].")

      // it reduces the # of calls to Z3, but not the overall time
      // TODO: it seems that these calls are expensive, figure out why
      val lowerBound = if (checkTightness && lowerBoundIsTight) {
        reporter.info("lower bound is tight")
        i.xlo
      } else {
        findLowerBound(i.xlo, i.xhi, 0)
      }

      // val lowerBound = findLowerBound(i.xlo, i.xhi, 0)

      reporter.debug(s"Going to find upper bound starting from [${i.xlo}, ${i.xhi}].")

      val upperBound = if (checkTightness && upperBoundIsTight) {
        reporter.info("upper bound is tight")
        i.xhi
      } else {
        findUpperBound(i.xlo, i.xhi, 0)
      }


      // val upperBound = findUpperBound(i.xlo, i.xhi, 0)


      Interval(lowerBound, upperBound)
    }

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
  @param precond the precondition of the function enclosing the expression
  @param constraints additional constraints on the range, e.g. if-conditions
 */
case class SMTRange private(interval: Interval, tree: Expr, precond: Expr, constraints: Set[Expr])
  extends RangeArithmetic[SMTRange] {
  import SMTRange._

  assert(constraints.forall(isBooleanTerm(_)))

  def toInterval: Interval = {
    interval  // is already tight
  }

  def +/-(r: Rational): SMTRange = ???

  def addConstraint(e: Set[Expr]): SMTRange = {
    SMTRange(interval, tree, precond, constraints ++ e)
  }

  def unary_- = {
    // negation does not affect tightness
    SMTRange(-this.interval, UMinus(tree), this.precond, this.constraints)
  }

  // needed for RangeArithmetic trait. apparently the members with parameters are not recognized
  def +(y: SMTRange): SMTRange = this.+(y, lowPrecision, lowLoopThreshold)
  def -(y: SMTRange): SMTRange = this.-(y, lowPrecision, lowLoopThreshold)
  def *(y: SMTRange): SMTRange = this.*(y, lowPrecision, lowLoopThreshold)
  def /(y: SMTRange): SMTRange = this./(y, lowPrecision, lowLoopThreshold)
//  def ^(n: SMTRange): SMTRange = this ^ (n, lowPrecision, lowLoopThreshold)
  def ^(n: Int): SMTRange = this ^ (n, lowPrecision, lowLoopThreshold)
  def squareRoot: SMTRange = this.squareRoot(lowPrecision, lowLoopThreshold)
  def sine: SMTRange = this.sine(lowPrecision, lowLoopThreshold)
  def cosine: SMTRange = this.cosine(lowPrecision, lowLoopThreshold)
  def tangent: SMTRange = this.tangent(lowPrecision, lowLoopThreshold)
  def arcsine: SMTRange = this.arcsine(lowPrecision, lowLoopThreshold)
  def arccosine: SMTRange = this.arccosine(lowPrecision, lowLoopThreshold)
  def arctangent: SMTRange = this.arctangent(lowPrecision, lowLoopThreshold)
  def exp: SMTRange = this.exp(lowPrecision, lowLoopThreshold)
  def log: SMTRange = this.log(lowPrecision, lowLoopThreshold)


  def +(y: SMTRange, precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {

    val newTree = Plus(this.tree, y.tree)
    val newPrecond = mergePrecond(this.precond, y.precond)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval + y.interval, newTree, newPrecond, newConstraints,
      precision, maxLoops)

    SMTRange(newInterval, newTree, newPrecond, newConstraints)
  }

  def -(y: SMTRange, precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {

    val newTree = Minus(this.tree, y.tree)
    val newPrecond = mergePrecond(this.precond, y.precond)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval - y.interval, newTree, newPrecond, newConstraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, newPrecond, newConstraints)
  }

  def *(y: SMTRange, precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Times(this.tree, y.tree)
    val newPrecond = mergePrecond(this.precond, y.precond)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval * y.interval, newTree, newPrecond, newConstraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, newPrecond, newConstraints)
  }

  def *(r: Rational): SMTRange = {
    ???
  }

//  // todo fix Pow: check if it is correct
//  def ^(n: SMTRange, precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
//    val newTree = Pow(this.tree, n.tree)
//    val newConstraints = mergeConstraints(this.constraints, n.constraints)
//    val newInterval = tightenBounds(this.interval ^ n. interval, newTree, newConstraints,
//      precision, maxLoops)
//    SMTRange(newInterval, newTree, newConstraints)
//  }

  def ^(n: Int, precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = IntPow(this.tree, n)
    val newInterval = tightenBounds(this.interval ^ n, newTree, precond, constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, precond, constraints)
  }

  def /(y: SMTRange, precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Division(this.tree, y.tree)
    val newPrecond = mergePrecond(this.precond, y.precond)
    val newConstraints = mergeConstraints(this.constraints, y.constraints)
    val newInterval = tightenBounds(this.interval / y. interval, newTree, newPrecond, newConstraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, newPrecond, newConstraints)
  }

  def inverse: SMTRange = {
    val newTree = Division(RealLiteral(one), this.tree)
    // val newConstraints = this.constraints
    // no tightening here, add if needed...
    val newInterval = Interval(Rational.one) / this.interval
    SMTRange(newInterval, newTree, precond, this.constraints)
  }

  def squareRoot(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {

    val newTree = Variable(FreshIdentifier("#sqrt", RealType, true))  // always shows unique ID
    val newCondition: Set[Expr] = Set(Equals(Times(newTree, newTree), this.tree),
      LessEquals(RealLiteral(zero), newTree))
    val newConstraints = mergeConstraints(this.constraints, newCondition)
    val newInterval = tightenBounds(this.interval.squareRoot, newTree, precond, newConstraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, precond, newConstraints)
  }

  def sine(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Sin(this.tree)
    val newInterval = tightenBounds(this.interval.sine, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  def cosine(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Cos(this.tree)
    val newInterval = tightenBounds(this.interval.cosine, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  def tangent(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Tan(this.tree)
    val newInterval = tightenBounds(this.interval.tangent, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  def arcsine(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Asin(this.tree)
    val newInterval = tightenBounds(this.interval.arcsine, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  def arccosine(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Acos(this.tree)
    val newInterval = tightenBounds(this.interval.arccosine, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  def arctangent(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Atan(this.tree)
    val newInterval = tightenBounds(this.interval.arctangent, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  def exp(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Exp(this.tree)
    val newInterval = tightenBounds(this.interval.exp, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  def log(precision: Rational = lowPrecision, maxLoops: Int = lowLoopThreshold): SMTRange = {
    val newTree = Log(this.tree)
    val newInterval = tightenBounds(this.interval.log, newTree, this.precond, this.constraints,
      precision, maxLoops)
    SMTRange(newInterval, newTree, this.precond, this.constraints)
  }

  // function for now, later may be inlined
  @inline
  private def mergeConstraints(set1: Set[Expr], set2: Set[Expr]): Set[Expr] = {

    set1 ++ set2

  }

  @inline
  private def mergePrecond(precond1: Expr, precond2: Expr): Expr = {
    if (precond1.equals(precond2)) {
      precond1
    } else {
      reporter.warning(s"2 unequal preconditions: $precond1 and $precond2")
      and(precond1, precond2)
    }
  }
}
