// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Trees._
import lang.Identifiers._
import Rational.max
import FinitePrecision._

import scala.collection.immutable._

trait IntervalSubdivision extends RoundoffEvaluators {

  /**
   * Performs an interval subdivision, recording only the result's range
   * and error.
   *
   * The tree does not get annotated.
   * The initErrorMap only contains initial errors, NOT roundoffs.
   *
   * @param expr expression to be analysed
   * @param inputParams free variables of the expr
   * @param initValMap ranges of free variables
   * @param initErrorMap initial errors of free variables (if any)
   *        These do NOT include roundoff errors
   * @param trackRoundoffErrs whether to track roundoff errors or to only
   *        propagate initial errors (as provided)
   * @param uniformPrecision the default precision to use when doing absolute
   *                         errors calculations
   */
  def doIntervalSubdivision(
    expr: Expr,
    inputParams: Set[Identifier],
    initValMap: Map[Identifier, Interval],
    initErrorMap: Map[Identifier, Rational],
    // trackInitialErrs: Boolean,
    trackRoundoffErrs: Boolean,
    uniformPrecision: Precision): (Interval, Rational) = {

    // TODO: this should not be hardcoded
    val numSplits = 10

    val inputsSubdiv: Seq[Map[Identifier, Interval]] =  initValMap.foldLeft(Seq(Map[Identifier, Interval]()))({
      case (currSeq: Seq[Map[Identifier, Interval]], (id, intrvl)) =>
        val xlo = intrvl.xlo
        val splitWidth = intrvl.width / numSplits
        val splits: Seq[Interval] = (0 until numSplits).map(i =>
          Interval(xlo + i * splitWidth, xlo + (i + 1) * splitWidth))

        currSeq.flatMap(m =>
          splits.map(i => m + (id -> i))
        )
    })

    val allIDs = inputParams
    val missingIDs = allIDs -- initErrorMap.keySet

    // the initErrorMap will simply be empty, if initial errors are not being tracked
    def getInputErrors(rangeMap: Map[Identifier, Interval]): Map[Identifier, Rational] =
      if (trackRoundoffErrs){

        initErrorMap ++ missingIDs.map(id => (id -> uniformPrecision.absRoundoff(rangeMap(id))))

      } else {

        initErrorMap ++ missingIDs.map(id => (id -> Rational.zero))

      }


    val (resError, resInterval) = uniformRoundoff_IA_AA(expr,
      inputsSubdiv.head, getInputErrors(inputsSubdiv.head), uniformPrecision)
    // val (resInterval, resError) = evaluate[Interval, AffineForm](
    //   expr, inputsSubdiv.head,
    //   getInputErrors(inputsSubdiv.head).map(x => (x._1 -> AffineForm.fromError(x._2))),
    //   Interval.apply, AffineForm.zero, AffineForm.fromError, AffineForm.apply,
    //   false, trackRoundoffErrs, uniformPrecision)

    var maxInterval = resInterval
    var maxError = resError

    for (m <- inputsSubdiv.tail) {

      val (tmpError, tmpRange) = uniformRoundoff_IA_AA(expr, m,
        getInputErrors(m), uniformPrecision)

      // evaluate[Interval, AffineForm](
      //   expr, m,
      //   getInputErrors(m).map(x => (x._1 -> AffineForm.fromError(x._2))),
      //   Interval.apply, AffineForm.zero, AffineForm.fromError, AffineForm.apply,
      //   false, trackRoundoffErrs, uniformPrecision)

      maxInterval = maxInterval.union(tmpRange)
      maxError = max(maxError, tmpError)
    }
    (maxInterval, maxError)
  }


  /**
   * Uses interval subdivision to compute ranges and affine arithmetic for errors.
   *
   * Unlike the other subdivision method, this one annotates the trees with
   * information. For this, ranges are computed first with interval subdivision,
   * and errors are computed in a second step. Due to this separation, the computed
   * errors may not be the tightest possible.
   * This methods is suitable for fixed-point arithmetic.
   *
   * TODO: This needs to be updated to work without annotating trees.
   *
   * @param expr expression to be analysed
   * @param inputRanges initial ranges of free variables
   * @param varErrorMap input errors (incl. roundoff) of free variables
   * @param trackRoundoffErrs whether or not to track roundoff errors or to
   *        propagate only input errors (as given in varErrorMap)
   * @param uniformPrecision the default precision to use when doing absolute
   *                         errors calculations
   */
  /* def evaluateSubdiv(
    expr: Expr,
    inputRanges: Map[Identifier, Interval],
    varErrorMap: Map[Identifier, Rational],
    trackRoundoffErrs: Boolean,
    uniformPrecision: Precision): (Interval, Rational) = {

    @inline
    def computeNewError(range: Interval, propagatedError: AffineForm): AffineForm =
      if (trackRoundoffErrs) {
        val actualRange: Interval = range + propagatedError.toInterval
        val rndoff = uniformPrecision.absRoundoff(actualRange)
        propagatedError +/- rndoff
      } else {
        propagatedError
      }

    val numSplits = 10

    //val inputRanges: Map[Identifier, Interval] = context.inputRanges(fncId)

    val inputsSubdiv: Seq[Map[Identifier, Interval]] =  inputRanges.foldLeft(Seq(Map[Identifier, Interval]()))({
      case (currSeq: Seq[Map[Identifier, Interval]], (id, intrvl)) =>
        val xlo = intrvl.xlo
        val splitWidth = intrvl.width / numSplits
        val splits: Seq[Interval] = (0 until numSplits).map(i =>
          Interval(xlo + i * splitWidth, xlo + (i+1) * splitWidth))

        currSeq.flatMap( m =>
          splits.map(i => m + (id -> i))
        )
    })

    // map of maximum seen ranges of all intermediate expressions
    var currentRanges: Map[Expr, Interval] = Map()

    // there is probably a helper function for this
    def attachInterval(e: Expr): Unit = (e: @unchecked) match {

      case x @ Variable(id) =>
        if (!x.hasInterval) {
          x.interval = currentRanges(x)
        }
      return

      case x @ RealLiteral(r) =>
        x.interval = Interval(r)
        return

      case x @ Plus(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ Minus(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ Times(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ Division(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ UMinus(t) =>
        attachInterval(t)
        x.interval = currentRanges(x)

      case x @ Sqrt(t) =>
        attachInterval(t)
        x.interval = currentRanges(x)

      // TODO: we may be missing intervals on the let-defined vars
      case x @ Let(id, value, body) =>
        attachInterval(value)
        attachInterval(body)
        // the range of the let is the range of the final result returned
        x.interval = currentRanges(body)
        currentRanges = currentRanges + (x -> x.interval)
    }

    // assumes that the ranges are attached
    def computeAndAttachError(e: Expr, errorMap: Map[Identifier, AffineForm]): AffineForm = (e: @unchecked) match {
      case x @ Variable(id) =>
        val err = errorMap(id)
        x.absError = maxAbs(err.toInterval)
        err

      case x @ RealLiteral(r) =>
        val err = if (isExactInFloats(r, uniformPrecision)) {
          AffineForm.zero
        } else {
          AffineForm.fromError(uniformPrecision.absRoundoff(r))
        }
        x.absError = maxAbs(err.toInterval)
        err

      case x @ Plus(lhs, rhs) =>
        val lErr = computeAndAttachError(lhs, errorMap)
        val rErr = computeAndAttachError(rhs, errorMap)

        val propagatedError = lErr + rErr
        val newError = computeNewError(x.interval, propagatedError)

        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ Minus(lhs, rhs) =>
        val lErr = computeAndAttachError(lhs, errorMap)
        val rErr = computeAndAttachError(rhs, errorMap)

        val propagatedError = lErr + rErr
        val newError = computeNewError(x.interval, propagatedError)

        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ Times(lhs, rhs) =>
        // propagated error
        val lAA = AffineForm(lhs.asInstanceOf[NumAnnotation].interval)
        val rAA = AffineForm(rhs.asInstanceOf[NumAnnotation].interval)
        val lErr = computeAndAttachError(lhs, errorMap)
        val rErr = computeAndAttachError(rhs, errorMap)

        var propagatedError = lAA*rErr + rAA*lErr + lErr*rErr

        // new error
        val newError = computeNewError(x.interval, propagatedError)

        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ Division(lhs, rhs) =>
        // propagated error

        val rhsInterval = rhs.asInstanceOf[NumAnnotation].interval
        val rErr = computeAndAttachError(rhs, errorMap)  //yErr

        val rInt = rhsInterval + rErr.toInterval // the actual interval, incl errors
        val a = Interval.minAbs(rInt)
        val errorMultiplier = -one / (a*a)

        val invErr = rErr * AffineForm(errorMultiplier)
        val lAA = AffineForm(lhs.asInstanceOf[NumAnnotation].interval)
        val inverse: Interval = rhsInterval.inverse
        val kAA = AffineForm(inverse)

        val lErr = computeAndAttachError(lhs, errorMap)  //xErr

        // multiplication with inverse
        var propagatedError = lAA*invErr + kAA*lErr + lErr*invErr

        // new error
        val newError = computeNewError(x.interval, propagatedError)

        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ UMinus(t) =>
        val newError = - computeAndAttachError(t, errorMap)   // no additional roundoff error
        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ Sqrt(t) =>

        // TODO: Not supported for fixed-points, add exception
        //case FPPrecision(_) => throw UnsupportedRealFragmentException("Sqrt not supported for fixed-points.")

        val tInterval = t.asInstanceOf[NumAnnotation].interval
        val tError = computeAndAttachError(t, errorMap)

        // propagated existing errors
        val a = Interval.minAbs(tInterval)
        val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)
        val propagatedError = tError * AffineForm(errorMultiplier)

        // new roundoff
        val newError = computeNewError(x.interval, propagatedError)

        x.absError = maxAbs(newError.toInterval)
        newError


      case Let(id, value, body) =>
        val aform = computeAndAttachError(value, errorMap)
        val newErrorMap = errorMap + (id -> aform)
        computeAndAttachError(body, newErrorMap)
    }

    // collect possible intervals
    // first intervals need to populate the map
    evalInterval(expr, inputsSubdiv.head,
      (x: Expr, i: Interval) => currentRanges = currentRanges + (x -> i))

    // the next intervals update the map
    inputsSubdiv.tail.foreach( subdiv =>
      evalInterval(expr, subdiv,
        (x: Expr, i: Interval) => currentRanges = currentRanges + (x -> currentRanges(x).union(i))))

    currentRanges = currentRanges ++ inputRanges.map(x => (Variable(x._1) -> x._2))

    // traverse the tree once more and attach the correct ranges
    attachInterval(expr)

    // Now that we have the ranges, we can compute the errors
    val resError = computeAndAttachError(expr, varErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))))

    (currentRanges(expr), maxAbs(resError.toInterval))
  } */

}
