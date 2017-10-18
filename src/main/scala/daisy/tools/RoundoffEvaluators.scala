// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Trees._
import lang.Identifiers._
import FinitePrecision._
import Rational._
import daisy.utils.CachingMap

trait RoundoffEvaluators extends RangeEvaluators {

  /**
   * Calculates the roundoff error for a given uniform precision
   * using interval arithmetic for ranges and affine arithmetic for errors.
   *
   * @param expr expression for which to compute roundoff
   * @param inputValMap real-valued ranges of all input variables
   * @param inputErrorMap errors of all input variables (incl. roundoff)
   * @param uniformPrecision precision for the entire computation
   *
   * @return (max. absolute roundoff error bound, real-valued result interval)
   */
  def uniformRoundoff_IA_AA(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision,
    trackRoundoffErrors: Boolean = true,
    approxRoundoff: Boolean = false): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[Interval](expr, inputValMap, Interval.apply)

    val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr, intermediateRanges,
      Map.empty.withDefaultValue(uniformPrecision),
      inputErrorMap.mapValues(AffineForm.+/-),
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors,
      approxRoundoff)

    (Interval.maxAbs(resRoundoff.toInterval), resRange)
  }

  /**
   * Calculates the roundoff error for a given uniform precision
   * using affine arithmetic for ranges and affine arithmetic for errors.
   *
   * @param expr expression for which to compute roundoff
   * @param inputValMap real-valued ranges of all input variables
   * @param inputErrorMap errors of all input variables (incl. roundoff)
   * @param uniformPrecision precision for the entire computation
   */
  def uniformRoundoff_AA_AA(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision,
    trackRoundoffErrors: Boolean = true,
    approxRoundoff: Boolean = false): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[AffineForm](expr,
      inputValMap.mapValues(AffineForm(_)), AffineForm.apply)

    val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr,
      intermediateRanges.mapValues(_.toInterval),
      Map.empty.withDefaultValue(uniformPrecision),
      inputErrorMap.mapValues(AffineForm.+/-),
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors,
      approxRoundoff)

    (Interval.maxAbs(resRoundoff.toInterval), resRange.toInterval)
  }

  /**
   * Calculates the roundoff error for a given uniform precision
   * using SMTRange for ranges and affine arithmetic for errors.
   *
   * @param expr expression for which to compute roundoff
   * @param inputValMap real-valued ranges of all input variables
   * @param inputErrorMap errors of all input variables (incl. roundoff)
   * @param uniformPrecision precision for the entire computation
   */
  def uniformRoundoff_SMT_AA(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision,
    trackRoundoffErrors: Boolean = true,
    approxRoundoff: Boolean = false): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[SMTRange](expr,
      inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) }),
      SMTRange.apply)

    val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr,
      intermediateRanges.mapValues(_.toInterval),
      Map.empty.withDefaultValue(uniformPrecision),
      inputErrorMap.mapValues(AffineForm.+/-),
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors,
      approxRoundoff)

    (Interval.maxAbs(resRoundoff.toInterval), resRange.toInterval)
  }

  /**
    Computes the absolute roundoff error for the given expression.

    The ranges of all the intermediate expressions have to be given in rangeMap.
    Allows mixed-precision by providing (possibly different) precisions for
    all declared variables (input parameters as well as locally defined variables.)
    Constants are assumed to be all in one precision, given by the user.

   */
  def evalRoundoff[T <: RangeArithmetic[T]](
    expr: Expr,
    range: Map[Expr, Interval],
    precision: Map[Identifier, Precision],
    freeVarsError: Map[Identifier, T],
    zeroError: T,
    fromError: Rational => T,
    interval2T: Interval => T,
    constantsPrecision: Precision,
    trackRoundoffErrors: Boolean, // if false, propagate only initial errors
    approxRoundoff: Boolean = false
    ): (T, Map[Expr, T]) = {


    val intermediateErrors = new CachingMap[Expr, (T, Precision)]

    for ((id, err) <- freeVarsError){
      intermediateErrors.put(Variable(id), (err, precision(id)))
    }

    // TODO: check the effectiveness of this
    // @inline
    def computeNewError(range: Interval, propagatedError: T, prec: Precision): (T, Precision) =
      if (trackRoundoffErrors) {
        val actualRange: Interval = range + propagatedError.toInterval
        var rndoff = prec.absRoundoff(actualRange)
        if (approxRoundoff) {
          rndoff = Rational.limitSize(rndoff)
        }
        (propagatedError +/- rndoff, prec)
      } else {
        (propagatedError, prec)
      }

    def computeNewErrorTranscendental(range: Interval, propagatedError: T, prec: Precision): (T, Precision) =
      if (trackRoundoffErrors) {
        val actualRange: Interval = range + propagatedError.toInterval
        var rndoff = prec.absTranscendentalRoundoff(actualRange)
        if (approxRoundoff) {
          rndoff = Rational.limitSize(rndoff)
        }
        (propagatedError +/- rndoff, prec)
      } else {
        (propagatedError, prec)
      }

    def eval(e: Expr): (T, Precision) = intermediateErrors.getOrAdd(e, {

      case x @ RealLiteral(r) =>
        val error = if (constantsPrecision.canRepresent(r) || !trackRoundoffErrors) {
          zeroError
        } else {
          fromError(constantsPrecision.absRoundoff(r))
        }
        (error, constantsPrecision)

      case x @ Plus(lhs, rhs) =>
        val (errorLhs, precLhs) = eval(lhs)
        val (errorRhs, precRhs) = eval(rhs)

        val propagatedError = errorLhs + errorRhs

        computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs)  /* Scala semantics */)

      case x @ Minus(lhs, rhs) =>
        val (errorLhs, precLhs) = eval(lhs)
        val (errorRhs, precRhs) = eval(rhs)

        val propagatedError = errorLhs - errorRhs

        computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs))

      case x @ Times(lhs, rhs) =>
        val (errorLhs, precLhs) = eval(lhs)
        val (errorRhs, precRhs) = eval(rhs)

        val rangeLhs = interval2T(range(lhs))
        val rangeRhs = interval2T(range(rhs))

        val propagatedError =
          rangeLhs * errorRhs +
          rangeRhs * errorLhs +
          errorLhs * errorRhs

        computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs))

      case x @ FMA(fac1, fac2, sum) =>
        val (errorFac1, precFac1) = eval(fac1)
        val (errorFac2, precFac2) = eval(fac2)
        val (errorSum, precSum) = eval(sum)

        val rangeFac1 = interval2T(range(fac1))
        val rangeFac2 = interval2T(range(fac2))
        val rangeSum = interval2T(range(sum))

        val propagatedError =
          rangeFac1 * errorFac2 +
          rangeFac2 * errorFac1 +
          errorFac1 * errorFac2 +
          errorSum

        computeNewError(range(x), propagatedError, getUpperBound(precFac1, precFac2, precSum))

      case x @ Division(lhs, rhs) =>
        val (errorLhs, precLhs) = eval(lhs)
        val (errorRhs, precRhs) = eval(rhs)

        val rangeLhs = range(lhs)
        val rangeRhs = range(rhs)

        // inverse, i.e. we are computing x * (1/y)
        val rightInterval = rangeRhs + errorRhs.toInterval // the actual interval, incl errors

        // the actual error interval can now contain 0, check this
        if (rightInterval.includes(Rational.zero)) {
          throw DivisionByZeroException("trying to divide by error interval containing 0")
        }
        val a = Interval.minAbs(rightInterval)
        val errorMultiplier: Rational = -one / (a*a)
        val invErr = errorRhs * errorMultiplier

        // error propagation
        val inverse: Interval = rangeRhs.inverse

        var propagatedError =
          interval2T(rangeLhs) * invErr +
          interval2T(inverse) * errorLhs +
          errorLhs * invErr

        computeNewError(range(x), propagatedError, getUpperBound(precLhs, precRhs))

      case x @ UMinus(t) =>
        val (error, prec) = eval(t)
        (- error, prec)

      case x @ Sqrt(t) =>
        // TODO: needs to fail for fixed-point precision
        val (errorT, prec) = eval(t)
        val rangeT = range(t)

        if ((errorT.toInterval.xlo + rangeT.xlo) < Rational.zero) {
          throw DivisionByZeroException("trying to take the square root of a negative number")
        }

        val a = Interval.minAbs(rangeT)
        val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)

        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        computeNewError(range(x), propagatedError, prec)

      case x @ Sin(t) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t)

        // Bound the slope of sin(x) over the range by computing its
        // derivative (i.e. cos(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv =  range(t).cosine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ Cos(t) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t)

        // Bound the slope of cos(x) over the range by computing its
        // derivative (i.e. -sin(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv = -range(t).sine
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ Tan(t) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t)

        // compute the derivative as 1/cos^2(x)
        val intCosine = range(t).cosine
        val deriv = (intCosine * intCosine).inverse

        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ Exp(t) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t)

        // maximal slope is always at the right ending point
        val b = range(t).xhi

        // compute the maximal slope over the interval
        // (exp(x) is the derivative of exp(x))
        val errorMultiplier = expUp(b)

        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ Log(t) =>
        // TODO not supported for fixed-points
        val (errorT, prec) = eval(t)

        // maximal slope is always at the left ending point
        val a = range(t).xlo

        // compute the maximal slope over the interval (1/x is the derivative of log(x))
        val errorMultiplier = Rational.one / a

        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        computeNewErrorTranscendental(range(x), propagatedError, prec)

      case x @ Let(id, value, body) =>
        val (valueError, valuePrec) = eval(value)

        val idPrec = precision(id)
        val error = if (idPrec < valuePrec) { // we need to cast down
          val valueRange = range(value)
          computeNewError(valueRange, valueError, idPrec)._1
        } else {
          valueError
        }

        intermediateErrors.put(Variable(id), (error, valuePrec)) // no problem as identifiers are unique
        eval(body)

      case Variable(_) => throw new Exception("Unknown variable")

      case _ => throw new Exception("Not supported")

    })
    val (resError, resPrecision) = eval(expr)
    (resError, intermediateErrors.mapValues(_._1).toMap)
  }


}
