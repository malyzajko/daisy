// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.collection.immutable.Seq

import lang.Trees._
import lang.Identifiers._
import lang.TreeOps.allVariablesOf
import FinitePrecision._
import Rational.{min, abs, sqrtDown, one}

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
    trackRoundoffErrors: Boolean = true): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[Interval](expr, inputValMap, Interval.apply)

    // println(intermediateRanges.mkString("\n"))

    val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr, intermediateRanges,
      allVariablesOf(expr).map(id => (id -> uniformPrecision)).toMap,
      inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
      zeroError = AffineForm.zero,
      fromError = AffineForm.fromError,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors)

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
    trackRoundoffErrors: Boolean = true): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[AffineForm](expr,
      inputValMap.map(x => (x._1 -> AffineForm(x._2))), AffineForm.apply)

    val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr,
      intermediateRanges.map(x => (x._1 -> x._2.toInterval)),
      allVariablesOf(expr).map(id => (id -> uniformPrecision)).toMap,
      inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
      zeroError = AffineForm.zero,
      fromError = AffineForm.fromError,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors)

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
    trackRoundoffErrors: Boolean = true): (Rational, Interval) = {

    val (resRange, intermediateRanges) = evalRange[SMTRange](expr,
      inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) }),
      SMTRange.apply)

    val (resRoundoff, allErrors) = evalRoundoff[AffineForm](expr,
      intermediateRanges.map(x => (x._1 -> x._2.toInterval)),
      allVariablesOf(expr).map(id => (id -> uniformPrecision)).toMap,
      inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
      zeroError = AffineForm.zero,
      fromError = AffineForm.fromError,
      interval2T = AffineForm.apply,
      constantsPrecision = uniformPrecision,
      trackRoundoffErrors)

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
    rangeMap: Map[Expr, Interval],
    precisionMap: Map[Identifier, Precision],
    initErrorMap: Map[Identifier, T],
    zeroError: T,
    fromError: Rational => T,
    interval2T: Interval => T,
    constantsPrecision: Precision,
    trackRoundoffErrors: Boolean         // if false, propagate only initial errors
    ): (T, Map[Expr, T]) = {


    var intermediateErrors: Map[Expr, T] = Map.empty

    // TODO: check the effectiveness of this
    // @inline
    def computeNewError(range: Interval, propagatedError: T, prec: Precision): T =
      if (trackRoundoffErrors) {
        val actualRange: Interval = range + propagatedError.toInterval
        val rndoff = prec.absRoundoff(actualRange)
        propagatedError +/- rndoff
      } else {
        propagatedError
      }

    def eval(e: Expr, errorMap: Map[Identifier, T]): (T, Precision) = (e: @unchecked) match {

      case x @ RealLiteral(r) =>
        val rndoff = if (isExactInFloats(r, constantsPrecision) || !trackRoundoffErrors) {
          zeroError
        } else {
          fromError(constantsPrecision.absRoundoff(r))
        }
        intermediateErrors += (x -> rndoff)
        (rndoff, constantsPrecision)

      case x @ Variable(id) =>
        // TODO: if the error is just a roundoff, then we can also compute it here...
        val rndoff = errorMap(id)
        intermediateErrors += (x -> rndoff)
        (rndoff, precisionMap(id))

      case x @ Plus(lhs, rhs) =>
        val range = rangeMap(x)

        val (rndoffLhs, precLhs) = eval(lhs, errorMap)
        val (rndoffRhs, precRhs) = eval(rhs, errorMap)
        val propagatedError = rndoffLhs + rndoffRhs

        val prec = getUpperBound(precLhs, precRhs)  // Scala semantics
        val rndoff = computeNewError(range, propagatedError, prec)
        intermediateErrors += (x -> rndoff)
        (rndoff, prec)

      case x @ Minus(lhs, rhs) =>
        val range = rangeMap(x)

        val (rndoffLhs, precLhs) = eval(lhs, errorMap)
        val (rndoffRhs, precRhs) = eval(rhs, errorMap)
        val propagatedError = rndoffLhs - rndoffRhs

        val prec = getUpperBound(precLhs, precRhs)
        val rndoff = computeNewError(range, propagatedError, prec)
        intermediateErrors += (x -> rndoff)
        (rndoff, prec)

      case x @ Times(lhs, rhs) =>
        val range = rangeMap(x)
        val rangeLhs = rangeMap(lhs)
        val rangeRhs = rangeMap(rhs)

        val (rndoffLhs, precLhs) = eval(lhs, errorMap)
        val (rndoffRhs, precRhs) = eval(rhs, errorMap)

        val propagatedError =
          interval2T(rangeLhs) * rndoffRhs +
          interval2T(rangeRhs) * rndoffLhs +
          rndoffLhs * rndoffRhs

        val prec = getUpperBound(precLhs, precRhs)
        val rndoff = computeNewError(range, propagatedError, prec)
        intermediateErrors += (x -> rndoff)
        (rndoff, prec)


      case x @ Division(lhs, rhs) =>
        val range = rangeMap(x)
        val rangeLhs = rangeMap(lhs)
        val rangeRhs = rangeMap(rhs)

        val (rndoffLhs, precLhs) = eval(lhs, errorMap)
        val (rndoffRhs, precRhs) = eval(rhs, errorMap)

        // inverse, i.e. we are computing x * (1/y)
        val rightInterval = rangeRhs + rndoffRhs.toInterval // the actual interval, incl errors

        // the actual error interval can now contain 0, check this
        if (rightInterval.xlo <= 0 && rightInterval.xhi >= 0) {
          throw DivisionByZeroException("trying to divide by error interval containing 0")
        }
        val a = min(abs(rightInterval.xlo), abs(rightInterval.xhi))
        val errorMultiplier: Rational = -one / (a*a)
        val invErr = rndoffRhs * errorMultiplier

        // error propagation
        val inverse: Interval = rangeRhs.inverse

        var propagatedError =
          interval2T(rangeLhs) * invErr +
          interval2T(inverse) * rndoffLhs +
          rndoffLhs * invErr

        val prec = getUpperBound(precLhs, precRhs)
        val rndoff = computeNewError(range, propagatedError, prec)
        intermediateErrors += (x -> rndoff)
        (rndoff, prec)

      case x @ UMinus(t) =>
        val (rndoff, prec) = eval(t, errorMap)
        intermediateErrors += (x -> - rndoff)
        (- rndoff, prec)

      case x @ Sqrt(t) =>
        // TODO: needs to fail for fixed-point precision
        val range = rangeMap(x)
        val rangeT = rangeMap(t)
        val (errorT, prec) = eval(t, errorMap)

        val tInterval = rangeT
        val a = min(abs(tInterval.xlo), abs(tInterval.xhi))
        val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)
        val propagatedError = errorT * errorMultiplier

        // TODO: check that this operation exists for this precision
        val rndoff = computeNewError(range, propagatedError, prec)

        intermediateErrors += (x -> rndoff)
        (rndoff, prec)


      case x @ Let(id, value, body) =>
        val (valueRndoff, valuePrec) = eval(value, errorMap)

        // downcast required
        val idPrec = precisionMap(id)
        val rndoff = if (idPrec < valuePrec) { // we need to cast down
          val valueRange = rangeMap(value)
          computeNewError(valueRange, valueRndoff, idPrec)
        } else {
          valueRndoff
        }

        eval(body, errorMap + (id -> rndoff))

    }
    val (resRndoff, resPrecision) = eval(expr, initErrorMap)
    (resRndoff, intermediateErrors)
  }


}