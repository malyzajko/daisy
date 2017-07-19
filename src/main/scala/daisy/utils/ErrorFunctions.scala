
package daisy
package utils

import analysis._
import analysis.DynamicPhase._
import lang.Trees._
import lang.Identifiers._
import lang.NumAnnotation
import Interval._
import Rational._
import FinitePrecision._

import daisy.analysis.Sampler._
import MPFRFloat.{abs => mpfr_abs, max => mpfr_max, min => mpfr_min}

import scala.collection.immutable._

/**
  * Implements functions for calculating error
  * Given an expression, its input value map,
  * and its initial errors
  * Parameters can be tweaked by the implementing object
  */
trait ErrorFunctions {

  var trackRoundoffErrs: Boolean = true
  var attachToTree: Boolean = false
  var dynamicSamplesDefault: Int = 100000
  //var defaultUniformPrecision: Precision = Float64

  var reporter: Reporter
  val debugSection: DebugSection

  /**
    * Calculates static error using interval ranges
    * and affine errors
    *
    * @param expr Expression whose error is to be calculated
    * @param inputValMap Map from function identifier to its input value interval
    * @param inputErrorMap Map from function
    */
  def errorIntervalAffine(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision): Rational = {
    val (_, error) = evaluate[Interval, AffineForm](
      expr,
      inputValMap,
      inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
      Interval.apply,
      AffineForm.zero,
      AffineForm.fromError,
      AffineForm.apply,
      attachToTree,
      trackRoundoffErrs,
      uniformPrecision
    )
    maxAbs(error.toInterval)
  }

  /**
    * Calculates static error using affine ranges
    * and affine errors
    *
    * @param expr Expression whose error is to be calculated
    * @param inputValMap Map from function identifier to its input value interval
    * @param inputErrorMap Map from function
    */
  def errorAffineAffine(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision): Rational = {
    val (_, error) = evaluate[AffineForm, AffineForm](
      expr,
      inputValMap.map(x => (x._1 -> AffineForm(x._2))),
      inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
      AffineForm.apply,
      AffineForm.zero,
      AffineForm.fromError,
      (x: AffineForm) => x, // Affine => Affine
      attachToTree,
      trackRoundoffErrs,
      uniformPrecision
    )
    maxAbs(error.toInterval)
  }

  /**
    * Calculates static error using SMT ranges
    * and affine errors
    *
    * @param expr Expression whose error is to be calculated
    * @param inputValMap Map from function identifier to its input value interval
    * @param inputErrorMap Map from function
    */
  def errorSMTAffine(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    uniformPrecision: Precision): Rational = {
    val (_, error) = evaluate[SMTRange, AffineForm](
      expr,
      inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) }),
      inputErrorMap.map(x => (x._1 -> AffineForm.fromError(x._2))),
      SMTRange.apply,
      AffineForm.zero,
      AffineForm.fromError,
      (x: SMTRange) => AffineForm(x.toInterval),  // SMTRange => AffineForm
      attachToTree,
      trackRoundoffErrs,
      uniformPrecision
    )
    maxAbs(error.toInterval)
  }

  /**
    * Estimates the (roundoff) error of an expression dynamically
    * by executing the finite-precision computation side-by-side with a higher-
    * precision one in MPFR.
    * This version does not automatically add any roundoff to the inputs,
    * you have to specify input errors manually, or map all variable identifiers
    * to zero if you don't want any input errors.
    * Errors added to each input are randomly taken from the interval
    * [- input-error, + input-error]
    *
    * @param expr expression whose error is to be calculated
    * @param inputValMap map from variable identifier to its input interval
    * @param inputErrorMap map from variable identifier to its max. input error
    * @param dynamicSamples how many samples to use
    */
  def errorDynamic(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    inputErrorMap: Map[Identifier, Rational],
    dynamicSamples: Int = dynamicSamplesDefault): Rational = {

    val inputRanges: Map[Identifier, Interval] = inputValMap.map({
      case (id, i) =>
        (id, Interval(i.mid - inputRangeFactor * i.radius,
          i.mid + inputRangeFactor * i.radius))
    })

    val rand = new util.Random(3691285)
    val sampler = new Uniform(inputRanges, 485793)  //no seed = System millis
    // TODO: this should not be using the ErrorMeasurer as it only needs the abs error
    val measurer = new ErrorMeasurerMPFR()
    var currentMaxAbsMPFR = measurer.maxAbsError
    var currentMaxAbs: Double = measurer.maxAbsError.doubleValue

    var i = 0
    while (i < dynamicSamples) {
      i = i + 1
      // no input errors
      val dblInputsWithoutErrors: Map[Identifier, Double] = sampler.next
      // baseline without errors
      val mpfrInputs: Map[Identifier, MPFRFloat] = dblInputsWithoutErrors.map({
        case (x, d) => (x -> MPFRFloat.fromDouble(d))
      })
      // add errors to lower precision version, if there are any
      val dblInputs: Map[Identifier, Double] = dblInputsWithoutErrors.map({
        case (x, d) =>
          if (rand.nextBoolean) (x -> (d + inputErrorMap(x).toDouble * rand.nextDouble))
          else (x -> (d - inputErrorMap(x).toDouble * rand.nextDouble))
      })

      val dblOutput: Double = evalDouble(expr, dblInputs)
      val mpfrOutput: MPFRFloat = evalMPFR(expr, mpfrInputs)

      measurer.nextValues(dblOutput, mpfrOutput)

      // Invariant that absolute errors have to grow monotonically
      assert(currentMaxAbsMPFR <= measurer.maxAbsError)
      currentMaxAbsMPFR = measurer.maxAbsError

      assert(currentMaxAbs <= measurer.maxAbsError.doubleValue)
      currentMaxAbs = measurer.maxAbsError.doubleValue

    }
    Rational.fromString(measurer.maxAbsError.toString())

  }

  /**
    * Estimates the (roundoff) error of an expression dynamically
    * by executing the finite-precision computation side-by-side with a higher-
    * precision one in MPFR.
    * In this version roundoff errors on inputs are considered by converting
    * a double-valued sample into a String and considering that string as the
    * 'baseline'. If you don't want roundoff errors, use the other errorDynamic
    * function.
    *
    * @param expr expression whose error is to be calculated
    * @param inputValMap map from variable identifier to its input interval
    * @param dynamicSamples how many samples to use
    */
  def errorDynamicWithInputRoundoff(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    dynamicSamples: Int = dynamicSamplesDefault): Rational = {

    val inputRanges: Map[Identifier, Interval] = inputValMap.map({
      case (id, i) =>
        (id, Interval(i.mid - inputRangeFactor * i.radius,
          i.mid + inputRangeFactor * i.radius))
    })

    val sampler = new Uniform(inputRanges, 485793)  //no seed = System millis
    // TODO: no need for the ErrorMeasurer
    val measurer = new ErrorMeasurerMPFR()
    var currentMaxAbsMPFR = measurer.maxAbsError
    var currentMaxAbs: Double = measurer.maxAbsError.doubleValue

    var i = 0
    while (i < dynamicSamples) {
      i = i + 1
      // roundoff errors on inputs
      val dblInputs: Map[Identifier, Double] = sampler.next
      val mpfrInputs: Map[Identifier, MPFRFloat] = dblInputs.map({
        case (x, d) => (x -> MPFRFloat.fromString(d.toString))
      })

      val dblOutput: Double = evalDouble(expr, dblInputs)
      val mpfrOutput: MPFRFloat = evalMPFR(expr, mpfrInputs)

      measurer.nextValues(dblOutput, mpfrOutput)

      // Invariant that absolute errors have to grow monotonically
      assert(currentMaxAbsMPFR <= measurer.maxAbsError)
      currentMaxAbsMPFR = measurer.maxAbsError

      assert(currentMaxAbs <= measurer.maxAbsError.doubleValue)
      currentMaxAbs = measurer.maxAbsError.doubleValue

    }
    Rational.fromString(measurer.maxAbsError.toString())

  }

  /**
    * Analyses ranges and errors with different methods.
    *
    * To check which combinations are definitely supported, see the run method.
    * There you can also see the default values for the (good few) params needed.
    *
    * @param expr expression to be evaluated
    * @param initValMap initial ranges for all free variables of expr
    * @param initErrorMap errors (incl. roundoff errors) for all free variables
    * @param rangeFromReal constructor to create a range from a rational with
    *        the range arithmetic chosed to track ranges
    * @param zeroError expression denoting no error in the range arithmetic
    *        chose for tracking errors
    * @param fromError constructor to create the range arithm. expression from
    *        an (roundoff) error
    * @param t2s transformation function to cast the range arith. used for tracking
    *        ranges into the one tracking errors
    * @param attachToTree if true, ranges and errors will be attached to the tree
    *        Note: this can only be done once, and this method does not check
    *        whether a range is already attched (will crash in that case)
    * @param trackRoundoffErrs if true, roundoff errors will be tracked, otherwise
    *        only the initial errors will be propagated
    * @param uniformPrecision the default precision to use when doing absolute
    *                         errors calculations
    */
  def evaluate[T <: RangeArithmetic[T], S <: RangeArithmetic[S]](
    expr: Expr,
    initValMap: Map[Identifier, T],
    initErrorMap: Map[Identifier, S],
    rangeFromReal: Rational => T,
    zeroError: S,
    fromError: Rational => S,
    t2s: T => S,
    attachToTree: Boolean,
    trackRoundoffErrs: Boolean,
    uniformPrecision: Precision): (T, S) = {

    @inline
    def computeNewError(range: T, propagatedError: S): S =
      if (trackRoundoffErrs) {
        val actualRange: Interval = range.toInterval + propagatedError.toInterval
        val rndoff = uniformPrecision.absRoundoff(actualRange)
        propagatedError +/- rndoff
      } else {
        propagatedError
      }

    def eval(e: Expr, valMap: Map[Identifier, T], errorMap: Map[Identifier, S]): (T, S) =
      (e: @unchecked) match {

        case x @ Delta(id) =>
          val range = valMap(id)
          val error = errorMap(id)
          if (attachToTree) {
            x.interval = range.toInterval
            x.absError = maxAbs(error.toInterval)
          }
          (range, error)

        case x @ Epsilon(id) =>
          val range = valMap(id)
          val error = errorMap(id)
          if (attachToTree) {
            x.interval = range.toInterval
            x.absError = maxAbs(error.toInterval)
          }
          (range, error)

        case x @ Variable(id) =>
          val range = valMap(id)
          val error = errorMap(id)
          if (attachToTree) {
            x.interval = range.toInterval
            x.absError = maxAbs(error.toInterval)
          }
          (range, error)

        case x @ RealLiteral(r) =>
          val range = rangeFromReal(r)
          val error = if (isExactInFloats(r, uniformPrecision) || !trackRoundoffErrs) {
              zeroError
            } else {
              fromError(uniformPrecision.absRoundoff(r))
            }
          if (attachToTree) {
            x.absError = maxAbs(error.toInterval)
            x.interval = range.toInterval
          }
          (range, error)

        case x @ Plus(lhs, rhs) =>
          val (rangeLHS, errorLHS) = eval(lhs, valMap, errorMap)
          val (rangeRHS, errorRHS) = eval(rhs, valMap, errorMap)

          val range = rangeLHS + rangeRHS

          val propagatedError = errorLHS + errorRHS
          val newError = computeNewError(range, propagatedError)

          if (attachToTree) {
            x.absError = maxAbs(newError.toInterval)
            x.interval = range.toInterval
          }
          (range, newError)

        case x @ Minus(lhs, rhs) =>
          val (rangeLHS, errorLHS) = eval(lhs, valMap, errorMap)
          val (rangeRHS, errorRHS) = eval(rhs, valMap, errorMap)

          val range = rangeLHS - rangeRHS

          val propagatedError = errorLHS - errorRHS
          val newError = computeNewError(range, propagatedError)

          if (attachToTree) {
            x.absError = maxAbs(newError.toInterval)
            x.interval = range.toInterval
          }
          (range, newError)

        case x @ Times(lhs, rhs) =>
          val (rangeLHS, errorLHS) = eval(lhs, valMap, errorMap)
          val (rangeRHS, errorRHS) = eval(rhs, valMap, errorMap)

          // new range
          val range: T = rangeLHS * rangeRHS

          // error propagation
          val leftRangeInS = t2s(rangeLHS)
          val rightRangeInS = t2s(rangeRHS)
          val propagatedError = leftRangeInS * errorRHS + rightRangeInS * errorLHS +
                                  errorLHS*errorRHS

          // new roundoff
          val newError = computeNewError(range, propagatedError)

          if (attachToTree) {
            x.absError = maxAbs(newError.toInterval)
            x.interval = range.toInterval
          }
          (range, newError)

        case x @ Division(lhs, rhs) =>
          val (rangeLHS, errorLHS) = eval(lhs, valMap, errorMap)
          val (rangeRHS, errorRHS) = eval(rhs, valMap, errorMap)

          // new range
          val range = rangeLHS / rangeRHS

          // inverse, i.e. we are computing x * (1/y)
          val rightInterval = rangeRHS.toInterval + errorRHS.toInterval // the actual interval, incl errors

          //the actual error interval can now contain 0.
          //check this
          if (rightInterval.xlo <= 0 && rightInterval.xhi >= 0)
             throw DivisionByZeroException("trying to divide by error interval containing 0")

          val a = min(abs(rightInterval.xlo), abs(rightInterval.xhi))
          val errorMultiplier: Rational = -one / (a*a)
          val invErr = errorRHS * errorMultiplier

          // error propagation
          val leftRangeInS = t2s(rangeLHS)
          val inverse: T = rangeRHS.inverse
          val kRangeInS = t2s(inverse)
          var propagatedError = leftRangeInS * invErr + kRangeInS * errorLHS + errorLHS * invErr

          // new roundoff
          val newError = computeNewError(range, propagatedError)

          if (attachToTree) {
            x.absError = maxAbs(newError.toInterval)
            x.interval = range.toInterval
          }
          (range, newError)

        case x @ UMinus(t) =>
          val (range, error) = eval(t, valMap, errorMap)
          // negation does not affect error magnitude
          (-range, -error)

        case x @ Sqrt(t) =>
          // TODO: Not supported for fixed-points, add exception
          //case FPPrecision(_) => throw UnsupportedRealFragmentException("Sqrt not supported for fixed-points.")
          val (rangeT, errorT) = eval(t, valMap, errorMap)
          val range = rangeT.squareRoot

          // propagated existing errors
          val tInterval = rangeT.toInterval
          val a = min(abs(tInterval.xlo), abs(tInterval.xhi))
          val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)
          val propagatedError = errorT * errorMultiplier

          // new roundoff
          val newError = computeNewError(range, propagatedError)

          if (attachToTree) {
            x.absError = maxAbs(newError.toInterval)
            x.interval = range.toInterval
          }
          (range, newError)

        case Let(id, value, body) =>
          val (valueRange, valueError) = eval(value, valMap, errorMap)
          val newMapRange = valMap + (id -> valueRange)
          val newMapError = errorMap + (id -> valueError)
          eval(body, newMapRange, newMapError)
    }
    eval(expr, initValMap, initErrorMap)
  }


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
    //trackInitialErrs: Boolean,
    trackRoundoffErrs: Boolean,
    uniformPrecision: Precision): (Interval, Rational) = {

    // TODO: this should not be hardcoded
    val numSplits = 10

    val inputsSubdiv: Seq[Map[Identifier, Interval]] =  initValMap.foldLeft(Seq(Map[Identifier, Interval]()))({
      case (currSeq: Seq[Map[Identifier, Interval]], (id, intrvl)) =>
        val xlo = intrvl.xlo
        val splitWidth = intrvl.width / numSplits
        val splits: Seq[Interval] = (0 until numSplits).map(i =>
          Interval(xlo + i * splitWidth, xlo + (i+1) * splitWidth))

        currSeq.flatMap( m =>
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

        initErrorMap ++ missingIDs.map( id => (id -> zero))

      }

    val (resInterval, resError) = evaluate[Interval, AffineForm](
      expr, inputsSubdiv.head,
      getInputErrors(inputsSubdiv.head).map(x => (x._1 -> AffineForm.fromError(x._2))),
      Interval.apply, AffineForm.zero, AffineForm.fromError, AffineForm.apply,
      false, trackRoundoffErrs, uniformPrecision)

    var maxInterval = resInterval
    var maxError = maxAbs(resError.toInterval)

    for (m <- inputsSubdiv.tail) {

      val (tmpRange, tmpError) = evaluate[Interval, AffineForm](
        expr, m,
        getInputErrors(m).map(x => (x._1 -> AffineForm.fromError(x._2))),
        Interval.apply, AffineForm.zero, AffineForm.fromError, AffineForm.apply,
        false, trackRoundoffErrs, uniformPrecision)

      maxInterval = maxInterval.union(tmpRange)
      maxError = max(maxError, maxAbs(tmpError.toInterval))
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
    * @param expr expression to be analysed
    * @param inputRanges initial ranges of free variables
    * @param varErrorMap input errors (incl. roundoff) of free variables
    * @param trackRoundoffErrs whether or not to track roundoff errors or to
    *        propagate only input errors (as given in varErrorMap)
    * @param uniformPrecision the default precision to use when doing absolute
    *                         errors calculations
    */
  def evaluateSubdiv(
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
        val a = min(abs(rInt.xlo), abs(rInt.xhi))
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
        val a = min(abs(tInterval.xlo), abs(tInterval.xhi))
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
  }

  /**
    * Generic interval evaluation function, which can additionally perform
    * some action at every node.
    *
    * The action can, for example, be used to record the ranges for all
    * intermediate nodes. Alternatively, it can also be a no-op.
    *
    * @param e expression to be analysed
    * @param valMap ranges of free variables
    * @param action action to be performed at each node, the node and its range
    *        is passed to this function
    */
  def evalInterval(
    e: Expr,
    valMap: Map[Identifier, Interval],
    action: (Expr, Interval) => Unit): Interval = (e: @unchecked) match {

    case x @ Variable(id) =>
      valMap(id)

    case x @ RealLiteral(r) =>
      Interval(r)

    case x @ Plus(lhs, rhs) =>
      val intrvl = evalInterval(lhs, valMap, action) + evalInterval(rhs, valMap, action)
      action(x, intrvl)
      //currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
      intrvl

    case x @ Minus(lhs, rhs) =>
      val intrvl = evalInterval(lhs, valMap, action) - evalInterval(rhs, valMap, action)
      action(x, intrvl)
      //currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
      intrvl

    case x @ Times(lhs, rhs) =>
      val intrvl = evalInterval(lhs, valMap, action) * evalInterval(rhs, valMap, action)
      action(x, intrvl)
      //currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
      intrvl

    case x @ Division(lhs, rhs) =>
      try {
        val intrvl = evalInterval(lhs, valMap, action) / evalInterval(rhs, valMap, action)
        action(x, intrvl)
        //currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
        intrvl
      } catch {
        case e: utils.DivisionByZeroException =>
          reporter.fatalError(x.getPos, "possible division by zero")
      }

    case x @ UMinus(t) =>
      val intrvl = - evalInterval(t, valMap, action)
      action(x, intrvl)
      //currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
      intrvl

    case x @ Sqrt(t) =>
      try {
        val intrvl = evalInterval(t, valMap, action).squareRoot
        action(x, intrvl)
        //currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
        intrvl
      } catch {
        case e: utils.NegativeSqrtException =>
          reporter.fatalError(x.getPos, "possible negative square root")
      }

    case x @ Let(id, value, body) =>
      val intrvl = evalInterval(value, valMap, action)
      action(Variable(id), intrvl)
      //currentRanges = currentRanges + (Variable(id) -> currentRanges(Variable(id)).union(intrvl))
      val newValMap = valMap + (id -> intrvl)
      evalInterval(body, newValMap, action)
  }

}
