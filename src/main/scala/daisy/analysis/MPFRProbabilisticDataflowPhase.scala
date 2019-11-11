package daisy
package analysis


import lang.Identifiers._
import lang.Trees._
import daisy.lang.TreeOps
import daisy.lang.Types.FinitePrecisionType
import daisy.tools.FinitePrecision.{Precision, getUpperBound}
import daisy.utils.CachingMap
import daisy.tools.MPFRFloat.{abs, expUp, zero, one, two, sqrtDown}
import daisy.tools.{MPFRInterval, _}
import daisy.utils.CachingMap

import scala.collection.mutable.ListBuffer

import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.{MemXYSeries, XYChart, XYData, XYPlotStyle}


//Computes a probabilistic distribution of propagated roundoff error
object MPFRProbabilisticDataflowPhase extends DaisyPhase with Subdivision with RoundoffEvaluators{
  override val name = "Probabilistic Error Analysis"
  override val shortName = "probabilistic-errors"
  override val description = "Probabilistic analysis of roundoff errors"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    FlagOption("worstCase", "Only divides the outer interval and performs non probabilistic analysis for each outer division"), //Performs worst case analysis
    NumOption("dsSubDiv", 2, "Number of DS subdivision"),   //Number of initial distribution subdivision
    FlagOption("gaussian", "Gaussian input distribution, otherwise the inputs are considered to be uniform"), //Distribution of the inputs
    NumOption("divLimit", 500, "Max amount of interval divisions"),
    FlagOption("plot", "Plots the error distribution"), //Plots the error distribution at the output
    FlagOption("vertical", "Over approximates small error probability, otherwise over approximates large error"), //Output Format
    FlagOption("approximate", "Used for approximate round-off error computation"), //Performs worst case analysis
    NumOption("bigErrorMultiplier", 2, "Used with approximate option: big error magnitude"),
    DoubleOption("bigErrorProb", 0.1, "Used with approximate option: the probability of the big error"),
    DoubleOption("thresProb", 0.15, "The probability threshold to reduce small error or to report the big error")
  )

  implicit val debugSection = DebugSectionAnalysis

  def rat(d: Double) = Rational.fromReal(d)
  val initialDistLowLim = rat(-1.0)
  val initialDistHighLim = rat(1.0)

  val timeFormat = new java.text.SimpleDateFormat("HH:mm-dd-MM")
  val timestamp: String = timeFormat.format(new java.util.Date())

  type MPFRPBox = List[(MPFRFloat, MPFRFloat)]

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val reporter = ctx.reporter
    val trackRoundoffErrs = !ctx.hasFlag("noRoundoff")
    val uniformInput = !ctx.hasFlag("gaussian") // Flag for Gaussian Distribution
    val numDSSubDiv = ctx.option[Long]("dsSubDiv")
    val prec = ctx.option[Precision]("precision")
    val outerSubDiv = ctx.option[Long]("divLimit").toInt
    val approximate = ctx.hasFlag("approximate")
    val probabilistiAnalysis = !ctx.hasFlag("worstCase") // Flag for Gaussian Distribution
    val plotGraph = ctx.hasFlag("plot")
    val bigErrorMultiplier = ctx.option[Long]("bigErrorMultiplier").toInt
    val bigErrorProb = ctx.option[Double]("bigErrorProb")
    val inputDistribution = if (uniformInput) {
      DistributionGenerators.generateUniform(initialDistLowLim, initialDistHighLim, numDSSubDiv.toInt)
    } else {
      DistributionGenerators.generateStandardNormalCase1(initialDistLowLim, initialDistHighLim, numDSSubDiv.toInt)
    }

    val res = analyzeConsideredFunctions(ctx, prg) { fnc =>
      ctx.reporter.info("analyzing fnc: " + fnc.id)
      val start = System.currentTimeMillis
      val fncBody = fnc.body.get
      val inputRanges: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)
      val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id)
      val inputError: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)
      val noise = convertToMPFR(inputDistribution)


      val div = math.floor(math.pow(outerSubDiv, 1.0/(fnc.params.length)) + 0.01).toInt // 0.01 for roundoff
      //val subIntervals: Seq[Map[Identifier, Interval]] = getIntervalSubdivision(inputRanges, div).toList
      val inputErrorMap: Map[Identifier, MPFRDSAffineForm] = inputError.mapValues(MPFRDSAffineForm.+/-(_, noise))

      val mpfrInputRange: Map[Identifier, MPFRInterval] = inputRanges.map({
        case (id, interval) => id -> MPFRInterval(interval)})
      val subIntervals: Seq[Map[Identifier, (MPFRInterval, MPFRFloat, MPFRDSInterval)]] = genericCartesianProduct[(MPFRInterval,
        MPFRFloat, MPFRDSInterval)](srcCartesian(mpfrInputRange, numDSSubDiv.toInt, div, uniformInput))

      if (probabilistiAnalysis) {
        val probabilisticError = probabilisticErrorAnalysis(fncBody, subIntervals, precisionMap, inputErrorMap, noise, prec,
          trackRoundoffErrs, approximate, bigErrorMultiplier, bigErrorProb)
        probErrorOutputMPFR(ctx, probabilisticError)
        val probabilisticErrorDs = MPFRDSInterval(probabilisticError.toList)
        if (plotGraph) { //Plots the error graph
          drawPBox(probabilisticErrorDs)
        }

        val stop = System.currentTimeMillis
        val time = stop-start
        ctx.reporter.info("Time: " + time + "\n")

        (MPFRInterval.maxAbs(probabilisticErrorDs.toMPFRInterval), probabilisticErrorDs.toMPFRInterval)
      } else {
        val errors = subIntervals.map({ subInputRanges: Map[Identifier, (MPFRInterval, MPFRFloat, MPFRDSInterval)] =>
          val allWeights = subInputRanges.map({ case (id, (_, weight, _)) => weight })
          val subInputWeight = allWeights.fold(MPFRFloat.one)({ case (product, next) => product * next })
          val newInput: Map[Identifier, Interval] = subInputRanges.map({
            case (x, (a,b,c)) => x -> a.toInterval })
          val absError = nonProbabilisticErrorAnalysis(fncBody, newInput, precisionMap, inputError, prec, trackRoundoffErrs)
          val weight = Rational.fromString(subInputWeight.toString)
          (absError, weight)
        }) // end subIntervals.map

        val (err, weight) = errors.unzip
        val totalAbsError = err.tail.fold(err.head)({
          case (x, y) => Rational.max(x, y)
        })

        nonProbabilisticErrorOutput(ctx, errors)
        ctx.reporter.title("\nAbsolute error:")
        ctx.reporter.info(totalAbsError)

        val stop = System.currentTimeMillis
        val time = stop-start
        ctx.reporter.info("Time: " + time + "\n")

        totalAbsError
      }
    }
    (ctx, prg)
  }

  //resError, resRange
  def evalProbabilisticRoundoff(
      expr: Expr,
      precision: Map[Identifier, Precision],
      freeVarsMap: Map[Identifier, (MPFRDSAffineForm, MPFRDSAffineForm)],
      zeroError: MPFRDSAffineForm,
      fromError: MPFRFloat => MPFRDSAffineForm,
      constantsPrecision: Precision,
      trackRoundoffErrors: Boolean, // if false, propagate only initial errors
      approximate: Boolean,
      bigErrorMultiplier: Int,
      bigErrorProb: Double
      ): (MPFRDSAffineForm, MPFRDSAffineForm) = {

    //Expr, (intermediateError, precision, intermediateRange)
    val intermediateStages = new CachingMap[(Expr, PathCond), (MPFRDSAffineForm, Precision, MPFRDSAffineForm)]

    for ((id, (err, range)) <- freeVarsMap) {
      intermediateStages.put((Variable(id), emptyPath), (err, precision(id), range))
    }

    def computeNewError(range: MPFRDSAffineForm, propagatedError: MPFRDSAffineForm, prec: Precision): (MPFRDSAffineForm,
      Precision) = _computeNewError(range, propagatedError, prec, prec.absRoundoff)

    def computeNewErrorTranscendental(range: MPFRDSAffineForm, propagatedError: MPFRDSAffineForm,
      prec: Precision): (MPFRDSAffineForm, Precision) = _computeNewError(range, propagatedError, prec, prec.absTranscendentalRoundoff)

    def _computeNewError(range: MPFRDSAffineForm, propagatedError: MPFRDSAffineForm, prec: Precision,
      roundoffComputationMethod: Interval => Rational): (MPFRDSAffineForm, Precision) = {

      if (trackRoundoffErrors) {
        val affineRange: MPFRDSAffineForm = range + propagatedError // New range is computed by adding the range and the propagated error
        val probabilisticRange = mergeMPFR(affineRange.toDSStruct.dsi)
        val rationalDSRange = convertToRational(probabilisticRange) //TODO: The finite precision should be changed to MPFR as well, this can introduce round-off error
        val rationalRoundOffErrors = {

          if (approximate) { // Enters in case of approximate error specification
            val smallErrorProb = Rational.one - bigErrorProb
            val approxRoundOffError = new ListBuffer[(Interval, Rational)]
            for (x <- rationalDSRange) {
              val exactRoundOff = roundoffComputationMethod(x._1) // The exact error is computed here
              val approxRoundOff = exactRoundOff * bigErrorMultiplier // This computes the big error
              approxRoundOffError += ((Interval.+/-(exactRoundOff), (x._2 * smallErrorProb)))
              approxRoundOffError += ((Interval.+/-(approxRoundOff), (x._2 * bigErrorProb)))
            }
            merge(approxRoundOffError.toList) // Generates two focal elements following approximate error specification
          } else {
            // If no approximate error specification is given, this computes the roundoff error for each focal element
            merge(rationalDSRange.map(x => (Interval.+/-(roundoffComputationMethod(x._1)), (x._2))))
          }
        }
        // Normalize the error distribution
        val lowerBound = rationalRoundOffErrors.minBy(x => x._1.xlo)._1.xlo
        val upperBound = rationalRoundOffErrors.maxBy(x => x._1.xhi)._1.xhi
        val mgnt = Rational.max(Rational.abs(lowerBound), Rational.abs(upperBound))
        // normalize to get back the distribution within [-1,1]
        val mgntNormalizor = Rational.one / mgnt
        val rndoffErrDistribution = convertToMPFR(DSInterval(rationalRoundOffErrors.map(x => (x._1 * mgntNormalizor, x._2))))
        // The new error is added as a new noise
        (propagatedError :+ (MPFRInterval(mgnt), rndoffErrDistribution, affineRange.noise.map(_.index).toSet), prec)
      } else {
        (propagatedError, prec)
      }
    }

    def eval(e: Expr, p: PathCond): (MPFRDSAffineForm, Precision, MPFRDSAffineForm) = intermediateStages.getOrAdd((e, p), {
      case (RealLiteral(r), path ) =>
        val error = if (constantsPrecision.canRepresent(r) || !trackRoundoffErrors) {
          zeroError
        } else {
            val roundOff = constantsPrecision.absRoundoff(r)
            fromError(MPFRFloat.fromString(roundOff.toString))
        }
        (error, constantsPrecision, MPFRDSAffineForm.apply(r.toMPFRInterval))

      case (Plus(lhs, rhs), path) =>
        val (errorLhs, precLhs, rangeLhs) = eval(lhs, path)
        val (errorRhs, precRhs, rangeRhs) = eval(rhs, path)

        val propagatedError = errorLhs + errorRhs
        val newRange = rangeLhs + rangeRhs

        val computedError = computeNewError(newRange, propagatedError, getUpperBound(precLhs, precRhs))

        (computedError._1, computedError._2, newRange)


      case (Minus(lhs, rhs), path) =>
        val (errorLhs, precLhs, rangeLhs) = eval(lhs, path)
        val (errorRhs, precRhs, rangeRhs) = eval(rhs, path)

        val propagatedError = errorLhs - errorRhs

        val newRange = rangeLhs - rangeRhs
        val computedError = computeNewError(newRange, propagatedError, getUpperBound(precLhs, precRhs))

        (computedError._1, computedError._2, newRange)

      case (Times(lhs, rhs), path) =>
        val (errorLhs, precLhs, rangeLhs) = eval(lhs, path)
        val (errorRhs, precRhs, rangeRhs) = eval(rhs, path)
        //println("\n I am here")

        val propagatedError =
          rangeLhs * errorRhs +
            rangeRhs * errorLhs +
            errorLhs * errorRhs

        val newRange = rangeLhs * rangeRhs
        val computedError = computeNewError(newRange, propagatedError, getUpperBound(precLhs, precRhs))

        (computedError._1, computedError._2, newRange)


      case (FMA(fac1, fac2, sum), path) =>
        val (errorFac1, precFac1, rangeF1) = eval(fac1, path)
        val (errorFac2, precFac2, rangeF2) = eval(fac2, path)
        val (errorSum, precSum,  rangeSum) = eval(sum, path)

        val propagatedError =
          rangeF1 * errorFac2 +
            rangeF2 * errorFac1 +
            errorFac1 * errorFac2 +
            errorSum

        val newRange = rangeF1 * rangeF2 + rangeSum
        val computedError = computeNewError(newRange, propagatedError, getUpperBound(precFac1, precFac2, precSum))

        (computedError._1, computedError._2, newRange)


      case (Division(lhs, rhs), path) =>
        val (errorLhs, precLhs, rangeLhs) = eval(lhs, path)
        val (errorRhs, precRhs, rangeRhs) = eval(rhs, path)

        // inverse, i.e. we are computing x * (1/y)
        val rightInterval = rangeRhs + errorRhs // the actual interval, incl errors

        // the actual error interval can now contain 0, check this
        if (rightInterval.toMPFRInterval.includes(zero)) {
          throw DivisionByZeroException("trying to divide by error interval containing 0")
        }
        val a = MPFRInterval.minAbs(rightInterval.toMPFRInterval)
        val errorMultiplier: MPFRFloat = -one / (a*a)
        val invErr = errorRhs * errorMultiplier

        // error propagation
        val inverse: MPFRDSAffineForm = rangeRhs.inverse

        var propagatedError =
            rangeLhs * invErr +
            inverse * errorLhs +
            errorLhs * invErr

        val newRange = rangeLhs / rangeRhs
        val computedError = computeNewError(newRange, propagatedError, getUpperBound(precLhs, precRhs))

        (computedError._1, computedError._2, newRange)

      case (IntPow(base, n), path) =>
        val (errorT, prec, range) = eval(base, path)

        val newRange = range ^ n

        var r = newRange
        var e = errorT
        for (_ <- 0 until n) {
          e = r * errorT + newRange * e + e * errorT
          r *= newRange
        }
        // The error of pow in java.Math is 1 ulp, thus we rely that the method
        // computeNewErrorTranscendental gives us 1 ulp error
        val computedError = computeNewErrorTranscendental(newRange, e, prec)

        (computedError._1, computedError._2, newRange)

      case (UMinus(t), path) =>
        val (error, prec, range) = eval(t, path)
        (- error, prec, range.unary_-())

      case (Sqrt(t), path) =>
        // TODO: needs to fail for fixed-point precision
        val (errorT, prec, rangeT) = eval(t, path)

        if ((errorT.toMPFRInterval.xlo + rangeT.toMPFRInterval.xlo) < zero) {
          throw DivisionByZeroException("trying to take the square root of a negative number")
        }

        val a = MPFRInterval.minAbs(rangeT.toMPFRInterval)
        val errorMultiplier = (one/two) / sqrtDown(a) // I am not entirely sure about this

        val propagatedError = errorT * errorMultiplier
        val newRange = rangeT.squareRoot

        val computedError = computeNewError(newRange, propagatedError, prec)

        (computedError._1, computedError._2, newRange)


      case (Sin(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec, rangeT) = eval(t, path)

        // Bound the slope of sin(x) over the range by computing its
        // derivative (i.e. cos(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv =  rangeT.cosine.toMPFRInterval
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier
        val newRange = rangeT.sine

        // TODO: check that this operation exists for this precision
        val computedError = computeNewErrorTranscendental(newRange, propagatedError, prec)

        (computedError._1, computedError._2, newRange)


      case (Cos(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec, rangeT) = eval(t, path)

        // Bound the slope of cos(x) over the range by computing its
        // derivative (i.e. -sin(x)) as an interval and then taking the bound
        // with the larger absolute value.
        val deriv = -rangeT.sine.toMPFRInterval
        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier
        val newRange = rangeT.cosine

        // TODO: check that this operation exists for this precision
        val computedError = computeNewErrorTranscendental(newRange, propagatedError, prec)

        (computedError._1, computedError._2, newRange)


      case (Tan(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec, rangeT) = eval(t, path)

        // compute the derivative as 1/cos^2(x)
        val intCosine = rangeT.cosine
        val deriv = (intCosine * intCosine).inverse.toMPFRInterval

        val errorMultiplier = if (abs(deriv.xlo) > abs(deriv.xhi)) deriv.xlo else deriv.xhi
        val propagatedError = errorT * errorMultiplier
        val newRange = rangeT.tangent

        // TODO: check that this operation exists for this precision
        val computedError = computeNewErrorTranscendental(newRange, propagatedError, prec)

        (computedError._1, computedError._2, newRange)


      case (Exp(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec, rangeT) = eval(t, path)

        // maximal slope is always at the right ending point
        val b = rangeT.toMPFRInterval.xhi

        // compute the maximal slope over the interval
        // (exp(x) is the derivative of exp(x))
        val errorMultiplier = expUp(b)

        val propagatedError = errorT * errorMultiplier
        val newRange = rangeT.exp

        // TODO: check that this operation exists for this precision
        val computedError = computeNewErrorTranscendental(newRange, propagatedError, prec)

        (computedError._1, computedError._2, newRange)


      case (Log(t), path) =>
        // TODO not supported for fixed-points
        val (errorT, prec, rangeT) = eval(t, path)

        // maximal slope is always at the left ending point
        val a = rangeT.toMPFRInterval.xlo

        // compute the maximal slope over the interval (1/x is the derivative of log(x))
        val errorMultiplier = one / a

        val propagatedError = errorT * errorMultiplier
        val newRange = rangeT.log

        // TODO: check that this operation exists for this precision
        val computedError = computeNewErrorTranscendental(newRange, propagatedError, prec)

        (computedError._1, computedError._2, newRange)


      case (Let(id, value, body), path) =>
        val (valueError, valuePrec, valueRange) = eval(value, path)
        val idPrec = precision(id)
        val error = if (idPrec < valuePrec) { // we need to cast down
          computeNewError(valueRange, valueError, idPrec)._1
        } else {
          valueError
        }

        intermediateStages.put((Variable(id), emptyPath), (error, valuePrec, valueError)) // no problem as identifiers are unique
        eval(body, path)

      case (Variable(id), path) =>

        val stage = intermediateStages.getOrElse((Variable(id), emptyPath),
          throw new Exception("Unknown variable: " + id)) // needed when constants are pulled out by ConstantTransformerPhase

        // if we are using SMTRange, we can constrain the variable with the path condition
        // currently without effect for IA and AA

        // we need constraints on all the free variables in the path
        val pathVars = lang.TreeOps.allVariablesOf(And.make(path))
        val varsRanges = pathVars.map(id =>
          (id -> intermediateStages((Variable(id), emptyPath))))
        val varsConstrs = varsRanges.flatMap(x =>
          SMTRange.toConstraints(Variable(x._1), x._2._3.toInterval))

        if (path.nonEmpty) {
          stage.copy(_3 = stage._3.addConstraint(path.toSet ++ varsConstrs))
        } else {
          throw new Exception("Unknown variable: " + id)
        }

      case (IfExpr(cond, thenn, elze), path) =>
        //TODO handle condition (e.g. for x-c <= a case)

        // do the ELSE branch
        val negCond = TreeOps.negate(cond)

         // TODO: do something with the condition
        val (errorThen, precThen, rangeThen) = eval(thenn, path :+ cond)
        val (errorElse, precElse, rangeElse) = eval(elze, path :+ negCond)

        val newRange = rangeThen.union( rangeElse )
        val propagatedError = errorThen.maximizingUnion(errorElse)// take max of the two errors

        val computedError = computeNewError(newRange, propagatedError, getUpperBound(precThen, precElse))

        (computedError._1, computedError._2, newRange)

      case (Cast(t, FinitePrecisionType(prec)), path) =>
        val (errorT, precT, rangeT) = eval(t, path)

        // add new roundoff error corresponding to the cast precision
        val computedError = computeNewError(rangeT, errorT, prec)

        (computedError._1, computedError._2, rangeT)

      case _ => throw new Exception("Not supported")

    })
    val (resError, _, resRange) = eval(expr, emptyPath)
    (resError, resRange)
  }

  // non probabilistic error analysis
  private def nonProbabilisticErrorAnalysis(fncBody: Expr, subInputRanges: Map[Identifier, Interval], precisionMap: Map[Identifier, Precision],
   inputError: Map[Identifier, Rational], prec: Precision, trackRoundoffErrs: Boolean) : Rational = {
    // Evaluates the range and propagated error using affine arithmetic
    val (resRange, intermediateRanges) = evalRange[Interval](fncBody, subInputRanges, Interval.apply)
    // val resRange = rng.toInterval
    // val intermediateRanges = intrmdRange.mapValues(_.toInterval)

    val (resRoundoff, allErrors) = evalRoundoff[AffineForm](fncBody, intermediateRanges,
      precisionMap,
      inputError.mapValues(AffineForm.+/-),
      zeroError = AffineForm.zero,
      fromError = AffineForm.+/-,
      interval2T = AffineForm.apply,
      constantsPrecision = prec,
      trackRoundoffErrs)
    Interval.maxAbs(resRoundoff.toInterval)
  }

  // probabilistic error analysis
  private def probabilisticErrorAnalysis(fncBody: Expr,
    subIntervals: Seq[Map[Identifier, (MPFRInterval, MPFRFloat, MPFRDSInterval)]],
    precisionMap: Map[Identifier, Precision],
    inputErrorMap: Map[Identifier, MPFRDSAffineForm],
    noise: MPFRDSInterval,
    prec: Precision,
    trackRoundoffErrs: Boolean,
    approximate: Boolean,
    bigErrorMultiplier: Int,
    bigErrorProb: Double
    ) : Seq[(MPFRInterval, MPFRFloat)] = {
    // println("\n Big error:", bigErrorMultiplier)
    // println("\n Big error prob:", bigErrorProb)

    val res = subIntervals.map({ subInput: Map[Identifier, (MPFRInterval, MPFRFloat, MPFRDSInterval)] =>
      val allWeights = subInput.map({ case (id, (_, weight, _)) => weight })
      val subInputWeight = allWeights.fold(MPFRFloat.one)({ case (product, next) => product * next })
      var dsAffineInput = collection.immutable.Map[Identifier, MPFRDSAffineForm]()
      // val inputErrorMap: Map[Identifier, MPFRDSAffineForm] = inputError.mapValues(MPFRDSAffineForm.+/-(_, noise))
      val inputValMap = subInput.mapValues({ case (range, _, noise) => MPFRDSAffineForm(range, noise) })
      val inputErrorAndRangeMap = inputErrorMap.map(x => x._1 -> (inputErrorMap(x._1), inputValMap(x._1)))

      val (resError, realRange) = {
        evalProbabilisticRoundoff(fncBody,
        precisionMap,
        inputErrorAndRangeMap,
        zeroError = MPFRDSAffineForm(zero),
        fromError = MPFRDSAffineForm.+/-(_, noise),
        constantsPrecision = prec,
        trackRoundoffErrors = trackRoundoffErrs,
        approximate,
        bigErrorMultiplier,
        bigErrorProb
      )}
      val error = resError.toDSStruct.dsi
      for (x <- error) //iterating over all possible non zero intervals
        yield (x._1, (x._2 * subInputWeight))
    })// end subIntervals.map
    res.flatten
  }


  private def areLowBoundsSorted(l: Seq[(MPFRInterval, MPFRFloat)]): Boolean = {
      val list = l.view
      !list.zip(list.tail).exists { case ((x, _), (y, _)) => x.xlo > y.xlo }
  }

  private def generatePBox(errDist: Seq[(MPFRInterval, MPFRFloat)]): (MPFRPBox, MPFRPBox) = {
    var focalElems = errDist
    var lBound = MPFRFloat.zero
    var uBound = MPFRFloat.zero

    var LB: MPFRPBox = List()
    var UB: MPFRPBox = List()

    if(!areLowBoundsSorted(focalElems)) {
      focalElems = focalElems.sortBy(_._1.xlo)
    }
    // println(focalElems.size)
    for(i <- 0 until focalElems.size) {
      val dsi = focalElems(i)
      val (MPFRInterval(xlo, xhi), w) = dsi
      uBound = uBound + w
      lBound = lBound + w
      UB = (xlo, uBound) +: UB
      LB = (xhi, lBound) +: LB
    }
    //println(LB)
    (MPFRDSInterval.cleanPBoxForGraph(LB.reverse), MPFRDSInterval.cleanPBoxForGraph(UB.reverse))

  }

  // Draws the error P-Box at the output and puts it inside the output folder in a png file named result
  private def drawPBox(errDist: MPFRDSInterval) = {
    val (lpBox, upBox) = generatePBox(errDist.dsi)
    //println(lpBox)
    val file = "result"
    // val fcnName = "result"
    // val filename = "output/" + fcnName
    def rectConnect = (x: (MPFRFloat, MPFRFloat), l: List[(Double, Double)]) => {
      if( l.isEmpty ) {
        (x._1.doubleValue(), x._2.doubleValue()) +: l
      } else {
        (x._1.doubleValue(), x._2.doubleValue()) +: (l.head._1, x._2.doubleValue()) +: l
      }
    }

    // adds also upper line of upper pBoxes, which is drawn a bit more down such that it is not redrawn by axes
    val upBoxRect = (upBox.head._1.doubleValue(), 0d) +:
      upBox.foldRight(List[(Double, Double)]())(rectConnect) :+
      (upBox(upBox.size - 1)._1.doubleValue(), 0.998d) :+
      (lpBox(lpBox.size - 1)._1.doubleValue(), 0.998d)

    // adds also bottom line of lower pBoxes, which is drawn a bit more up such that it is not redrawn by axes
    val lpBoxRect = (upBox.head._1.doubleValue(), 0.002d) +: (lpBox.head._1.doubleValue(), 0.002d) +:
      lpBox.foldRight(List[(Double, Double)](  ))(rectConnect)

    val upBoxSeries = new MemXYSeries( upBoxRect.map(_._1), upBoxRect.map(_._2), "UpperBound")
    val lpBoxSeries = new MemXYSeries( lpBoxRect.map(_._1), lpBoxRect.map(_._2), "LowerBound")

    upBoxSeries.plotStyle = XYPlotStyle.Lines
    lpBoxSeries.plotStyle = XYPlotStyle.Lines

    val data = new XYData()
    data += upBoxSeries
    data += lpBoxSeries

    val chart = new XYChart("Error P-Box", data)

    //val file = filename.reverse.takeWhile( _.toString.takeRight(1) != "/" ).reverse
    val dir = "probabilistic-error-graphs/"
    //if(file.size < filename.size)
      //filename.substring(0, filename.size - file.size)
    //else
      //"output/"

    output(PDF(dir, file), chart)
  }

  // merge in Rational
  private def merge (d: List[(Interval, Rational)]): List[(Interval, Rational)] = {
    val dSorted = d.sortWith(_._1.xlo < _._1.xlo)
    val minThresWeight = Rational.fromReal(1e-10)

    var currentDS: List[(Interval, Rational)] = dSorted.tail.foldLeft(List(dSorted.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        if (r1 <= minThresWeight) {
          (Interval(intBefore.xlo, intNext.xhi), r1 + r2) :: acc.tail
        } else if (intBefore.equals(intNext)) {
          (intBefore, r1 + r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }.reverse
    currentDS
  }

  // merge in MPFR
  private def mergeMPFR (d: List[(MPFRInterval, MPFRFloat)]): List[(MPFRInterval, MPFRFloat)] = {
   val dSorted = d.sortWith(_._1.xlo < _._1.xlo)
   val minThresWeight = MPFRFloat.fromDouble(1e-10)

   var currentDS: List[(MPFRInterval, MPFRFloat)] = dSorted.tail.foldLeft(List(dSorted.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        if (r1 <= minThresWeight) {
          (MPFRInterval(intBefore.xlo, intNext.xhi), r1 + r2) :: acc.tail
        } else if (intBefore.equals(intNext)) {
          (intBefore, r1 + r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }.reverse
    currentDS
  }

  // Computes over approximated small error and its associated probability implemented in MPFR
  private def overApproxSmallErrorMPFR (lowDSSorted: Seq[(MPFRInterval, MPFRFloat)], thresProb: Double): (MPFRInterval, MPFRFloat) = {
    val thres = MPFRFloat.fromDouble(thresProb)/MPFRFloat.two
    var sumOfWeights = MPFRFloat.zero
    var prob = MPFRFloat.one

    var intermDS: List[(MPFRInterval, MPFRFloat)] = lowDSSorted.tail.foldLeft(List(lowDSSorted.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        sumOfWeights = sumOfWeights + r1
        if (sumOfWeights <= thres) {
          prob = prob - r1
          (intNext, r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }.reverse

    sumOfWeights = MPFRFloat.zero
    val highDSSorted = intermDS.sortWith(_._1.xhi > _._1.xhi)
    var finalDS: List[(MPFRInterval, MPFRFloat)] = highDSSorted.tail.foldLeft(List(highDSSorted.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        sumOfWeights = sumOfWeights + r1
        if (sumOfWeights <= thres) {
          prob = prob - r1
          (intNext, r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }

    val smallErrorLowBound = intermDS(0)._1.xlo
    val smallErrorHighBound = finalDS.last._1.xhi
    (MPFRInterval(smallErrorLowBound, smallErrorHighBound), prob)
  }

  private def overApproxLargeErrorMPFR(cleanDS: Seq[(MPFRInterval, MPFRFloat)], thresProb: Double): (MPFRInterval, MPFRFloat) = {
    val (lowPBox, highPBox) = generatePBox(cleanDS)
    val totalRange = (lowPBox.map(_._1) ::: highPBox.map(_._1)).distinct
    val sortedRange = totalRange.sorted
    val verticalDS = new ListBuffer[(MPFRInterval, MPFRFloat)]()

    for ((low, high) <- totalRange zip totalRange.drop(1)) {
      val computedInterval = MPFRInterval(low, high)
      val indexLow = lowPBox.indexWhere(_._1 > high)
      val indexHigh = highPBox.indexWhere(_._1 > low)
      var weightLow = MPFRFloat.zero
      var weightHigh = MPFRFloat.zero
      if (indexLow == -1) {
        weightHigh = lowPBox.last._2
      } else {
        weightHigh = lowPBox(indexLow-1)._2
      }
      if (indexHigh != 0) {
        weightLow = highPBox(indexHigh-1)._2
      }
      val weight = weightHigh - weightLow
      verticalDS += ((computedInterval, weight))
    }

    val largeErrorFromLeft = new ListBuffer[MPFRInterval]()
    val largeErrorFromRight = new ListBuffer[MPFRInterval]()

    val thres = MPFRFloat.fromDouble(thresProb)/MPFRFloat.two
    var sumOfWeights = MPFRFloat.zero
    var prob = MPFRFloat.one

    var intermDS: List[(MPFRInterval, MPFRFloat)] = verticalDS.tail.foldLeft(List(verticalDS.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        sumOfWeights = sumOfWeights + r1
        if (sumOfWeights <= thres) {
          prob = prob - r1
          largeErrorFromLeft += intBefore
          (intNext, r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }.reverse

    sumOfWeights = MPFRFloat.zero
    intermDS.tail.foldLeft(List(intermDS.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        sumOfWeights = sumOfWeights + r1
        if (sumOfWeights <= thres) {
          prob = prob - r1
          largeErrorFromRight += intBefore
          (intNext, r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }

    val largeErrorLowBound = largeErrorFromLeft(0).xlo
    val largeErrorHighBound = largeErrorFromRight.last.xhi
    (MPFRInterval(largeErrorLowBound, largeErrorHighBound), prob)
  }

  // Computes absolute error and prints the over approximated small error and its associated probability in MPFR for probabilistic error
  private def probErrorOutputMPFR (ctx: Context, errDist: Seq[(MPFRInterval, MPFRFloat)]) = {
    val thresProb = ctx.option[Double]("thresProb")
    val (intervals, weights) = errDist.unzip
    var absError = MPFRFloat.zero

    //var test1 = MPFRInterval.zero
    for (interval <- intervals) {
      val maxBound = MPFRInterval.maxAbs(interval)
      if (absError < maxBound) {
        absError = maxBound
        //test1 = interval
      }
    }
    //val test = errDist.find(_._1 == test1).get._2
    //println("\nError prob:",test)

    ctx.reporter.title("\nAbsolute error:")
    ctx.reporter.info(absError)

    val dSortedLowBounds = errDist.sortWith(_._1.xlo < _._1.xlo) //sorting error distribution with lower bounds

    if (!ctx.hasFlag("vertical")) {
      val (overArroxSmallError, prob) = overApproxSmallErrorMPFR(dSortedLowBounds, thresProb)
      val smallErrorMagnitude = MPFRInterval.maxAbs(overArroxSmallError)

      ctx.reporter.title("\n(error, probability):")
      ctx.reporter.info("(" + smallErrorMagnitude + ", " + prob + ")\n")
    } else {
      val (overArroxLargeError, prob) = overApproxLargeErrorMPFR(dSortedLowBounds, thresProb)
      val largeErrorMagnitude = MPFRInterval.maxAbs(overArroxLargeError)

      ctx.reporter.title("\n(error, probability):")
      ctx.reporter.info("(" + largeErrorMagnitude + ", " + prob + ")\n")
    }
  }

  // Computes over approximated small error and its associated probability implemented for worst case
  private def nonProbabilisticSmallError(sortedErr: Seq[(Rational, Rational)], thresProb: Double): (Rational, Rational) = {
    val thres = Rational.fromDouble(thresProb)
    var sumOfWeights = Rational.zero
    var prob = Rational.one

    var finalErr: List[(Rational, Rational)] = sortedErr.tail.foldLeft(List(sortedErr.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        sumOfWeights = sumOfWeights + r1
        if (sumOfWeights <= thres) {
          prob = prob - r1
          (intNext, r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }
    (finalErr.last._1, prob)
  }

  // Computes over approximated large error and its associated probability implemented for worst case
  private def nonProbabilisticLargeError(sortedErr: Seq[(Rational, Rational)], thresProb: Double): (Rational, Rational) = {
    val thres = Rational.fromDouble(thresProb)
    val smallErrProb = Rational.one - thres
    var sumOfWeights = Rational.zero
    var prob = Rational.one

    var finalErr: List[(Rational, Rational)] = sortedErr.tail.foldLeft(List(sortedErr.head)) {
      case (acc, (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        sumOfWeights = sumOfWeights + r1
        if (sumOfWeights <= smallErrProb) {
          prob = prob - r1
          (intNext, r2) :: acc.tail
        } else {
          (intNext, r2) :: acc
        }
    }
    (finalErr.last._1, prob)
  }


  // Computes absolute error and prints the over approximated small error and its associated probability for worst case analysis
  private def nonProbabilisticErrorOutput(ctx: Context, errDist: Seq[(Rational, Rational)]) = {
    val thresProb = ctx.option[Double]("thresProb")
    val (absErrors, weights) = errDist.unzip
    val dSorted= errDist.sortWith(_._1 > _._1) //The final error itnerval is not a proper p-box anymore

    if (!ctx.hasFlag("vertical")) {
      val (overArroxSmallError, prob) = nonProbabilisticSmallError(dSorted, thresProb)
      ctx.reporter.title("\nOver Approximated Small Error:")
      ctx.reporter.info(overArroxSmallError + ", " + prob + "\n")
    } else {
      val (overArroxLargeError, prob) = nonProbabilisticLargeError(dSorted, thresProb)
      ctx.reporter.title("\nOver Approximated Large Error:")
      ctx.reporter.info(overArroxLargeError + ", " + prob + "\n")
    }
  }

  private def convertToMPFR(d: DSInterval) : MPFRDSInterval = { // TODO: remove this after converting the error computation in MPFR
    val list = d.dsi
    MPFRDSInterval(for (x <- list)
      yield (MPFRInterval(x._1), MPFRFloat.fromString(x._2.toString)))
  }

  private def convertToRational(list: List[(MPFRInterval, MPFRFloat)]): List[(Interval, Rational)] = {
    for (x <- list)
      yield (x._1.toInterval, Rational.fromString(x._2.toString))
  }

  private def genericCartesianProduct[T](a: Map[Identifier, Seq[T]]): Seq[Map[Identifier, T]] = {
    def product(a: List[(Identifier, Seq[T])]): Seq[Map[Identifier, T]] =
      a match {
        case (name, values) :: tail =>
          for {
            result <- product(tail)
            value <- values
          } yield Map(name -> value).++(result)
        case Nil => Seq(Map.empty)
      }
    product(a.toList)
  }

  private def srcCartesian(inputRanges: Map[Identifier, MPFRInterval], numDSSubdiv: Int, divLimit: Int, uniformInput: Boolean): Map[Identifier,
    Seq[(MPFRInterval, MPFRFloat, MPFRDSInterval)]] = {
    val supportNormalized = MPFRInterval(- MPFRFloat.one, MPFRFloat.one).divide(numDSSubdiv)
    inputRanges.map({
      case (id, interval) if (interval.xlo.equals(interval.xhi)) =>
        val noise = if (uniformInput) {
            DistributionGenerators.generateUniform(initialDistLowLim, initialDistHighLim, numDSSubdiv)
          } else {
            DistributionGenerators.generateStandardNormalCase1(initialDistLowLim, initialDistHighLim, numDSSubdiv)
          }
        // val noise = DistributionGenerators.generateStandardNormalCase1(initialDistLowLim, initialDistHighLim, numDSSubdiv)
        val mpfrNoise = convertToMPFR(noise)
        (id -> Seq( (interval, MPFRFloat.one, mpfrNoise) ))

      case (id, interval) =>
        // there is something to subdivide
        val numTotalSubdivs = numDSSubdiv * divLimit
        val noiseTotal = if (uniformInput) {
            DistributionGenerators.generateUniform(initialDistLowLim, initialDistHighLim, numTotalSubdivs)
          } else {
            DistributionGenerators.generateStandardNormalCase1(initialDistLowLim, initialDistHighLim, numTotalSubdivs)
          }
        // val noiseTotal: DSInterval = DistributionGenerators.generateStandardNormalCase1(initialDistLowLim, initialDistHighLim, numTotalSubdivs)
        val mpfrNoiseTotal = convertToMPFR(noiseTotal)

        // get slices of the noise of size inner subdiv
        val outerSubdiv: List[MPFRDSInterval] = {
          val outerDiv = new scala.collection.mutable.ListBuffer[MPFRDSInterval]
          val innerSlice = mpfrNoiseTotal.dsi.grouped(numDSSubdiv).toList
          innerSlice.foreach(i => outerDiv += MPFRDSInterval(i))
          outerDiv.toList
        }

        assert(outerSubdiv.length == divLimit)  // we got the correct number of outer subdivisions

        val outerSubdivRanges = interval.divide(divLimit)
        // match each noise with the corresponding outer interval
        (id -> outerSubdiv.zip(outerSubdivRanges).map({
      case (noise, outerInterval) =>
        // normalize the noise to have support [-1, 1] and total weight 1
        // for the support [-1, 1], it's easy as we know how many pieces it was subdivided to
        val (_, weights: Seq[MPFRFloat]) = noise.dsi.unzip
                  // total weight of this slice
        val weightSum: MPFRFloat = weights.fold(MPFRFloat.zero)({case (sum, weight) => sum + weight })
        val weightsNormalized = weights.map( w => w / weightSum )

        (outerInterval, weightSum, MPFRDSInterval(supportNormalized.toList.zip(weightsNormalized)))
      }))
    }) // end cartesian
  }
}
