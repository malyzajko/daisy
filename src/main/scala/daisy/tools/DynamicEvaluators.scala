// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import analysis._
import lang.Trees._
import lang.Identifiers._

import daisy.analysis.Sampler._

import scala.collection.immutable._

trait DynamicEvaluators {

  var inputRangeFactor: Rational = 1

  var dynamicSamplesDefault: Int = 100000

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
    val sampler = new Uniform(inputRanges, 485793)  // no seed = System millis
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
          if (rand.nextBoolean) {
            (x -> (d + inputErrorMap(x).toDouble * rand.nextDouble))
          } else {
            (x -> (d - inputErrorMap(x).toDouble * rand.nextDouble))
          }
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

    val sampler = new Uniform(inputRanges, 485793)  // no seed = System millis
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

  def dynamicErrorEvaluation(expr: Expr, inputConfig: Map[Identifier, Interval], 
    seed: Long, numSamples: Long, useRoundoff: Boolean): ErrorMeasurerMPFR = {

    val sampler = new Uniform(inputConfig, seed)
    val measurer = new ErrorMeasurerMPFR()
    var i = 0
    while (i < numSamples) {
      i = i + 1
      val strInputs: Map[Identifier, String] = sampler.nextString
      val dblInputs: Map[Identifier, Double] = strInputs.map({
        case (x, s) => (x -> s.toDouble)
      })
      val mpfrInputs: Map[Identifier, MPFRFloat] =
      if (useRoundoff) {
        // WITH input errors
        strInputs.map({
          case (x, s) => (x -> MPFRFloat.fromString(s))
        })
      } else {
        // no input errors
        dblInputs.map({
          case (x, d) => (x -> MPFRFloat.fromDouble(d))
        })
      }
      val dblOutput: Double = evalDouble(expr, dblInputs)
      val mpfrOutput: MPFRFloat = evalMPFR(expr, mpfrInputs)
      measurer.nextValues(dblOutput, mpfrOutput)
    }
    measurer
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
  def generalErrorDynamicWithInputRoundoff(
    expr: Expr,
    inputValMap: Map[Identifier, Interval],
    dynamicSamples: Int = dynamicSamplesDefault): ErrorMeasurerMPFR = {

    val inputRanges: Map[Identifier, Interval] = inputValMap.map({
      case (id, i) =>
        (id, Interval(i.mid - inputRangeFactor * i.radius,
          i.mid + inputRangeFactor * i.radius))
    })

    val sampler = new Uniform(inputRanges, 485793)  //no seed = System millis
    val measurer = new ErrorMeasurerMPFR()
    //var currentAvrgAbsMPFR = measurer.avrgError
    //var currentAvrgAbs: Double = measurer.maxAvrgError.doubleValue

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

      //currentAvrgAbsMPFR = measurer.maxAbsError
      //currentAvrgAbs = measurer.maxAbsError.doubleValue

    }
    //Rational.fromString(measurer.maxAbsError.toString())

    // and then get whichever measure you need
    measurer
  }

  def evalRational(expr: Expr, _valMap: Map[Identifier, Rational]): Rational = {
    var valMap = _valMap

    def eval(e: Expr): Rational = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case RealLiteral(r) => r
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
//      case Pow(x, y) => eval(x) ^ eval(y)
      case IntPow(x, y) => eval(x) ^ y
      case UMinus(x) => - eval(x)
      case Let(id, v, b) =>
        valMap += (id -> eval(v))
        eval(b)

    }
    eval(expr)

  }

  def evalDouble(expr: Expr, _valMap: Map[Identifier, Double]): Double = {
    var valMap = _valMap

    def eval(e: Expr): Double = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case RealLiteral(r) => r.toDouble
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
//      case Pow(x, y) => math.pow(eval(x), eval(y))
      case IntPow(x, y) => math.pow(eval(x), y)
      case UMinus(x) => - eval(x)
      case Sqrt(x) => math.sqrt(eval(x))
      case Sin(x) => math.sin(eval(x))
      case Cos(x) => math.cos(eval(x))
      case Tan(x) => math.tan(eval(x))
      case Asin(x) => math.asin(eval(x))
      case Acos(x) => math.acos(eval(x))
      case Atan(x) => math.atan(eval(x))
      case Exp(x) => math.exp(eval(x))
      case Log(x) => math.log(eval(x))
      case Let(id, v, b) =>
        valMap += (id -> eval(v))
        eval(b)

    }
    eval(expr)

  }

  def evalMPFR(expr: Expr, _valMap: Map[Identifier, MPFRFloat]): MPFRFloat = {
    var valMap = _valMap

    def eval(e: Expr): MPFRFloat = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case r: RealLiteral => MPFRFloat.fromString(r.stringValue)
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
//      case Pow(x, y) => MPFRFloat.pow(eval(x), eval(y))
      case IntPow(x, y) => MPFRFloat.pow(eval(x), MPFRFloat.fromDouble(y))
      case UMinus(x) => - eval(x)
      case Sqrt(x) => MPFRFloat.sqrt(eval(x) )
      case Sin(x) => MPFRFloat.sin(eval(x))
      case Cos(x) => MPFRFloat.cos(eval(x))
      case Tan(x) => MPFRFloat.tan(eval(x))
      case Asin(x) => MPFRFloat.asin(eval(x))
      case Acos(x) => MPFRFloat.acos(eval(x))
      case Atan(x) => MPFRFloat.atan(eval(x))
      case Exp(x) => MPFRFloat.exp(eval(x))
      case Log(x) => MPFRFloat.log(eval(x))
      case Let(id, v, b) =>
        valMap += (id -> eval(v))
        eval(b)

    }
    eval(expr)
  }
}