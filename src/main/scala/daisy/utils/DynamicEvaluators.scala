
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

trait DynamicEvaluators {

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


}