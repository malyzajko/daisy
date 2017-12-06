// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import tools.{MPFRFloat, Rational}
import MPFRFloat.{abs => mpfr_abs, max => mpfr_max, min => mpfr_min}
import Rational.{abs => rat_abs, max => rat_max, min => rat_min}
import daisy.lang.Identifiers.Identifier


/**
  Keeps track of absolute, relative and ulp errors seen so far.
  This class is used by the DynamicPhase.
 */
class ErrorMeasurerMPFR {

  var n = 0

  var currentAbsError_Min = MPFRFloat.fromDouble(100000.0)  // TODO: this should be MaxValue or some such
  var currentAbsError_Max = MPFRFloat.fromDouble(0.0)

  // var currentAbsError_Avrg = MPFRFloat(0.0)
  var currentAbsError_Sum = MPFRFloat.fromDouble(0.0)

  var currentRelError_Min = MPFRFloat.fromDouble(100000.0)  // this should be MaxValue or some such
  var currentRelError_Max = MPFRFloat.fromDouble(0.0)

  // var currentRelError_Avrg = MPFRFloat(0.0)
  var currentRelError_Sum = MPFRFloat.fromDouble(0.0)

  var currentUlpError_Min = Long.MaxValue
  var currentUlpError_Max = 0l
  var currentUlpError_Sum = 0l

  /*
    @return (abs, rel error, ulp error)
   */
  def nextValues(lowPrec: Double, highPrec: MPFRFloat): (MPFRFloat, MPFRFloat, Long) = {
    n = n + 1
    val absError = mpfr_abs(MPFRFloat.fromDouble(lowPrec) - highPrec)
    val relError = mpfr_abs(absError / highPrec)
    val ulpError = ulpd(lowPrec, highPrec.doubleValue)

    currentAbsError_Min = mpfr_min(currentAbsError_Min, absError)
    currentAbsError_Max = mpfr_max(currentAbsError_Max, absError)
    currentRelError_Min = mpfr_min(currentRelError_Min, relError)
    currentRelError_Max = mpfr_max(currentRelError_Max, relError)

    currentUlpError_Min = math.min(currentUlpError_Min, ulpError)
    currentUlpError_Max = math.max(currentUlpError_Max, ulpError)

    /* if (n != 0) {
      //val m_new = ((n - 1.0) * m + x) / n
      currentAbsError_Avrg = (MPFRFloat(n - 1) * currentAbsError_Avrg + absError) / MPFRFloat(n)
    } else {
      currentAbsError_Avrg = absError
    } */
    currentAbsError_Sum = currentAbsError_Sum + absError
    currentRelError_Sum = currentRelError_Sum + relError
    currentUlpError_Sum = currentUlpError_Sum + ulpError

    (absError, relError, ulpError)
  }

  def maxAbsError: MPFRFloat = currentAbsError_Max
  def minAbsError: MPFRFloat = currentAbsError_Min
  def maxRelError: MPFRFloat = currentRelError_Max
  def minRelError: MPFRFloat = currentRelError_Min

  def minUlpError: Long = currentUlpError_Min
  def maxUlpError: Long = currentUlpError_Max

  // def runningAverageAbs: MPFRFloat = currentAbsError_Avrg
  def avrgAbsError: MPFRFloat = (currentAbsError_Sum / MPFRFloat.fromDouble(n))
  def avrgRelError: MPFRFloat = (currentRelError_Sum / MPFRFloat.fromDouble(n))
  def avrgUlpError: Double = (currentUlpError_Sum.toDouble / n.toDouble)



  // from Zach Tatlock
  private def ulpd(_x: Double, _y: Double): Long = {
    var x = _x
    var y = _y
    if (x == 0) x = math.abs(x); // -0 == 0
    if (y == 0) y = math.abs(y); // -0 == 0

    if (x != x && y != y) return 0;  // NaN
    if (x != x) return Long.MinValue; // Maximum error
    if (y != y) return Long.MinValue; // Maximum error

    val xx = java.lang.Double.doubleToLongBits(x)
    if (xx < 0) Long.MinValue - xx

    val yy = java.lang.Double.doubleToLongBits(y)
    if (yy < 0) Long.MinValue - yy

    if (xx >= yy) {
      xx - yy
    } else {
      yy - xx
    }
  }
}

class ErrorMeasurerRational {
  private var n = 0
  var currentAbsError_Min = Rational(100000)  // TODO: this should be MaxValue or some such
  var currentAbsError_Max = Rational.zero
  var currentRelError_Min = Rational(100000)  // this should be MaxValue or some such
  var currentRelError_Max = Rational.zero

  // This does not work right now, probably because Rationals get too large
  // var currentAbsError_Sum = Rational.zero
  // var currentRelError_Sum = Rational.zero

  // TODO: ulp measure and average error

  /*
    @return new resp. current (abs error, rel error)
   */
  def nextValues(lowPrec: Double, highPrec: Rational, inputR: Map[Identifier, Rational],
    inputX: Map[Identifier, Double]): (Rational, Rational) = {

    n = n + 1
    val absError = rat_abs(Rational.fromDouble(lowPrec) - highPrec)
    val relError = rat_abs(absError / highPrec)

    currentAbsError_Min = rat_min(currentAbsError_Min, absError)
    currentAbsError_Max = rat_max(currentAbsError_Max, absError)
    currentRelError_Min = rat_min(currentRelError_Min, relError)
    currentRelError_Max = rat_max(currentRelError_Max, relError)

    // currentAbsError_Sum = currentAbsError_Sum + Rational.zero //absError
    // currentRelError_Sum = currentRelError_Sum + Rational.zero //relError
    (absError, relError)
  }

  def maxAbsError: Rational = currentAbsError_Max
  def minAbsError: Rational = currentAbsError_Min
  def maxRelError: Rational = currentRelError_Max
  def minRelError: Rational = currentRelError_Min

  // def avrgAbsError: Rational = (currentAbsError_Sum / Rational(n))
  // def avrgRelError: Rational = (currentRelError_Sum / Rational(n))
}
