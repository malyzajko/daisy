import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object instantaneousCurrent {


  def instantaneousCurrent(t: Real, resistance: Real, frequency: Real, inductance: Real, maxVoltage: Real): Real = {
    require(((0.0 <= t) && (t <= 300.0) && (1.0 <= resistance) && (resistance <= 50.0) && (1.0 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004) && (1.0 <= maxVoltage) && (maxVoltage <= 12.0)))
    val pi: Real = 3.14159265359
    val impedance_re: Real = resistance
    val impedance_im: Real = (((2.0 * pi) * frequency) * inductance)
    val denom: Real = ((impedance_re * impedance_re) + (impedance_im * impedance_im))
    val current_re: Real = ((maxVoltage * impedance_re) / denom)
    val current_im: Real = (-((maxVoltage * impedance_im)) / denom)
    val maxCurrent: Real = sqrt(((current_re * current_re) + (current_im * current_im)))
    val theta: Real = atan((current_im / current_re))
    (maxCurrent * cos(((((2.0 * pi) * frequency) * t) + theta)))
  }

}
