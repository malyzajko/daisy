import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object instantCurrent {

  def divide_rel(real1: Real, img1: Real, real2: Real, img2: Real): Real = {
    require((0.0 <= real1) && (real1 <= 15.0) && (0.0 <= img1) && (img1 <= 15.0) && (0.0 <= real2) && (real2 <=50.0) &&
      (6.28318530717889E-4 <= img2) && (img2 <= 15.0))
    val denominator: Real = (real2 * real2) + (img2 * img2)
    val realPart = ((real1 * real2) + (img1 * img2)) / denominator
    realPart
  }

 def divide_img(real1: Real, img1: Real, real2: Real, img2: Real): Real = {
   require((1.0 <= real1) && (real1 <= 50.0) && (0.0 <= img1) && (img1 <= 50.0) && (0.9 <= real2) && (real2 <= 50.0) &&
     (0.0 <= img2) && (img2 <= 50.0))
    val denominator: Real = (real2 * real2) + (img2 * img2)
    val imaginaryPart = ((img1 * real2) - (real1 * img2)) / denominator
    imaginaryPart
  }

def computeCurrent_rel(maxVoltage: Real, resistance: Real, frequency: Real, inductance: Real): Real = {
  require((1.0 <= maxVoltage) && (maxVoltage <= 12.0) && (1.0 <= resistance) && (resistance <= 50.0) &&
  (0.1 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004))

  val pi: Real = 3.14159265359
  val t_rel: Real = resistance

  //impedance
  val t_img: Real = 2.0 * pi * frequency * inductance
  divide_rel(maxVoltage, 0.0, t_rel, t_img)
 }

  def computeCurrent_img(maxVoltage: Real, resistance: Real, frequency: Real, inductance: Real): Real = {
    require((1.0 <= maxVoltage) && (maxVoltage <= 12.0) && (1.0 <= resistance) && (resistance <= 50.0) &&
    (0.1 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004))

    val pi: Real = 3.14159265359
    val t_rel = resistance

    //impedance
    val t_img = 2.0 * pi * frequency * inductance
    divide_img(maxVoltage, 0.0, t_rel, t_img)
  }


  /*def computeInstantCurrent(time: Real, maxVoltage: Real, resistance: Real, frequency: Real, inductance: Real) : Real = {
    require((0.0 <= time) && (time <= 300.0) && (1.0 <= maxVoltage) && (maxVoltage <= 12.0) && (1.0 <= resistance) &&
      (resistance <= 50.0)  && (0.1 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004))
    val pi: Real = 3.14159265359

    val current_rel: Real = computeCurrent_rel(maxVoltage, resistance, frequency, inductance)
    val current_img: Real = computeCurrent_img(maxVoltage, resistance, frequency, inductance)
    val maxCurrent: Real = sqrt((current_rel * current_rel) + (current_img * current_img))
    val theta: Real = atan(current_img / current_rel)

    maxCurrent * cos((2.0 * 3.14159265359 * frequency * time) + theta)

  }*/

def computeInstantCurrent_theta(maxVoltage: Real, resistance: Real, frequency: Real, inductance: Real) : Real = {
  require((1.0 <= maxVoltage) && (maxVoltage <= 12.0) && (1.0 <= resistance) &&
      (resistance <= 50.0)  && (0.1 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004))
  val pi: Real = 3.14159265359

  val current_rel: Real = computeCurrent_rel(maxVoltage, resistance, frequency, inductance)
  val current_img: Real = computeCurrent_img(maxVoltage, resistance, frequency, inductance)
  val theta: Real = atan(current_img / current_rel)
  theta
  }

  def computeInstantCurrent_maxCurrent( maxVoltage: Real, resistance: Real, frequency: Real, inductance: Real) : Real = {
    require((1.0 <= maxVoltage) && (maxVoltage <= 12.0) && (1.0 <= resistance) &&
      (resistance <= 50.0)  && (0.1 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004))
    val pi: Real = 3.14159265359

    val current_rel: Real = computeCurrent_rel(maxVoltage, resistance, frequency, inductance)
    val current_img: Real = computeCurrent_img(maxVoltage, resistance, frequency, inductance)
    val maxCurrent: Real = sqrt((current_rel * current_rel) + (current_img * current_img))

    maxCurrent
  }

  def helperFunc(time:Real, maxVoltage: Real, resistance: Real, frequency: Real, inductance: Real) : Real = {
    require((0.0 <= time) && (time <= 300.0)  && (1.0 <= maxVoltage) && (maxVoltage <= 12.0) && (1.0 <= resistance) &&
      (resistance <= 50.0)  && (0.1 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004))

    val maxCurrent: Real = computeInstantCurrent_maxCurrent(maxVoltage, resistance, frequency, inductance)
    val theta : Real = computeInstantCurrent_theta(maxVoltage, resistance, frequency, inductance)
    maxCurrent * cos((2.0 * 3.14159265359 * frequency * time) + theta)
  }
}
