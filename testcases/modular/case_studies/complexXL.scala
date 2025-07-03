import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object complexXL {

  def _add(rm1: Real): Real = {
    require(((rm1 >= 0.9) && (rm1 <= 11.1)))
    rm1 + 0.5
  }

  def radius(re: Real, im: Real): Real = {
    require(((re >= 0.001) && (re <= 11.6) && (im >= 0.001) && (im <= 11.6)))
    sqrt(((re * re) + (im * im)))
  }

  def divideRe(re1: Real, im1: Real, re2: Real, im2: Real): Real = {
    require(((re1 >= 0.9) && (re1 <= 12.1) && (im1 >= -0.1) && (im1 <= 6.7) && (re2 >= 0.9) && (re2 <= 50.1) && (im2 >= -0.1) && (im2 <= 6.7)))
    val denominator = (re2 * re2) + (im2 * im2)
    val res_re = ((re1 * re2) + (im1 * im2)) / denominator
    res_re
  }

  def divideIm(re1: Real, im1: Real, re2: Real, im2: Real): Real = {
    require(((re1 >= 0.9) && (re1 <= 12.1) && (im1 >= -0.1) && (im1 <= 6.7) && (re2 >= 0.9) && (re2 <= 50.1) && (im2 >= -0.1) && (im2 <= 6.7)))
    val denominator = (re2 * re2) + (im2 * im2)
    val res_im = ((im1 * re2) - (re1 * im2)) / denominator
    res_im
  }

  def reciprocalRe(re1: Real, im1: Real): Real = {
    require(((re1 >= 0.9) && (re1 <= 6.7) && (im1 >= 0.9) && (im1 <= 6.7)))
    val scale = re1 * re1 + im1 * im1
    val res_rel = re1 / scale
    res_rel
  }

  def reciprocalIm(re1: Real, im1: Real): Real = {
    require(((re1 >= 0.9) && (re1 <= 6.7) && (im1 >= 0.9) && (im1 <= 6.7)))
    val scale = re1 * re1 + im1 * im1
    val res_im = im1 / scale
    res_im
  }

  def impedanceIm(frequency5: Real, inductance: Real): Real = {
    require(((frequency5 >= 0.9) && (frequency5 <= 100.1) && (inductance >= 0.0) && (inductance <= 0.1)))

    val PI: Real = 3.14159265359
    2.0 * PI * frequency5 * inductance;
  }

  def instantVoltage(maxVoltage: Real, frequency4: Real, time: Real): Real = {
    require(((maxVoltage >= 0.9) && (maxVoltage <= 12.1) && (frequency4 >= 0.9) && (frequency4 <= 100.1) &&
      (time >= 1.0) && (time <= 6.0)))

    val PI: Real = 3.14159265359
    maxVoltage * cos(2.0 * PI * frequency4 * time)
  }

  def computeCurrentRe(maxVoltage: Real, frequency3: Real, inductance: Real, resistance: Real): Real = {
    require(((maxVoltage >= 1.0) && (maxVoltage <= 12.0) && (frequency3 >= 1.0) && (frequency3 <= 100.0) &&
      (inductance >= 0.001) && (inductance <= 0.004) && (resistance >= 1.0) && (resistance <= 50.0)))

    val impedance_im = impedanceIm(frequency3, inductance)
    divideRe(maxVoltage, 0.0, resistance, impedance_im)
  }

  def computeCurrentIm(maxVoltage: Real, frequency2: Real, inductance: Real, resistance: Real): Real = {
    require(((maxVoltage >= 1.0) && (maxVoltage <= 12.0) && (frequency2 >= 1.0) && (frequency2 <= 100.0) &&
      (inductance >= 0.001) && (inductance <= 0.004) && (resistance >= 1.0) && (resistance <= 50.0)))

    val impedance_im = impedanceIm(frequency2, inductance)
    divideIm(maxVoltage, 0.0, resistance, impedance_im)
  }

  def computeInstantCurrent(frequency1: Real, time: Real, maxVoltage: Real, inductance: Real, resistance: Real): Real = {
    require(((frequency1 >= 1.0) && (frequency1 <= 100.0) && (time >= 0.0) && (time <= 6.0) &&
      (maxVoltage >= 1.0) && (maxVoltage <= 12.0) && (inductance >= 0.001) && (inductance <= 0.004) &&
      (resistance >= 1.0) && (resistance <= 50.0)))

    val PI: Real = 3.14159265359
    val current_re: Real = computeCurrentRe(maxVoltage, frequency1, inductance, resistance)
    val current_im: Real = computeCurrentIm(maxVoltage, frequency1, inductance, resistance)
    val maxCurrent: Real = sqrt((current_re * current_re) + (current_im * current_im))
    val theta: Real = atan(current_im / current_re)
    maxCurrent * cos((2.0 * PI * frequency1 * time) + theta)
  }

  def approxEnergy(frequency: Real, maxVoltage: Real, inductance: Real, resistance: Real): Real = {
    require(((frequency >= 1.0) && (frequency <= 100.0) && (maxVoltage >= 1.0) && (maxVoltage <= 12.0) &&
      (inductance >= 0.001) && (inductance <= 0.004) && (resistance >= 1.0) && (resistance <= 50.0)))

    val t1: Real = 1.0
    val instCurrent1: Real = computeInstantCurrent(frequency, t1, maxVoltage, inductance, resistance)
    val instVoltage1: Real = instantVoltage(maxVoltage, frequency, t1)
    val instantPower1: Real = instCurrent1 * instVoltage1

    val t2: Real = _add(t1)
    val instCurrent2: Real = computeInstantCurrent(frequency, t2, maxVoltage, inductance, resistance)
    val instVoltage2: Real = instantVoltage(maxVoltage, frequency, t2)
    val instantPower2: Real = instCurrent2 * instVoltage2

    val t3: Real = _add(t2)
    val instCurrent3: Real = computeInstantCurrent(frequency, t3, maxVoltage, inductance, resistance)
    val instVoltage3: Real = instantVoltage(maxVoltage, frequency, t3)
    val instantPower3: Real = instCurrent3 * instVoltage3

    val t4: Real = _add(t3)
    val instCurrent4: Real = computeInstantCurrent(frequency, t4, maxVoltage, inductance, resistance)
    val instVoltage4: Real = instantVoltage(maxVoltage, frequency, t4)
    val instantPower4: Real = instCurrent4 * instVoltage4

    val t5: Real = _add(t4)
    val instCurrent5: Real = computeInstantCurrent(frequency, t5, maxVoltage, inductance, resistance)
    val instVoltage5: Real = instantVoltage(maxVoltage, frequency, t5)
    val instantPower5: Real = instCurrent5 * instVoltage5

    (0.5 * instantPower1) + (0.5 * instantPower2) + (0.5 * instantPower3) + (0.5 * instantPower4) + (0.5 * instantPower5)
  }

  def computeRadiusVector(re: Real, im: Real): Real = {
    require(((re >= 1) && (re <= 2.0) && (im >= 1) && (im <= 2.0)))

    val v1 = radius(re, im)
    val re2 = _add(re)
    val im2 = _add(im)
    val v2 = radius(re2, im2)
    val re3 = _add(re2)
    val im3 = _add(im2)
    val v3 = radius(re3, im3)
    val re4 = _add(re3)
    val im4 = _add(im3)
    val v4 = radius(re4, im4)
    val re5 = _add(re4)
    val im5 = _add(im4)
    val v5 = radius(re5, im5)
    val re6 = _add(re5)
    val im6 = _add(im5)
    val v6 = radius(re6, im6)
    val re7 = _add(re6)
    val im7 = _add(im6)
    val v7 = radius(re7, im7)
    val re8 = _add(re7)
    val im8 = _add(im7)
    val v8 = radius(re8, im8)
    val re9 = _add(re8)
    val im9 = _add(im8)
    val v9 = radius(re9, im9)
    val re10 = _add(re9)
    val im10 = _add(im9)
    val v10 = radius(re10, im10)

    v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10

  }

  def computeDivideVector(re1: Real, im1: Real, re2: Real, im2: Real): Real = {
    require(((re1 >= 1.0) && (re1 <= 2.0) && (im1 >= 1.0) && (im1 <= 2.0) && (re2 >= 1.0) && (re2 <= 2.0) && (im2 >= 1.0) && (im2 <= 2.0)))

    val v1 = divideRe(re1, im1, re2, im2)
    val v2 = divideIm(re1, im1, re2, im2)

    val re3 = _add(re1)
    val im3 = _add(im1)
    val re4 = _add(re2)
    val im4 = _add(im2)
    val v3 = divideRe(re3, im3, re4, im4)
    val v4 = divideIm(re3, im3, re4, im4)

    val re5 = _add(re3)
    val im5 = _add(im3)
    val re6 = _add(re4)
    val im6 = _add(im4)
    val v5 = divideRe(re5, im5, re6, im6)
    val v6 = divideIm(re5, im5, re6, im6)

    val re7 = _add(re5)
    val im7 = _add(im5)
    val re8 = _add(re6)
    val im8 = _add(im6)
    val v7 = divideRe(re7, im7, re8, im8)
    val v8 = divideIm(re7, im7, re8, im8)

    val re9 = _add(re7)
    val im9 = _add(im7)
    val re10 = _add(re8)
    val im10 = _add(im8)
    val v9 = divideRe(re9, im9, re10, im10)
    val v10 = divideIm(re9, im9, re10, im10)

    val re11 = _add(re9)
    val im11 = _add(im9)
    val re12 = _add(re10)
    val im12 = _add(im10)
    val v11 = divideRe(re11, im11, re12, im12)
    val v12 = divideIm(re11, im11, re12, im12)

    val re13 = _add(re11)
    val im13 = _add(im11)
    val re14 = _add(re12)
    val im14 = _add(im12)
    val v13 = divideRe(re13, im13, re14, im14)
    val v14 = divideIm(re13, im13, re14, im14)

    val re15 = _add(re13)
    val im15 = _add(im13)
    val re16 = _add(re14)
    val im16 = _add(im14)
    val v15 = divideRe(re15, im15, re16, im16)
    val v16 = divideIm(re15, im15, re16, im16)

    val re17 = _add(re15)
    val im17 = _add(im15)
    val re18 = _add(re16)
    val im18 = _add(im16)
    val v17 = divideRe(re17, im17, re18, im18)
    val v18 = divideIm(re17, im17, re18, im18)

    val re19 = _add(re17)
    val im19 = _add(im17)
    val re20 = _add(re18)
    val im20 = _add(im18)
    val v19 = divideRe(re19, im19, re20, im20)
    val v20 = divideIm(re19, im19, re20, im20)

    v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + v14 + v15 + v16 + v17 + v18 + v19 + v20

  }

  def computeReciprocalRadiusVector(re1: Real, im1: Real): Real = {
    require(((re1 >= 1.0) && (re1 <= 2.0) && (im1 >= 1.0) && (im1 <= 2.0)))

    val v1 = reciprocalRe(re1, im1)
    val v2 = reciprocalIm(re1, im1)

    val re2 = _add(re1)
    val im2 = _add(im1)
    val v3 = reciprocalRe(re2, im2)
    val v4 = reciprocalIm(re2, im2)

    val re3 = _add(re2)
    val im3 = _add(im2)
    val v5 = reciprocalRe(re3, im3)
    val v6 = reciprocalIm(re3, im3)

    val re4 = _add(re3)
    val im4 = _add(im3)
    val v7 = reciprocalRe(re4, im4)
    val v8 = reciprocalIm(re4, im4)


    val re5 = _add(re4)
    val im5 = _add(im4)
    val v9 = reciprocalRe(re5, im5)
    val v10 = reciprocalIm(re5, im5)

    val re6 = _add(re5)
    val im6 = _add(im5)
    val v11 = reciprocalRe(re6, im6)
    val v12 = reciprocalIm(re6, im6)

    val re7 = _add(re6)
    val im7 = _add(im6)
    val v13 = reciprocalRe(re7, im7)
    val v14 = reciprocalIm(re7, im7)

    val re8 = _add(re7)
    val im8 = _add(im7)
    val v15 = reciprocalRe(re8, im8)
    val v16 = reciprocalIm(re8, im8)

    val re9 = _add(re8)
    val im9 = _add(im8)
    val v17 = reciprocalRe(re9, im9)
    val v18 = reciprocalIm(re9, im9)

    val re10 = _add(re9)
    val im10 = _add(im9)
    val v19 = reciprocalRe(re10, im10)
    val v20 = reciprocalIm(re10, im10)

    val rad1 = radius(v1, v2)
    val rad2 = radius(v3, v4)
    val rad3 = radius(v5, v6)
    val rad4 = radius(v7, v8)
    val rad5 = radius(v9, v10)
    val rad6 = radius(v11, v12)
    val rad7 = radius(v13, v14)
    val rad8 = radius(v15, v16)
    val rad9 = radius(v17, v18)
    val rad10 = radius(v19, v20)

    rad1 + rad2 + rad3 + rad4 + rad5 + rad6 + rad7 + rad8 + rad9 + rad10

  }

  def approxEnergyXL(frequency: Real, maxVoltage: Real, inductance: Real, resistance: Real): Real = {
    require(((frequency >= 1.0) && (frequency <= 100.0) && (maxVoltage >= 1.0) && (maxVoltage <= 12.0) &&
      (inductance >= 0.001) && (inductance <= 0.004) && (resistance >= 1.0) && (resistance <= 50.0)))

    val t1: Real = 1.0
    val instCurrent1: Real = computeInstantCurrent(frequency, t1, maxVoltage, inductance, resistance)
    val instVoltage1: Real = instantVoltage(maxVoltage, frequency, t1)
    val instantPower1: Real = instCurrent1 * instVoltage1

    val t2: Real = _add(t1)
    val instCurrent2: Real = computeInstantCurrent(frequency, t2, maxVoltage, inductance, resistance)
    val instVoltage2: Real = instantVoltage(maxVoltage, frequency, t2)
    val instantPower2: Real = instCurrent2 * instVoltage2

    val t3: Real = _add(t2)
    val instCurrent3: Real = computeInstantCurrent(frequency, t3, maxVoltage, inductance, resistance)
    val instVoltage3: Real = instantVoltage(maxVoltage, frequency, t3)
    val instantPower3: Real = instCurrent3 * instVoltage3

    val t4: Real = _add(t3)
    val instCurrent4: Real = computeInstantCurrent(frequency, t4, maxVoltage, inductance, resistance)
    val instVoltage4: Real = instantVoltage(maxVoltage, frequency, t4)
    val instantPower4: Real = instCurrent4 * instVoltage4

    val t5: Real = _add(t4)
    val instCurrent5: Real = computeInstantCurrent(frequency, t5, maxVoltage, inductance, resistance)
    val instVoltage5: Real = instantVoltage(maxVoltage, frequency, t5)
    val instantPower5: Real = instCurrent5 * instVoltage5

    val t6: Real = _add(t5)
    val instCurrent6: Real = computeInstantCurrent(frequency, t6, maxVoltage, inductance, resistance)
    val instVoltage6: Real = instantVoltage(maxVoltage, frequency, t6)
    val instantPower6: Real = instCurrent6 * instVoltage6

    val t7: Real = _add(t6)
    val instCurrent7: Real = computeInstantCurrent(frequency, t7, maxVoltage, inductance, resistance)
    val instVoltage7: Real = instantVoltage(maxVoltage, frequency, t7)
    val instantPower7: Real = instCurrent7 * instVoltage7

    val t8: Real = _add(t7)
    val instCurrent8: Real = computeInstantCurrent(frequency, t8, maxVoltage, inductance, resistance)
    val instVoltage8: Real = instantVoltage(maxVoltage, frequency, t8)
    val instantPower8: Real = instCurrent8 * instVoltage8

    val t9: Real = _add(t8)
    val instCurrent9: Real = computeInstantCurrent(frequency, t9, maxVoltage, inductance, resistance)
    val instVoltage9: Real = instantVoltage(maxVoltage, frequency, t9)
    val instantPower9: Real = instCurrent9 * instVoltage9

    val t10: Real = _add(t9)
    val instCurrent10: Real = computeInstantCurrent(frequency, t10, maxVoltage, inductance, resistance)
    val instVoltage10: Real = instantVoltage(maxVoltage, frequency, t10)
    val instantPower10: Real = instCurrent10 * instVoltage10

    (0.5 * instantPower1) + (0.5 * instantPower2) + (0.5 * instantPower3) + (0.5 * instantPower4) +
      (0.5 * instantPower5) + (0.5 * instantPower6) + (0.5 * instantPower7) + (0.5 * instantPower8) +
      (0.5 * instantPower9) + (0.5 * instantPower10)
  }
}
