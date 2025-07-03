import scala.annotation.strictfp
import daisy.lang._
import Real._

@strictfp
object allTests_modular {

  def determinant(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((0.9 <= a) && (a <= 52.1) && (-40.1 <= b) && (b <= 10.1) && (0.9 <= c) && (c <= 125.1) &&
      (0.9 <= d) && (d <= 98.1) && (0.9 <= e) && (e <= 121.1) && (0.9 <= f) && (f <= 54.1) &&
      (-96.1 <= g) && (g <= 10.0) && (-10.1 <= h) && (h <= 20.1) && (-10.1 <= i) && (i <= 81.1))

    a * ((e * i) - (f * h)) - b * ((d * i) - (f * g)) + c * ((d * h) - (e * g))
  }

  // solving a system of equations using the Cramer's rule variable x
  def solveEquationX(a1: Real, b1: Real, c1: Real, d1: Real, a2: Real, b2: Real, c2: Real, d2: Real, a3: Real, b3: Real,
                     c3: Real, d3: Real): Real = {
    require(((22 <= a1) && (a1 <= 52) && (-40 <= b1) && (b1 <= -10) && (95 <= c1) && (c1 <= 125) && (1.0 <= d1) && (d1 <= 10.0) &&
      (68 <= a2) && (a2 <= 98) && (91 <= b2) && (b2 <= 121) && (24 <= c2) && (c2 <= 54) && (1.0 <= d2) && (d2 <= 10.0) &&
      (-96 <= a3) && (a3 <= -66) && (-10 <= b3) && (b3 <= 20) && (51 <= c3) && (c3 <= 81) && (1.0 <= d3) && (d3 <= 10.0)))


    val d: Real = determinant(a1, b1, c1, a2, b2, c2, a3, b3, c3)
    val d_x: Real = determinant(d1, b1, c1, d2, b2, c2, d3, b3, c3)

    val x: Real = d_x / d
    x
  }

  // solving a system of equations using the Cramer's rule -  variable y
  def solveEquationY(a1: Real, b1: Real, c1: Real, d1: Real, a2: Real, b2: Real, c2: Real, d2: Real, a3: Real, b3: Real,
                     c3: Real, d3: Real): Real = {
    require(((22 <= a1) && (a1 <= 52) && (-40 <= b1) && (b1 <= -10) && (95 <= c1) && (c1 <= 125) && (1.0 <= d1) && (d1 <= 10.0) &&
      (68 <= a2) && (a2 <= 98) && (91 <= b2) && (b2 <= 121) && (24 <= c2) && (c2 <= 54) && (1.0 <= d2) && (d2 <= 10.0) &&
      (-96 <= a3) && (a3 <= -66) && (-10 <= b3) && (b3 <= 20) && (51 <= c3) && (c3 <= 81) && (1.0 <= d3) && (d3 <= 10.0)))


    val d: Real = determinant(a1, b1, c1, a2, b2, c2, a3, b3, c3)
    val d_y: Real = determinant(a1, d1, c1, a2, d2, c2, a3, d3, c3)

    val y: Real = d_y / d
    y
  }

  // solving a system of equations using the Cramer's rule - variable z
  def solveEquationZ(a1: Real, b1: Real, c1: Real, d1: Real, a2: Real, b2: Real, c2: Real, d2: Real, a3: Real, b3: Real,
                     c3: Real, d3: Real): Real = {
    require(((22 <= a1) && (a1 <= 52) && (-40 <= b1) && (b1 <= -10) && (95 <= c1) && (c1 <= 125) && (1.0 <= d1) && (d1 <= 10.0) &&
      (68 <= a2) && (a2 <= 98) && (91 <= b2) && (b2 <= 121) && (24 <= c2) && (c2 <= 54) && (1.0 <= d2) && (d2 <= 10.0) &&
      (-96 <= a3) && (a3 <= -66) && (-10 <= b3) && (b3 <= 20) && (51 <= c3) && (c3 <= 81) && (1.0 <= d3) && (d3 <= 10.0)))


    val d: Real = determinant(a1, b1, c1, a2, b2, c2, a3, b3, c3)
    val d_z: Real = determinant(a1, b1, d1, a2, b2, d2, a3, d3, b3)

    val z: Real = d_z / d
    z
  }


  def solveEquationsVector(a11: Real, b11: Real, c11: Real, a12: Real, b12: Real, c12: Real, a13: Real, b13: Real, c13: Real,
                           a21: Real, b21: Real, c21: Real, a22: Real, b22: Real, c22: Real, a23: Real, b23: Real, c23: Real,
                           a31: Real, b31: Real, c31: Real, a32: Real, b32: Real, c32: Real, a33: Real, b33: Real, c33: Real,
                           d1: Real, d2: Real, d3: Real): Real = {

    require((23.0 <= a11) && (a11 <= 26.0) && (-39.0 <= b11) && (b11 <= -36.0) && (96.0 <= c11) && (c11 <= 99.0) &&
      (69.0 <= a12) && (a12 <= 72.0) && (92.0 <= b12) && (b12 <= 95.0) && (25.0 <= c12) && (c12 <= 28.0) &&
      (-95.0 <= a13) && (a13 <= -92.0) && (-9.0 <= b13) && (b13 <= -6.0) && (52.0 <= c13) && (c13 <= 55.0) &&
      (26.0 <= a21) && (a21 <= 30.0) && (-36.0 <= b21) && (b21 <= -32.0) && (99.0 <= c21) && (c21 <= 103.0) &&
      (72.0 <= a22) && (a22 <= 76.0) && (95.0 <= b22) && (b22 <= 99.0) && (28.0 <= c22) && (c22 <= 32.0) &&
      (-92.0 <= a23) && (a23 <= -88.0) && (-6.0 <= b23) && (b23 <= -2.0) && (55.0 <= c23) && (c23 <= 59.0) &&
      (30.0 <= a31) && (a31 <= 34.0) && (-32.0 <= b31) && (b31 <= -28.0) && (103.0 <= c31) && (c31 <= 107.0) &&
      (76.0 <= a32) && (a32 <= 80.0) && (99.0 <= b32) && (b32 <= 103.0) && (32.0 <= c32) && (c32 <= 36.0) &&
      (-88.0 <= a33) && (a33 <= -84.0) && (-2.0 <= b33) && (b33 <= 2.0) && (59.0 <= c33) && (c33 <= 63.0) &&
      (1.0 <= d1) && (d1 <= 10.0) && (1.0 <= d2) && (d2 <= 10.0) && (1.0 <= d3) && (d3 <= 10.0))


    val x1: Real = solveEquationX(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)
    val y1: Real = solveEquationY(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)
    val z1: Real = solveEquationZ(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)


    val x2: Real = solveEquationX(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)
    val y2: Real = solveEquationY(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)
    val z2: Real = solveEquationZ(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)


    val x3: Real = solveEquationX(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)
    val y3: Real = solveEquationY(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)
    val z3: Real = solveEquationZ(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)


    val avg = (x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3) / 9.0
    avg

  }

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

}