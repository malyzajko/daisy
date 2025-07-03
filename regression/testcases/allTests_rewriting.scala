import scala.annotation.strictfp
import daisy.lang._
import Real._

@strictfp
object allTests_rewriting {


  // def azimuth(lat1: Real, lat2: Real, lon1: Real, lon2: Real): Real = {
  //   require(((0.0 <= lat1) && (lat1 <= 0.4) && (0.5 <= lat2) && (lat2 <= 1.0) && (0.0 <= lon1) && (lon1 <= 3.14159265) && (-3.14159265 <= lon2) && (lon2 <= -0.5)))
  //   val dLon: Real = (lon2 - lon1)
  //   val s_lat1: Real = sin(lat1)
  //   val c_lat1: Real = cos(lat1)
  //   val s_lat2: Real = sin(lat2)
  //   val c_lat2: Real = cos(lat2)
  //   val s_dLon: Real = sin(dLon)
  //   val c_dLon: Real = cos(dLon)
  //   atan(((c_lat2 * s_dLon) / ((c_lat1 * s_lat2) - ((s_lat1 * c_lat2) * c_dLon))))
  // }

  def bspline3(u: Real): Real = {
    require(((0.0 <= u) && (u <= 1.0)))
    (-(((u * u) * u)) / 6.0)
  }

  // def carbonGas(v: Real): Real = {
  //   require(((0.1 <= v) && (v <= 0.5)))
  //   val p: Real = 35000000.0
  //   val a: Real = 0.401
  //   val b: Real = 4.27e-05
  //   val t: Real = 300.0
  //   val n: Real = 1000.0
  //   val k: Real = 1.3806503e-23
  //   (((p + ((a * (n / v)) * (n / v))) * (v - (n * b))) - ((k * n) * t))
  // }
  
  // def carthesianToPolar_radius(x: Real, y: Real): Real = {
  //   require(((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0)))
  //   sqrt(((x * x) + (y * y)))
  // }

  // def carthesianToPolar_theta(x: Real, y: Real): Real = {
  //   require(((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0)))
  //   val pi: Real = 3.14159265359
  //   val radiant: Real = atan((y / x))
  //   (radiant * (180.0 / pi))
  // }

  // def cav10(x: Real): Real = {
  //   require(((0.0 < x) && (x < 10.0)))
  //   val temp: Real = if ((((x * x) - x) >= 0.0)) {
  //     (x / 10.0)
  //   }
  //   else {
  //     ((x * x) + 2.0)
  //   }
  //   temp
  // } // warning subdiv

  def complex_sine_cosine(re: Real, im: Real): Real = {
    require(((re >= -10.0) && (re <= 10.0) && (im >= -10.0) && (im <= 10.0)))
    ((0.5 * sin(re)) * (exp(-(im)) - exp(im)))
  }

  // def complex_square_root(re: Real, im: Real): Real = {
  //   require(((re >= 0.001) && (re <= 10.0) && (im >= 0.001) && (im <= 10.0)))
  //   (0.5 * sqrt((2.0 * (sqrt(((re * re) + (im * im))) + re))))
  // }

  // def delta(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
  //   require(((4.0 <= x1) && (x1 <= 6.3504) && (4.0 <= x2) && (x2 <= 6.3504) && (4.0 <= x3) && (x3 <= 6.3504) && (4.0 <= x4) && (x4 <= 6.3504) && (4.0 <= x5) && (x5 <= 6.3504) && (4.0 <= x6) && (x6 <= 6.3504)))
  //   ((((((((x1 * x4) * (((((-(x1) + x2) + x3) - x4) + x5) + x6)) + ((x2 * x5) * (((((x1 - x2) + x3) + x4) - x5) + x6))) + ((x3 * x6) * (((((x1 + x2) - x3) + x4) + x5) - x6))) + ((-(x2) * x3) * x4)) + ((-(x1) * x3) * x5)) + ((-(x1) * x2) * x6)) + ((-(x4) * x5) * x6))
  // }

  // def delta4(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
  //   require(((4.0 <= x1) && (x1 <= 6.3504) && (4.0 <= x2) && (x2 <= 6.3504) && (4.0 <= x3) && (x3 <= 6.3504) && (4.0 <= x4) && (x4 <= 6.3504) && (4.0 <= x5) && (x5 <= 6.3504) && (4.0 <= x6) && (x6 <= 6.3504)))
  //   ((((((-(x2) * x3) - (x1 * x4)) + (x2 * x5)) + (x3 * x6)) - (x5 * x6)) + (x1 * (((((-(x1) + x2) + x3) - x4) + x5) + x6)))
  // }

  def doppler1(u: Real, v: Real, T: Real): Real = {
    require(((-100.0 <= u) && (u <= 100.0) && (20.0 <= v) && (v <= 20000.0) && (-30.0 <= T) && (T <= 50.0)))
    val t1: Real = (331.4 + (0.6 * T))
    ((-(t1) * v) / ((t1 + u) * (t1 + u)))
  }

  def doppler2(u: Real, v: Real, T: Real): Real = {
    require(((-125.0 <= u) && (u <= 125.0) && (15.0 <= v) && (v <= 25000.0) && (-40.0 <= T) && (T <= 60.0)))
    val t1: Real = (331.4 + (0.6 * T))
    ((-(t1) * v) / ((t1 + u) * (t1 + u)))
  }

  def doppler3(u: Real, v: Real, T: Real): Real = {
    require(((-30.0 <= u) && (u <= 120.0) && (320.0 <= v) && (v <= 20300.0) && (-50.0 <= T) && (T <= 30.0)))
    val t1: Real = (331.4 + (0.6 * T))
    ((-(t1) * v) / ((t1 + u) * (t1 + u)))
  }

  // def exp1x_32(x: Real): Real = {
  //   require(((0.01 <= x) && (x <= 0.5)))
  //   ((exp(x) - 1.0) / x)
  // }

  // def exp1x_log(x: Real): Real = {
  //   require(((0.01 <= x) && (x <= 0.5)))
  //   ((exp(x) - 1.0) / log(exp(x)))
  // }

  // def exp1x(x: Real): Real = {
  //   require(((0.01 <= x) && (x <= 0.5)))
  //   ((exp(x) - 1.0) / x)
  // }

  // def floudas(x1: Real, x2: Real): Real = {
  //   require(((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && ((x1 + x2) <= 2.0)))
  //   (x1 + x2)
  // }

  // def floudas1(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
  //   require(((0.0 <= x1) && (x1 <= 6.0) && (0.0 <= x2) && (x2 <= 6.0) && (1.0 <= x3) && (x3 <= 5.0) && (0.0 <= x4) && (x4 <= 6.0) && (0.0 <= x5) && (x5 <= 6.0) && (0.0 <= x6) && (x6 <= 10.0) && (((((x3 - 3.0) * (x3 - 3.0)) + x4) - 4.0) >= 0.0) && (((((x5 - 3.0) * (x5 - 3.0)) + x6) - 4.0) >= 0.0) && (((2.0 - x1) + (3.0 * x2)) >= 0.0) && (((2.0 + x1) - x2) >= 0.0) && (((6.0 - x1) - x2) >= 0.0) && (((x1 + x2) - 2.0) >= 0.0)))
  //   ((((((-25.0 * ((x1 - 2.0) * (x1 - 2.0))) - ((x2 - 2.0) * (x2 - 2.0))) - ((x3 - 1.0) * (x3 - 1.0))) - ((x4 - 4.0) * (x4 - 4.0))) - ((x5 - 1.0) * (x5 - 1.0))) - ((x6 - 4.0) * (x6 - 4.0)))
  // }

  // def floudas2(x1: Real, x2: Real): Real = {
  //   require(((0.0 <= x1) && (x1 <= 3.0) && (0.0 <= x2) && (x2 <= 4.0) && (((((2.0 * ((x1 * x1) * (x1 * x1))) - ((8.0 * (x1 * x1)) * x1)) + ((8.0 * x1) * x1)) - x2) >= 0.0) && (((((((4.0 * ((x1 * x1) * (x1 * x1))) - ((32.0 * (x1 * x1)) * x1)) + ((88.0 * x1) * x1)) - (96.0 * x1)) + 36.0) - x2) >= 0.0)))
  //   (-(x1) - x2)
  // }

  // def floudas3(x1: Real, x2: Real): Real = {
  //   require(((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && (((-2.0 * ((x1 * x1) * (x1 * x1))) + 2.0) >= x2)))
  //   (((-12.0 * x1) - (7.0 * x2)) + (x2 * x2))
  // }


  // def hartman3(x1: Real, x2: Real, x3: Real): Real = {
  //   require(((0.0 <= x1) && (x1 <= 1.0) && (0.0 <= x2) && (x2 <= 1.0) && (0.0 <= x3) && (x3 <= 1.0)))
  //   val e1: Real = (((3.0 * ((x1 - 0.3689) * (x1 - 0.3689))) + (10.0 * ((x2 - 0.117) * (x2 - 0.117)))) + (30.0 * ((x3 - 0.2673) * (x3 - 0.2673))))
  //   val e2: Real = (((0.1 * ((x1 - 0.4699) * (x1 - 0.4699))) + (10.0 * ((x2 - 0.4387) * (x2 - 0.4387)))) + (35.0 * ((x3 - 0.747) * (x3 - 0.747))))
  //   val e3: Real = (((3.0 * ((x1 - 0.1091) * (x1 - 0.1091))) + (10.0 * ((x2 - 0.8732) * (x2 - 0.8732)))) + (30.0 * ((x3 - 0.5547) * (x3 - 0.5547))))
  //   val e4: Real = (((0.1 * ((x1 - 0.03815) * (x1 - 0.03815))) + (10.0 * ((x2 - 0.5743) * (x2 - 0.5743)))) + (35.0 * ((x3 - 0.8828) * (x3 - 0.8828))))
  //   val exp1: Real = exp(-(e1))
  //   val exp2: Real = exp(-(e2))
  //   val exp3: Real = exp(-(e3))
  //   val exp4: Real = exp(-(e4))
  //   -(((((1.0 * exp1) + (1.2 * exp2)) + (3.0 * exp3)) + (3.2 * exp4)))
  // }

  // def hartman6(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
  //   require(((0.0 <= x1) && (x1 <= 1.0) && (0.0 <= x2) && (x2 <= 1.0) && (0.0 <= x3) && (x3 <= 1.0) && (0.0 <= x4) && (x4 <= 1.0) && (0.0 <= x5) && (x5 <= 1.0) && (0.0 <= x6) && (x6 <= 1.0)))
  //   val e1: Real = ((((((10.0 * ((x1 - 0.1312) * (x1 - 0.1312))) + (3.0 * ((x2 - 0.1696) * (x2 - 0.1696)))) + (17.0 * ((x3 - 0.5569) * (x3 - 0.5569)))) + (3.5 * ((x4 - 0.0124) * (x4 - 0.0124)))) + (1.7 * ((x5 - 0.8283) * (x5 - 0.8283)))) + (8.0 * ((x6 - 0.5886) * (x6 - 0.5886))))
  //   val e2: Real = ((((((0.05 * ((x1 - 0.2329) * (x1 - 0.2329))) + (10.0 * ((x2 - 0.4135) * (x2 - 0.4135)))) + (17.0 * ((x3 - 0.8307) * (x3 - 0.8307)))) + (0.1 * ((x4 - 0.3736) * (x4 - 0.3736)))) + (8.0 * ((x5 - 0.1004) * (x5 - 0.1004)))) + (14.0 * ((x6 - 0.9991) * (x6 - 0.9991))))
  //   val e3: Real = ((((((3.0 * ((x1 - 0.2348) * (x1 - 0.2348))) + (3.5 * ((x2 - 0.1451) * (x2 - 0.1451)))) + (1.7 * ((x3 - 0.3522) * (x3 - 0.3522)))) + (10.0 * ((x4 - 0.2883) * (x4 - 0.2883)))) + (17.0 * ((x5 - 0.3047) * (x5 - 0.3047)))) + (8.0 * ((x6 - 0.665) * (x6 - 0.665))))
  //   val e4: Real = ((((((17.0 * ((x1 - 0.4047) * (x1 - 0.4047))) + (8.0 * ((x2 - 0.8828) * (x2 - 0.8828)))) + (0.05 * ((x3 - 0.8732) * (x3 - 0.8732)))) + (10.0 * ((x4 - 0.5743) * (x4 - 0.5743)))) + (0.1 * ((x5 - 0.1091) * (x5 - 0.1091)))) + (14.0 * ((x6 - 0.0381) * (x6 - 0.0381))))
  //   val exp1: Real = exp(-(e1))
  //   val exp2: Real = exp(-(e2))
  //   val exp3: Real = exp(-(e3))
  //   val exp4: Real = exp(-(e4))
  //   -(((((1.0 * exp1) + (1.2 * exp2)) + (3.0 * exp3)) + (3.2 * exp4)))
  // }

  // def himmilbeau(x1: Real, x2: Real): Real = {
  //   require(((-5.0 <= x1) && (x1 <= 5.0) && (-5.0 <= x2) && (x2 <= 5.0)))
  //   val a: Real = (((x1 * x1) + x2) - 11.0)
  //   val b: Real = ((x1 + (x2 * x2)) - 7.0)
  //   ((a * a) + (b * b))
  // }

  // def hypot(x1: Real, x2: Real): Real = {
  //   require(((1.0 <= x1) && (x1 <= 100.0) && (1.0 <= x2) && (x2 <= 100.0)))
  //   sqrt(((x1 * x1) + (x2 * x2)))
  // }

  // def hypot32(x1: Real, x2: Real): Real = {
  //   require(((1.0 <= x1) && (x1 <= 100.0) && (1.0 <= x2) && (x2 <= 100.0)))
  //   sqrt(((x1 * x1) + (x2 * x2)))
  // }

  // def i4modified(x: Real, y: Real): Real = {
  //   require(((0.1 <= x) && (x <= 10.0) && (0.0 <= y) && (y <= 5.0)))
  //   sqrt((x + (y * y)))
  // }

  // def i6(x: Real, y: Real): Real = {
  //   require(((0.1 <= x) && (x <= 10.0) && (-5.0 <= y) && (y <= 5.0)))
  //   sin((x * y))
  // }

  // def instantaneousCurrent(t: Real, resistance: Real, frequency: Real, inductance: Real, maxVoltage: Real): Real = {
  //   require(((0.0 <= t) && (t <= 300.0) && (1.0 <= resistance) && (resistance <= 50.0) && (1.0 <= frequency) && (frequency <= 100.0) && (0.001 <= inductance) && (inductance <= 0.004) && (1.0 <= maxVoltage) && (maxVoltage <= 12.0)))
  //   val pi: Real = 3.14159265359
  //   val impedance_re: Real = resistance
  //   val impedance_im: Real = (((2.0 * pi) * frequency) * inductance)
  //   val denom: Real = ((impedance_re * impedance_re) + (impedance_im * impedance_im))
  //   val current_re: Real = ((maxVoltage * impedance_re) / denom)
  //   val current_im: Real = (-((maxVoltage * impedance_im)) / denom)
  //   val maxCurrent: Real = sqrt(((current_re * current_re) + (current_im * current_im)))
  //   val theta: Real = atan((current_im / current_re))
  //   (maxCurrent * cos(((((2.0 * pi) * frequency) * t) + theta)))
  // }

  // def intro_example(t: Real): Real = {
  //   require(((0.0 <= t) && (t <= 999.0)))
  //   (t / (t + 1.0))
  // }

  def jacobisMethodX1(a11: Real, b1: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(((0.001 < a11) && (a11 < 10.0) && (0.001 < b1) && (b1 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
    val x_n1: Real = ((((b1 / a11) - ((0.1 / a11) * x2)) - ((0.2 / a11) * x3)) + ((0.3 / a11) * x4))
    x_n1
  }

  // def jacobisMethodX2(a22: Real, b2: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
  //   require(((0.001 < a22) && (a22 < 10.0) && (0.001 < b2) && (b2 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
  //   val x_n2: Real = ((((b2 / a22) - ((0.3 / a22) * x1)) + ((0.1 / a22) * x3)) - ((0.2 / a22) * x4))
  //   x_n2
  // }

  // def jacobisMethodX3(a33: Real, b3: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
  //   require(((0.001 < a33) && (a33 < 10.0) && (0.001 < b3) && (b3 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
  //   val x_n3: Real = ((((b3 / a33) - ((0.2 / a33) * x1)) + ((0.3 / a33) * x2)) - ((0.1 / a33) * x4))
  //   x_n3
  // }

  // def jacobisMethodX4(a44: Real, b4: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
  //   require(((0.001 < a44) && (a44 < 10.0) && (0.001 < b4) && (b4 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
  //   val x_n4: Real = ((((b4 / a44) + ((0.1 / a44) * x1)) - ((0.2 / a44) * x2)) - ((0.3 / a44) * x3))
  //   x_n4
  // }

  // def jetEngine(x1: Real, x2: Real): Real = {
  //   require(((-5.0 <= x1) && (x1 <= 5.0) && (-20.0 <= x2) && (x2 <= 5.0)))
  //   val t: Real = ((((3.0 * x1) * x1) + (2.0 * x2)) - x1)
  //   val t2: Real = ((((3.0 * x1) * x1) - (2.0 * x2)) - x1)
  //   val d: Real = ((x1 * x1) + 1.0)
  //   val s: Real = (t / d)
  //   val s2: Real = (t2 / d)
  //   (x1 + (((((((((2.0 * x1) * s) * (s - 3.0)) + ((x1 * x1) * ((4.0 * s) - 6.0))) * d) + (((3.0 * x1) * x1) * s)) + ((x1 * x1) * x1)) + x1) + (3.0 * s2)))
  // } // warning dataflow

  // def jetEngineModified(x1: Real, x2: Real): Real = {
  //   require(((0.0 <= x1) && (x1 <= 5.0) && (-20.0 <= x2) && (x2 <= 5.0)))
  //   val t: Real = ((((3.0 * x1) * x1) + (2.0 * x2)) - x1)
  //   val t2: Real = ((((3.0 * x1) * x1) - (2.0 * x2)) - x1)
  //   val d: Real = ((x1 * x1) + 1.0)
  //   val s: Real = (t / d)
  //   val s2: Real = (t2 / d)
  //   (x1 + (((((((((2.0 * x1) * s) * (s - 3.0)) + ((x1 * x1) * ((4.0 * s) - 6.0))) * d) + (((3.0 * x1) * x1) * s)) + ((x1 * x1) * x1)) + x1) + (3.0 * s2)))
  // }

  def kepler0(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36) && (4.0 <= x5) && (x5 <= 6.36) && (4.0 <= x6) && (x6 <= 6.36)))
    (((((x2 * x5) + (x3 * x6)) - (x2 * x3)) - (x5 * x6)) + (x1 * (((((-(x1) + x2) + x3) - x4) + x5) + x6)))
  }

  def kepler1(x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36)))
    ((((((((x1 * x4) * (((-(x1) + x2) + x3) - x4)) + (x2 * (((x1 - x2) + x3) + x4))) + (x3 * (((x1 + x2) - x3) + x4))) - ((x2 * x3) * x4)) - (x1 * x3)) - (x1 * x2)) - x4)
  }

  def kepler2(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36) && (4.0 <= x5) && (x5 <= 6.36) && (4.0 <= x6) && (x6 <= 6.36)))
    ((((((((x1 * x4) * (((((-(x1) + x2) + x3) - x4) + x5) + x6)) + ((x2 * x5) * (((((x1 - x2) + x3) + x4) - x5) + x6))) + ((x3 * x6) * (((((x1 + x2) - x3) + x4) + x5) - x6))) - ((x2 * x3) * x4)) - ((x1 * x3) * x5)) - ((x1 * x2) * x6)) - ((x4 * x5) * x6))
  }

  // def logexp(x: Real): Real = {
  //   require(((-8.0 <= x) && (x <= 8.0)))
  //   val e: Real = exp(x)
  //   log((1.0 + e))
  // }

  // def matrixDeterminant(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
  //   require(((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0)))
  //   (((((a * e) * i) + ((b * f) * g)) + ((c * d) * h)) - ((((c * e) * g) + ((b * d) * i)) + ((a * f) * h)))
  // }

  // def matrixDeterminant2(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
  //   require(((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0)))
  //   (((a * (e * i)) + ((g * (b * f)) + (c * (d * h)))) - ((e * (c * g)) + ((i * (b * d)) + (a * (f * h)))))
  // }

  // def matrixDeterminant2modified(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
  //   require(((5.0 <= a) && (a <= 50.0) && (5.0 <= b) && (b <= 50.0) && (2.0 <= c) && (c <= 12.0) && (0.0 <= d) && (d <= 5.0) && (-10.0 <= e) && (e <= -1.0) && (3.0 <= f) && (f <= 42.0) && (-10.0 <= g) && (g <= 10.0) && (-0.5 <= h) && (h <= 1.5) && (-11.0 <= i) && (i <= -8.0)))
  //   (((a * (e * i)) + ((g * (b * f)) + (c * (d * h)))) - ((e * (c * g)) + ((i * (b * d)) + (a * (f * h)))))
  // }

  // def n_bodyXmodified(x0: Real, y0: Real, z0: Real, vx0: Real, vy0: Real, vz0: Real): Real = {
  //   require(((0.001 < x0) && (x0 < 6.0) && (0.001 < y0) && (y0 < 6.0) && (0.001 < z0) && (z0 < 0.2) && (-3.0 < vx0) && (vx0 < 3.0) && (-3.0 < vy0) && (vy0 < 3.0) && (-0.1 < vz0) && (vz0 < 0.1)))
  //   val dt: Real = 0.1
  //   val solarMass: Real = 39.47841760435743
  //   val distance: Real = sqrt((((x0 * x0) + (y0 * y0)) + (z0 * z0)))
  //   val mag: Real = (dt / ((distance * distance) * distance))
  //   val vxNew: Real = (vx0 - ((x0 * solarMass) * mag))
  //   val x_2: Real = (x0 + (dt * vxNew))
  //   x_2
  // }

  // def n_bodyZmodified(x0: Real, y0: Real, z0: Real, vx0: Real, vy0: Real, vz0: Real): Real = {
  //   require(((0.001 < x0) && (x0 < 6.0) && (0.001 < y0) && (y0 < 6.0) && (0.001 < z0) && (z0 < 0.2) && (-3.0 < vx0) && (vx0 < 3.0) && (-3.0 < vy0) && (vy0 < 3.0) && (-0.1 < vz0) && (vz0 < 0.1)))
  //   val dt: Real = 0.1
  //   val solarMass: Real = 39.47841760435743
  //   val distance: Real = sqrt((((x0 * x0) + (y0 * y0)) + (z0 * z0)))
  //   val mag: Real = (dt / ((distance * distance) * distance))
  //   val vzNew: Real = (vz0 - ((z0 * solarMass) * mag))
  //   val z_2: Real = (z0 + (dt * vzNew))
  //   z_2
  // }

  // def nmse_example_3_1(x: Real): Real = {
  //   require(((x >= 0.001) && (x <= 10.0)))
  //   (sqrt((x + 1.0)) - sqrt(x))
  // }

  // def nmse_example_3_3(x: Real, eps: Real): Real = {
  //   require(((x >= 0.0) && (x <= 10.0) && (eps >= -1.0) && (eps <= 1.0)))
  //   (sin((x + eps)) - sin(x))
  // }

  // def nmse_example_3_4(x: Real): Real = {
  //   require(((x >= 0.1) && (x <= 3.0)))
  //   ((1.0 - cos(x)) / sin(x))
  // }

  // def nmse_example_3_5(N: Real): Real = {
  //   require(((N >= -1.0) && (N <= 1.0)))
  //   (atan((N + 1.0)) - atan(N))
  // }

  // def nmse_example_3_6(x: Real): Real = {
  //   require(((x >= 0.001) && (x <= 10.0)))
  //   ((1.0 / sqrt(x)) - (1.0 / sqrt((x + 1.0))))
  // }

  // def nmse_example_3_7(x: Real): Real = {
  //   require(((x >= -10.0) && (x <= 10.0)))
  //   (exp(x) - 1.0)
  // }

  // def nmse_example_3_8(N: Real): Real = {
  //   require(((N >= 0.0001) && (N <= 10.0)))
  //   ((((N + 1.0) * log((N + 1.0))) - (N * log(N))) - 1.0)
  // }

  // def nmse_example_3_9(x: Real): Real = {
  //   require(((x >= 0.001) && (x <= 1.5)))
  //   ((1.0 / x) - (1.0 / tan(x)))
  // }

  // def nmse_example_3_10(x: Real): Real = {
  //   require(((0.001 < x) && (x < 0.9999)))
  //   (log((1.0 - x)) / log((1.0 + x)))
  // }


  // def nmse_p42_positive(a: Real, b: Real, c: Real): Real = {
  //   require(((a >= 0.001) && (a <= 10.0) && (b >= 70.0) && (b <= 120.0) && (c >= -10.0) && (c <= 10.0) && ((b * b) >= (4.0 * (a * c))) && (a != 0.0)))
  //   ((-(b) + sqrt(((b * b) - (4.0 * (a * c))))) / (2.0 * a))
  // }

  // def nmse_problem_3_2_1_negative(a: Real, b2: Real, c: Real): Real = {
  //   require(((a >= 0.001) && (a <= 10.0) && (b2 >= 20.0) && (b2 <= 30.0) && (c >= -10.0) && (c <= 10.0) && ((b2 * b2) >= (a * c)) && (a != 0.0)))
  //   ((-(b2) - sqrt(((b2 * b2) - (a * c)))) / a)
  // }

  // def nmse_problem_3_2_1_positive(a: Real, b2: Real, c: Real): Real = {
  //   require(((a >= 0.001) && (a <= 10.0) && (b2 >= 20.0) && (b2 <= 30.0) && (c >= -10.0) && (c <= 10.0) && ((b2 * b2) >= (a * c)) && (a != 0.0)))
  //   ((-(b2) + sqrt(((b2 * b2) - (a * c)))) / a)
  // }

  // def nmse_problem_3_3_1(x: Real): Real = {
  //   require(((x >= 0.001) && (x <= 1.5)))
  //   ((1.0 / (x + 1.0)) - (1.0 / x))
  // }

  // def nmse_problem_3_3_2(x: Real, eps: Real): Real = {
  //   require(((x >= -1.5) && (x <= 1.5) && (eps >= -0.01) && (eps <= 0.01)))
  //   (tan((x + eps)) - tan(x))
  // }

  // def nmse_problem_3_3_3(x: Real): Real = {
  //   require(((x >= 0.0001) && (x <= 0.9999)))
  //   (((1.0 / (x + 1.0)) - (2.0 / x)) + (1.0 / (x - 1.0)))
  // }

  // def nmse_problem_3_3_5(x: Real, eps: Real): Real = {
  //   require(((x >= -10.0) && (x <= 10.0) && (eps >= -1.0) && (eps <= 1.0)))
  //   (cos((x + eps)) - cos(x))
  // }

  // def nmse_problem_3_3_6(N: Real): Real = {
  //   require(((N >= 0.0001) && (N <= 10.0)))
  //   (log((N + 1.0)) - log(N))
  // }

  // def nmse_problem_3_3_7(x: Real): Real = {
  //   require(((x >= -10.0) && (x <= 10.0)))
  //   ((exp(x) - 2.0) + exp(-(x)))
  // }

  // def nmse_problem_3_4_1(x: Real): Real = {
  //   require(((x >= 0.1) && (x <= 10.0)))
  //   ((1.0 - cos(x)) / (x * x))
  // }

  // def nmse_problem_3_4_2(a: Real, b: Real, eps: Real): Real = {
  //   require(((a >= 5.0) && (a <= 15.0) && (b >= -15.0) && (b <= -5.0) && (eps >= 0.9) && (eps <= 1.1)))
  //   ((eps * (exp(((a + b) * eps)) - 1.0)) / ((exp((a * eps)) - 1.0) * (exp((b * eps)) - 1.0)))
  // }

  // def nmse_problem_3_4_3(eps: Real): Real = {
  //   require(((-0.9999 < eps) && (eps < 0.9999)))
  //   log(((1.0 - eps) / (1.0 + eps)))
  // }

  // def nmse_section_3_5(a: Real, x: Real): Real = {
  //   require(((a >= -10.0) && (a <= 10.0) && (x >= -10.0) && (x <= 10.0)))
  //   (exp((a * x)) - 1.0)
  // }

  // def nmse_section_3_11(x: Real): Real = {
  //   require(((x >= 0.1) && (x <= 10.0)))
  //   (exp(x) / (exp(x) - 1.0))
  // }

  // def nonlin1(z: Real): Real = {
  //   require(((0.0 <= z) && (z <= 999.0)))
  //   (z / (z + 1.0))
  // }

  // def nonlin2(x: Real, y: Real): Real = {
  //   require(((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0)))
  //   val t: Real = (x * y)
  //   ((t - 1.0) / ((t * t) - 1.0))
  // }

  // def pendulum(t0: Real, w0: Real): Real = {
  //   require(((-2.0 < t0) && (t0 < 2.0) && (-5.0 < w0) && (w0 < 5.0)))
  //   val h: Real = 0.01
  //   val L: Real = 2.0
  //   val m: Real = 1.5
  //   val g: Real = 9.80665
  //   val t_1: Real = t0
  //   val w_1: Real = w0
  //   val n: Real = 0.0
  //   val k1w: Real = ((-(g) / L) * sin(t_1))
  //   val k2t: Real = (w_1 + ((h / 2.0) * k1w))
  //   val t_3: Real = (t_1 + (h * k2t))
  //   t_3
  // }

  // def pid(m: Real, c: Real): Real = {
  //   require(((-10.0 < m) && (m < 10.0) && (-10.0 < c) && (c < 10.0)))
  //   val ki: Real = 0.69006
  //   val kp: Real = 9.4514
  //   val kd: Real = 2.8454
  //   val i_0: Real = 0.0
  //   val dt: Real = 0.2
  //   val invdt: Real = 5.0
  //   val eold: Real = 0.0
  //   val e_1: Real = (c - m)
  //   val p_1: Real = (kp * e_1)
  //   val i_1: Real = (i_0 + ((ki * dt) * e_1))
  //   val d_1: Real = ((kd * invdt) * (e_1 - eold))
  //   val r_1: Real = ((p_1 + i_1) + d_1)
  //   (m + (0.01 * r_1))
  // }

  // def polarToCarthesian_x(radius: Real, theta: Real): Real = {
  //   require(((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0)))
  //   val pi: Real = 3.14159265359
  //   val radiant: Real = (theta * (pi / 180.0))
  //   (radius * cos(radiant))
  // }

  // def polarToCarthesian_y(radius: Real, theta: Real): Real = {
  //   require(((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0)))
  //   val pi: Real = 3.14159265359
  //   val radiant: Real = (theta * (pi / 180.0))
  //   (radius * sin(radiant))
  // }

  def predatorPrey(x: Real): Real = {
    require(((0.1 <= x) && (x <= 0.3)))
    val r: Real = 4.0
    val K: Real = 1.11
    (((r * x) * x) / (1.0 + ((x / K) * (x / K))))
  }

  def rigidBody1(x1: Real, x2: Real, x3: Real): Real = {
    require(((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0)))
    (((-((x1 * x2)) - ((2.0 * x2) * x3)) - x1) - x3)
  }

  def rigidBody2(x1: Real, x2: Real, x3: Real): Real = {
    require(((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0)))
    (((((((2.0 * x1) * x2) * x3) + ((3.0 * x3) * x3)) - (((x2 * x1) * x2) * x3)) + ((3.0 * x3) * x3)) - x2)
  }

  // def rump_from_C(a: Real, b: Real): Real = {
  //   require(((70000.0 <= a) && (a <= 80000.0) && (30000.0 <= b) && (b <= 34000.0)))
  //   val b2: Real = (b * b)
  //   val b4: Real = (b2 * b2)
  //   val b6: Real = (b4 * b2)
  //   val b8: Real = (b4 * b4)
  //   val a2: Real = (a * a)
  //   val firstexpr: Real = (((((11.0 * a2) * b2) - b6) - (121.0 * b4)) - 2.0)
  //   ((((333.75 * b6) + (a2 * firstexpr)) + (5.5 * b8)) + (a / (2.0 * b)))
  // }

  // def rump_revisited(a: Real, b: Real): Real = {
  //   require(((70000.0 <= a) && (a <= 80000.0) && (30000.0 <= b) && (b <= 34000.0)))
  //   val b2: Real = (b * b)
  //   val b4: Real = (b2 * b2)
  //   val b6: Real = (b4 * b2)
  //   val b8: Real = (b4 * b4)
  //   val a2: Real = (a * a)
  //   val firstexpr: Real = ((((11.0 * a2) * b2) - (121.0 * b4)) - 2.0)
  //   (((((333.75 - a2) * b6) + (a2 * firstexpr)) + (5.5 * b8)) + (a / (2.0 * b)))
  // }

  // def rump_with_pow(a: Real, b: Real): Real = {
  //   require(((70000.0 <= a) && (a <= 80000.0) && (30000.0 <= b) && (b <= 34000.0)))
  //   ((((333.75 * (((((b * b) * b) * b) * b) * b)) + ((a * a) * (((((11.0 * (a * a)) * (b * b)) - (((((b * b) * b) * b) * b) * b)) - (121.0 * (((b * b) * b) * b))) - 2.0))) + (5.5 * (((((((b * b) * b) * b) * b) * b) * b) * b))) + (a / (2.0 * b)))
  // }

  // def runge_kutta_4(h: Real, y_n: Real, c: Real): Real = {
  //   require(((0.0 <= y_n) && (y_n <= 100.0) && (1.0e-05 < h) && (h < 0.1) && (50.0 < c) && (c < 200.0)))
  //   val sixieme: Real = 0.0
  //   val eps: Real = 0.005
  //   val k: Real = 1.2
  //   val v_1: Real = (c - y_n)
  //   val k1: Real = ((k * v_1) * v_1)
  //   val v_2: Real = (c - (y_n + ((0.5 * h) * k1)))
  //   val k2: Real = ((k * v_2) * v_2)
  //   val v_3: Real = (c - (y_n + ((0.5 * h) * k2)))
  //   val k3: Real = ((k * v_3) * v_3)
  //   val v_4: Real = (c - (y_n + (h * k3)))
  //   val k4: Real = ((k * v_4) * v_4)
  //   (y_n + ((sixieme * h) * (((k1 + (2.0 * k2)) + (2.0 * k3)) + k4)))
  // }

  // def sec4_example(x: Real, y: Real): Real = {
  //   require(((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0)))
  //   val t: Real = (x * y)
  //   ((t - 1.0) / ((t * t) - 1.0))
  // }

  // def sine_newton(x0: Real): Real = {
  //   require(((-1.0 < x0) && (x0 < 1.0)))
  //   val x_2: Real = (x0 - ((((x0 - (((x0 * x0) * x0) / 6.0)) + (((((x0 * x0) * x0) * x0) * x0) / 120.0)) + (((((((x0 * x0) * x0) * x0) * x0) * x0) * x0) / 5040.0)) / (((1.0 - ((x0 * x0) / 2.0)) + ((((x0 * x0) * x0) * x0) / 24.0)) + ((((((x0 * x0) * x0) * x0) * x0) * x0) / 720.0))))
  //   x_2
  // }

  // def sine(x: Real): Real = {
  //   require(((-1.57079632679 < x) && (x < 1.57079632679)))
  //   (((x - (((x * x) * x) / 6.0)) + (((((x * x) * x) * x) * x) / 120.0)) - (((((((x * x) * x) * x) * x) * x) * x) / 5040.0))
  // }

  // def sineOrder3(x: Real): Real = {
  //   require(((-2.0 < x) && (x < 2.0)))
  //   ((0.954929658551372 * x) - (0.12900613773279798 * ((x * x) * x)))
  // }

  // def sphere(x: Real, r: Real, lat: Real, lon: Real): Real = {
  //   require(((-10.0 <= x) && (x <= 10.0) && (0.0 <= r) && (r <= 10.0) && (-1.570796 <= lat) && (lat <= 1.570796) && (-3.14159265 <= lon) && (lon <= 3.14159265)))
  //   val sinLat: Real = sin(lat)
  //   val cosLon: Real = cos(lon)
  //   (x + ((r * sinLat) * cosLon))
  // }

  // def sqroot(x: Real): Real = {
  //   require(((0.0 <= x) && (x <= 1.0)))
  //   ((((1.0 + (0.5 * x)) - ((0.125 * x) * x)) + (((0.0625 * x) * x) * x)) - ((((0.0390625 * x) * x) * x) * x))
  // }

  // def sqrt_add(x: Real): Real = {
  //   require(((1.0 <= x) && (x <= 1000.0)))
  //   (1.0 / (sqrt((x + 1.0)) + sqrt(x)))
  // }

  // def squareRoot3(x: Real): Real = {
  //   require(((0.0 < x) && (x < 10.0)))
  //   val temp: Real = if ((x < 1.0e-05)) {
  //     (1.0 + (0.5 * x))
  //   }
  //   else {
  //     sqrt((1.0 + x))
  //   }
  //   temp
  // }

  // def squareRoot3Invalid(x: Real): Real = {
  //   require(((0.0 < x) && (x < 10.0)))
  //   val temp: Real = if ((x < 0.0001)) {
  //     (1.0 + (0.5 * x))
  //   }
  //   else {
  //     sqrt((1.0 + x))
  //   }
  //   temp
  // }

  // def sum(x0: Real, x1: Real, x2: Real): Real = {
  //   require(((1.0 <= x0) && (x0 <= 2.0) && (1.0 <= x1) && (x1 <= 2.0) && (1.0 <= x2) && (x2 <= 2.0)))
  //   val p0: Real = ((x0 + x1) - x2)
  //   val p1: Real = ((x1 + x2) - x0)
  //   val p2: Real = ((x2 + x0) - x1)
  //   ((p0 + p1) + p2)
  // }

  // def test01_sum3(x0: Real, x1: Real, x2: Real): Real = {
  //   require(((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0)))
  //   val p0: Real = ((x0 + x1) - x2)
  //   val p1: Real = ((x1 + x2) - x0)
  //   val p2: Real = ((x2 + x0) - x1)
  //   ((p0 + p1) + p2)
  // }

  // def test02_sum8(x0: Real, x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real, x7: Real): Real = {
  //   require(((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0) && (1.0 < x3) && (x3 < 2.0) && (1.0 < x4) && (x4 < 2.0) && (1.0 < x5) && (x5 < 2.0) && (1.0 < x6) && (x6 < 2.0) && (1.0 < x7) && (x7 < 2.0)))
  //   (((((((x0 + x1) + x2) + x3) + x4) + x5) + x6) + x7)
  // }

  // def test03_nonlin2(x: Real, y: Real): Real = {
  //   require(((0.0 < x) && (x < 1.0) && (-1.0 < y) && (y < -0.1)))
  //   ((x + y) / (x - y))
  // }

  // def test04_dqmom9(m0: Real, m1: Real, m2: Real, w0: Real, w1: Real, w2: Real, a0: Real, a1: Real, a2: Real): Real = {
  //   require(((-1.0 < m0) && (m0 < 1.0) && (-1.0 < m1) && (m1 < 1.0) && (-1.0 < m2) && (m2 < 1.0) && (1.0e-05 < w0) && (w0 < 1.0) && (1.0e-05 < w1) && (w1 < 1.0) && (1.0e-05 < w2) && (w2 < 1.0) && (1.0e-05 < a0) && (a0 < 1.0) && (1.0e-05 < a1) && (a1 < 1.0) && (1.0e-05 < a2) && (a2 < 1.0)))
  //   val v2: Real = ((w2 * (0.0 - m2)) * (-3.0 * ((1.0 * (a2 / w2)) * (a2 / w2))))
  //   val v1: Real = ((w1 * (0.0 - m1)) * (-3.0 * ((1.0 * (a1 / w1)) * (a1 / w1))))
  //   val v0: Real = ((w0 * (0.0 - m0)) * (-3.0 * ((1.0 * (a0 / w0)) * (a0 / w0))))
  //   (0.0 + ((v0 * 1.0) + ((v1 * 1.0) + ((v2 * 1.0) + 0.0))))
  // }

  // def test05_nonlin1_r4(x: Real): Real = {
  //   require(((1.00001 < x) && (x < 2.0)))
  //   val r1: Real = (x - 1.0)
  //   val r2: Real = (x * x)
  //   (r1 / (r2 - 1.0))
  // }

  // def test05_nonlin1_test2(x: Real): Real = {
  //   require(((1.00001 < x) && (x < 2.0)))
  //   (1.0 / (x + 1.0))
  // }

  // def test06_sums4_sum1(x0: Real, x1: Real, x2: Real, x3: Real): Real = {
  //   require(((-1.0e-05 < x0) && (x0 < 1.00001) && (0.0 < x1) && (x1 < 1.0) && (0.0 < x2) && (x2 < 1.0) && (0.0 < x3) && (x3 < 1.0)))
  //   (((x0 + x1) + x2) + x3)
  // }

  // def test06_sums4_sum2(x0: Real, x1: Real, x2: Real, x3: Real): Real = {
  //   require(((-1.0e-05 < x0) && (x0 < 1.00001) && (0.0 < x1) && (x1 < 1.0) && (0.0 < x2) && (x2 < 1.0) && (0.0 < x3) && (x3 < 1.0)))
  //   ((x0 + x1) + (x2 + x3))
  // }

  def turbine1(v: Real, w: Real, r: Real): Real = {
    require(((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)))
    (((3.0 + (2.0 / (r * r))) - (((0.125 * (3.0 - (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 4.5)
  }

  def turbine2(v: Real, w: Real, r: Real): Real = {
    require(((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)))
    (((6.0 * v) - (((0.5 * v) * (((w * w) * r) * r)) / (1.0 - v))) - 2.5)
  }

  def turbine3(v: Real, w: Real, r: Real): Real = {
    require(((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)))
    (((3.0 - (2.0 / (r * r))) - (((0.125 * (1.0 + (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 0.5)
  }

  def verhulst(x: Real): Real = {
    require(((0.1 <= x) && (x <= 0.3)))
    val r: Real = 4.0
    val K: Real = 1.11
    ((r * x) / (1.0 + (x / K)))
  }

  // def x_by_xy(x: Real, y: Real): Real = {
  //   require(((1.0 <= x) && (x <= 4.0) && (1.0 <= y) && (y <= 4.0)))
  //   (x / (x + y))
  // }

}
