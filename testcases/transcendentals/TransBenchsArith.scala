import daisy.lang._
import Real._


// some benchmarks from http://www-sop.inria.fr/coprin/logiciels/ALIAS/Benches/node2.html#SECTION00021000000000000000
object TranscendentalBenchmarks {

  def xu1(x1: Real, x2: Real): Real = {
    require(-3.14 <= x1 && x1 <= 3.14 && -3.14 <= x2 && x2 <= 3.14)

    2 * sin(x1) + 0.8 * cos(2 * x1) + 7 * sin(x2) - x1
  }

  def xu1Arith(x1: Real, x2: Real): Real = {
    require(-3.14 <= x1 && x1 <= 3.14 && -3.14 <= x2 && x2 <= 3.14)

    2 * x1 + 0.8 * (2 * x1) + 7 * x2 - x1
  }

  def xu2(x1: Real, x2: Real): Real = {
    require(-3.14 <= x1 && x1 <= 3.14 && -3.14 <= x2 && x2 <= 3.14)

    1.4 * sin(3 * x2) + 3.1 * cos(2 * x2) - x2 + 4 * sin(2 * x1)
  }

  def xu2Arith(x1: Real, x2: Real): Real = {
    require(-3.14 <= x1 && x1 <= 3.14 && -3.14 <= x2 && x2 <= 3.14)

    1.4 * (3 * x2) + 3.1 * (2 * x2) - x2 + 4 * (2 * x1)
  }

  def integrate18257(x: Real): Real = {
    require(0 <= x && x <= 3.14)

    exp(cos(x)) * cos(x - sin(x))
  }

  def integrate18257Arith(x: Real): Real = {
    require(0 <= x && x <= 3.14)

    (x*x) * (x - x)
  }

  def integrateStoutemyer2007(x: Real): Real = {
    require(0.1 <= x && x <= 1)

    log((exp(x) + 2 * sqrt(x) + 1) / 2.0)
  }

  def integrateStoutemyer2007Arith(x: Real): Real = {
    require(0.1 <= x && x <= 1)

    ((x + 2 * x + 1) / 2.0)
  }

  def stoutemyerEq2007(x: Real): Real = {
    require(-1e-8 <= x && x <= 1e-8)

    exp(x) + atan(x)
  }

  def stoutemyerEq2007Arith(x: Real): Real = {
    require(-1e-8 <= x && x <= 1e-8)

    x + x
  }

  def sinxx10(x: Real): Real = {
    require(-3 <= x && x <= 3)

    (3 * x * x * x - 5 * x + 2) * sin(x) * sin(x) + (x * x * x + 5 * x) * sin(x) - 2*x*x - x - 2
  }

  def sinxx10Arith(x: Real): Real = {
    require(-3 <= x && x <= 3)

    (3 * x * x * x - 5 * x + 2) * x * x + (x * x * x + 5 * x) * x - 2*x*x - x - 2
  }

  def axisRotationX(x: Real, y: Real, theta: Real): Real =  {
    require(-2 <= x && x <= 2 && -4 <= y && y <= 4 && -5 <= theta && theta <= 5)// && x +/- 0.01 && y +/- 0.01 && theta +/- 0.01 )

    x * cos(theta) + y * sin(theta)

  }

  def axisRotationXArith(x: Real, y: Real, theta: Real): Real =  {
    require(-2 <= x && x <= 2 && -4 <= y && y <= 4 && -5 <= theta && theta <= 5)// && x +/- 0.01 && y +/- 0.01 && theta +/- 0.01 )

    x * theta + y * theta

  }

  def axisRotationY(x: Real, y: Real, theta: Real): Real = {
    require(-2 <= x && x <= 2 && -10 <= y && y <= 10 && -5 <= theta && theta <= 5) // && x +/- 0.01 && y +/- 0.01 && theta +/- 0.01 )
    -x * sin(theta) + y * cos(theta)
  }

  def axisRotationYArith(x: Real, y: Real, theta: Real): Real = {
    require(-2 <= x && x <= 2 && -10 <= y && y <= 10 && -5 <= theta && theta <= 5) // && x +/- 0.01 && y +/- 0.01 && theta +/- 0.01 )
    -x * theta + y * theta
  }

  def rodriguesRotation(v1: Real, v2: Real, v3: Real, k1: Real, k2: Real, k3: Real, theta: Real): Real = {
    require(-2 <= v1 && v1 <= 2 && -2 <= v2 && v2 <= 2 && -2 <= v3 && v3 <= 2 &&
      -5 <= k1 && k1 <= 5 && -5 <= k2 && k2 <= 5 && -5 <= k3 && k3 <= 5 && -5 <= theta && theta <= 5)

    v1 * cos(theta) + (k2 * v3 - k3 * v2) * sin(theta) + k1 * (k1 * v1 + k2 * v2 + k3 * v3) * (1 - cos(theta))
  }

  def rodriguesRotationArith(v1: Real, v2: Real, v3: Real, k1: Real, k2: Real, k3: Real, theta: Real): Real = {
    require(-2 <= v1 && v1 <= 2 && -2 <= v2 && v2 <= 2 && -2 <= v3 && v3 <= 2 &&
      -5 <= k1 && k1 <= 5 && -5 <= k2 && k2 <= 5 && -5 <= k3 && k3 <= 5 && -5 <= theta && theta <= 5)

    v1 * theta + (k2 * v3 - k3 * v2) * theta + k1 * (k1 * v1 + k2 * v2 + k3 * v3) * (1 - theta)
  }

  def pendulum1(t: Real, w: Real): Real = {
    require(1 <= t && t <= 3 && -5 <= w && w <= 5) // && t +/- 0.01 && w +/- 0.01)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665
    t + h*(w + h/2*(-g/L * sin(t)))
  }

  def pendulum1Arith(t: Real, w: Real): Real = {
    require(1 <= t && t <= 3 && -5 <= w && w <= 5) // && t +/- 0.01 && w +/- 0.01)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665
    t + h*(w + h/2*(-g/L * (t)))
  }

  def pendulum2(t: Real, w: Real): Real = {
    require(-2 <= t && t <= 2 && 1 <= w && w <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665
    w + h * exp(-g/L * sin(t + h/2*cos(w)))
  }

  def pendulum2Arith(t: Real, w: Real): Real = {
    require(-2 <= t && t <= 2 && 1 <= w && w <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665
    w + h * (-g/L * (t + h/2*(w)))
  }

  def forwardk2jY(theta1: Real, theta2: Real): Real = {
    require(-3.14 <= theta1 && theta1 <= 3.14 && -3.14 <= theta2 && theta2 <= 3.14)
    val l1: Real = 0.5
    val l2: Real = 2.5

    l1 * sin(theta1) + l2 * sin(theta1 + theta2)
  }

  def forwardk2jYArith(theta1: Real, theta2: Real): Real = {
    require(-3.14 <= theta1 && theta1 <= 3.14 && -3.14 <= theta2 && theta2 <= 3.14)
    val l1: Real = 0.5
    val l2: Real = 2.5

    l1 * theta1 + l2 * (theta1 + theta2)
  }


  def forwardk2jX(theta1: Real, theta2: Real): Real = {
    require(-3.14 <= theta1 && theta1 <= 3.14 && -3.14 <= theta2 && theta2 <= 3.14)
    val l1: Real = 0.5
    val l2: Real = 5.5

    l1 * cos(theta1) + l2 * cos(theta1 + theta2)
  }

  def forwardk2jXArith(theta1: Real, theta2: Real): Real = {
    require(-3.14 <= theta1 && theta1 <= 3.14 && -3.14 <= theta2 && theta2 <= 3.14)
    val l1: Real = 0.5
    val l2: Real = 5.5

    l1 * theta1 + l2 * (theta1 + theta2)
  }

  /* The following benchmarks are taken from an analysis textbook */


  def ex2_1(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    val x1 = sin(x)
    val x2 = cos(x)
    x1 * x1 * x2 * x2
  }

  def ex2_1Arith(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    val x1 = (x)
    val x2 = (x)
    x1 * x1 * x2 * x2
  }

  def ex2_2(x: Real): Real = {
    require(x > -1 && x < 1)
    val x1 = sin(2 * x)
    val x2 = cos(2 * x)
    x1 * x1 * x2 * x2 * x2
  }

  def ex2_2Arith(x: Real): Real = {
    require(x > -1 && x < 1)
    val x1 = (2 * x)
    val x2 = (2 * x)
    x1 * x1 * x2 * x2 * x2
  }

  def ex2_3(x: Real): Real = {
    require(x > 0 && x < 1)
    val x1 = cos(2 * x)
    val x2 = cos(3 * x)
    x1 * x2
  }

  def ex2_3Arith(x: Real): Real = {
    require(x > 0 && x < 1)
    val x1 = (2 * x)
    val x2 = (3 * x)
    x1 * x2
  }

  def ex2_4(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    val x1 = sin(x)
    val x2 = cos(x)
    x1 * x1 * x1 * x1 * x1 * x2 * x2
  }

  def ex2_4Arith(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    val x1 = (x)
    val x2 = (x)
    x1 * x1 * x1 * x1 * x1 * x2 * x2
  }

  def ex2_5(x: Real): Real = {
    require(17 <= x && x <= 18)
    val x1 = sin(x)
    val x2 = cos(x)
    (x1 + 2 * x2) / (x2 + 2 * x1)
  }

  def ex2_5Arith(x: Real): Real = {
    require(17 <= x && x <= 18)
    val x1 = (x)
    val x2 = (x)
    (x1 + 2 * x2) / (x2 + 2 * x1)
  }

  def ex2_6(x: Real): Real = {
    require(-1 <= x && x <= 1)
    val x1 = tan(x)
    x1 * x1
  }

  def ex2_6Arith(x: Real): Real = {
    require(-1 <= x && x <= 1)
    val x1 = (x)
    x1 * x1
  }

  def ex2_7(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    val x1 = cos(x)
    1 / (3 + 5 * x1)
  }

  def ex2_7Arith(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    val x1 = (x)
    1 / (3 + 5 * x1)
  }

  def ex2_8(x: Real): Real = {
    require(17 <= x && x <= 21)
    val x1 = sin(x)
    1 / (25 + 24 * x1 * x1)
  }

  def ex2_8Arith(x: Real): Real = {
    require(17 <= x && x <= 21)
    val x1 = (x)
    1 / (25 + 24 * x1 * x1)
  }

  def ex2_9(x: Real): Real = {
    require(1 <= x && x < 3.1415)
    val x1 = sin(x)
    val x2 = cos(x)
    1 / (1 - x2 + x1)
  }

  def ex2_9Arith(x: Real): Real = {
    require(1 <= x && x < 3.1415)
    val x1 = (x)
    val x2 = (x)
    1 / (1 + x2 + x1)   // modified to avoid div by zero
  }

  def ex2_10(x: Real): Real = {
    require(-20 <= x && x < -18)
    val x1 = sin(x)
    val x2 = 1 + cos(x)
    x1 / (x2 * x2)
  }

  def ex2_10Arith(x: Real): Real = {
    require(-20 <= x && x < -18)
    val x1 = (x)
    val x2 = 1 + (x)
    x1 / (x2 * x2)
  }

  def ex2_11(x: Real): Real = {
    require(-1.1 <= x && x < 0.9)
    val x1 = 1 / cos(x)
    val x2 = tan(x)
    (x1 * x1) / (4 + x2 * x2)
  }

  def ex2_11Arith(x: Real): Real = {
    require(0.1 <= x && x < 2.1)    //modified to avoid div by zero
    val x1 = 1 / (x)
    val x2 = (x)
    (x1 * x1) / (4 + x2 * x2)
  }

  def ex2_12(x: Real): Real = {
    require(-17 <= x && x < 7.3)
    val x1 = exp(x)
    x1 / (1 + x1)
  }

  def ex2_12Arith(x: Real): Real = {
    require(-17 <= x && x < 7.3)
    val x1 = (x)
    x1 / (1 + x1)
  }

  def ex3_c(x: Real): Real = {
    require(0 <= x && x <= 1)
    val x1 = exp(-2 * x)
    x * x * x * x1
  }

  def ex3_cArith(x: Real): Real = {
    require(0 <= x && x <= 1)
    val x1 = (-2 * x)
    x * x * x * x1
  }

  def ex3_d(x: Real): Real = {
    require(0 <= x && x <= 7)
    val x1 = exp(-2 * x)
    val x2 = sin(x)
    x1 * x2
  }

  def ex3_dArith(x: Real): Real = {
    require(0 <= x && x <= 7)
    val x1 = (-2 * x)
    val x2 = (x)
    x1 * x2
  }

}
