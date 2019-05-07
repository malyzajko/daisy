import daisy.lang._
import Real._


// some benchmarks from http://www-sop.inria.fr/coprin/logiciels/ALIAS/Benches/node2.html#SECTION00021000000000000000
object TranscendentalBenchmarks {

  def xu1(x1: Real, x2: Real): Real = {
    require(-3.14 <= x1 && x1 <= 3.14 && -3.14 <= x2 && x2 <= 3.14)

    2 * sin(x1) + 0.8 * cos(2 * x1) + 7 * sin(x2) - x1
  } ensuring(res => res +/- 1e-4)

  def xu2(x1: Real, x2: Real): Real = {
    require(-3.14 <= x1 && x1 <= 3.14 && -3.14 <= x2 && x2 <= 3.14)

    1.4 * sin(3 * x2) + 3.1 * cos(2 * x2) - x2 + 4 * sin(2 * x1)
  } ensuring(res => res +/- 1e-4)

  def integrate18257(x: Real): Real = {
    require(0 <= x && x <= 3.14)

    exp(cos(x)) * cos(x - sin(x))
  } ensuring(res => res +/- 1e-4)

  def integrateStoutemyer2007(x: Real): Real = {
    require(0.1 <= x && x <= 1)

    log((exp(x) + 2 * sqrt(x) + 1) / 2.0)
  } ensuring(res => res +/- 1e-5)

  def stoutemyerEq2007(x: Real): Real = {
    require(-1e-8 <= x && x <= 1e-8)

    exp(x) + atan(x)
  } ensuring(res => res +/- 1e-5)

  def sinxx10(x: Real): Real = {
    require(-3 <= x && x <= 3)

    (3 * x * x * x - 5 * x + 2) * sin(x) * sin(x) + (x * x * x + 5 * x) * sin(x) - 2*x*x - x - 2
  } ensuring(res => res +/- 1e-2)

  def axisRotationX(x: Real, y: Real, theta: Real): Real =  {
    require(-2 <= x && x <= 2 && -4 <= y && y <= 4 && -5 <= theta && theta <= 5)// && x +/- 0.01 && y +/- 0.01 && theta +/- 0.01 )

    x * cos(theta) + y * sin(theta)

  } ensuring(res => res +/- 1e-4)

  def axisRotationY(x: Real, y: Real, theta: Real): Real = {
    require(-2 <= x && x <= 2 && -10 <= y && y <= 10 && -5 <= theta && theta <= 5) // && x +/- 0.01 && y +/- 0.01 && theta +/- 0.01 )
    -x * sin(theta) + y * cos(theta)

  } ensuring(res => res +/- 1e-4)

  def rodriguesRotation(v1: Real, v2: Real, v3: Real, k1: Real, k2: Real, k3: Real, theta: Real): Real = {
    require(-2 <= v1 && v1 <= 2 && -2 <= v2 && v2 <= 2 && -2 <= v3 && v3 <= 2 &&
      -5 <= k1 && k1 <= 5 && -5 <= k2 && k2 <= 5 && -5 <= k3 && k3 <= 5 && -5 <= theta && theta <= 5)

    v1 * cos(theta) + (k2 * v3 - k3 * v2) * sin(theta) + k1 * (k1 * v1 + k2 * v2 + k3 * v3) * (1 - cos(theta))
  } ensuring(res => res +/- 1e-2)

  def pendulum1(t: Real, w: Real): Real = {
    require(1 <= t && t <= 3 && -5 <= w && w <= 5) // && t +/- 0.01 && w +/- 0.01)

    t + 0.01 * (w + 0.01/2*(-9.80665/2.0 * sin(t)))
  } ensuring(res => res +/- 1e-5)

  def pendulum2(t: Real, w: Real): Real = {
    require(-2 <= t && t <= 2 && 1 <= w && w <= 5)

    w + 0.01 * exp(-9.80665/2.0 * sin(t + 0.01/2*cos(w)))
  } ensuring(res => res +/- 1e-4)

  def forwardk2jY(theta1: Real, theta2: Real): Real = {
    require(-3.14 <= theta1 && theta1 <= 3.14 && -3.14 <= theta2 && theta2 <= 3.14)

    0.5 * sin(theta1) + 2.5 * sin(theta1 + theta2)
  } ensuring(res => res +/- 1e-4)


  def forwardk2jX(theta1: Real, theta2: Real): Real = {
    require(-3.14 <= theta1 && theta1 <= 3.14 && -3.14 <= theta2 && theta2 <= 3.14)

    0.5 * cos(theta1) + 5.5 * cos(theta1 + theta2)
  } ensuring(res => res +/- 1e-4)

  def ex2_1(x: Real): Real = {
    require(x > -1.57 && x < 1.57)
    val x1 = sin(x)
    val x2 = cos(x)
    x1 * x1 * x2 * x2
  } ensuring(res => res +/- 1e-5)

  def ex2_2(x: Real): Real = {
    require(x > -1 && x < 1)
    val x1 = sin(2 * x)
    val x2 = cos(2 * x)
    x1 * x1 * x2 * x2 * x2
  } ensuring(res => res +/- 1e-4)

  def ex2_3(x: Real): Real = {
    require(x > 0 && x < 1)
    val x1 = cos(2 * x)
    val x2 = cos(3 * x)
    x1 * x2
  } ensuring(res => res +/- 1e-5)

  def ex2_4(x: Real): Real = {
    require(x > -1.57 && x < 1.57)
    val x1 = sin(x)
    val x2 = cos(x)
    x1 * x1 * x1 * x1 * x1 * x2 * x2
  } ensuring(res => res +/- 1e-4)

  def ex2_5(x: Real): Real = {
    require(17 <= x && x <= 18)
    val x1 = sin(x)
    val x2 = cos(x)
    (x1 + 2 * x2) / (x2 + 2 * x1)
  } ensuring(res => res +/- 1e-4)

  def ex2_6(x: Real): Real = {
    require(-1 <= x && x <= 1)
    val x1 = tan(x)
    x1 * x1
  } ensuring(res => res +/- 1e-4)

  def ex2_7(x: Real): Real = {
    require(x > -1.57 && x < 1.570)
    val x1 = cos(x)
    1 / (3 + 5 * x1)
  } ensuring(res => res +/- 1e-5)

  def ex2_8(x: Real): Real = {
    require(17 <= x && x <= 21)
    val x1 = sin(x)
    1 / (25 + 24 * x1 * x1)
  } ensuring(res => res +/- 1e-3)

  def ex2_9(x: Real): Real = {
    require(1 <= x && x < 3.1415)
    val x1 = sin(x)
    val x2 = cos(x)
    1 / (1 - x2 + x1)
  } ensuring(res => res +/- 1e-4)

  def ex2_10(x: Real): Real = {
    require(-20 <= x && x < -18)
    val x1 = sin(x)
    val x2 = 1 + cos(x)
    x1 / (x2 * x2)
  } ensuring(res => res +/- 1e-4)

  def ex2_11(x: Real): Real = {
    require(-1.1 <= x && x < 0.9)
    val x1 = 1 / cos(x)
    val x2 = tan(x)
    (x1 * x1) / (4 + x2 * x2)
  } ensuring(res => res +/- 3)

  def ex2_12(x: Real): Real = {
    require(-17 <= x && x < 7.3)
    val x1 = exp(x)
    x1 / (1 + x1)
  } ensuring(res => res +/- 200)

  def ex3_c(x: Real): Real = {
    require(0 <= x && x <= 1)
    val x1 = exp(-2 * x)
    x * x * x * x1
  } ensuring(res => res +/- 1e-5)

  def ex3_d(x: Real): Real = {
    require(0 <= x && x <= 7)
    val x1 = exp(-2 * x)
    val x2 = sin(x)
    x1 * x2
  } ensuring(res => res +/- 1e-4)



}
