import daisy.lang._
import Real._


object ApproxBenchmarksSmall {

  def axisRotationX(x: Real, y: Real, theta: Real): Real =  {
    require(-2 <= x && x <= 2 && -2 <= y && y <= 2 && 0.01 <= theta && theta <= 1.5)
    x * cos(theta) + y * sin(theta)

  } ensuring (res => res +/- 1.49e-10)

  def axisRotationY(x: Real, y: Real, theta: Real): Real = {
    require(-2 <= x && x <= 2 && -2 <= y && y <= 2 && 0.01 <= theta && theta <= 1.5)
    -x * sin(theta) + y * cos(theta)
  } ensuring (res => res +/- 1.49e-10)


  def forwardk2jY(theta1: Real, theta2: Real): Real = {
    require(0.01 <= theta1 && theta1 <= 1.5 && 0.01 <= theta2 && theta2 <= 1.5)
    val l1: Real = 0.5
    val l2: Real = 0.5

    l1 * sin(theta1) + l2 * sin((theta1 + theta2) / 2)
  } ensuring (res => res +/- 4.89e-11)


  def forwardk2jX(theta1: Real, theta2: Real): Real = {
    require(0.01 <= theta1 && theta1 <= 1.5 && 0.01 <= theta2 && theta2 <= 1.5)
    val l1: Real = 0.5
    val l2: Real = 0.5

    l1 * cos(theta1) + l2 * cos(theta1 + theta2)
  } ensuring (res => res +/- 8.39e-11)

  def rodriguesRotation(v1: Real, v2: Real, v3: Real, k1: Real, k2: Real, k3: Real, theta: Real): Real = {
    require(-2 <= v1 && v1 <= 2 && -2 <= v2 && v2 <= 2 && -2 <= v3 && v3 <= 2 &&
      -5 <= k1 && k1 <= 5 && -5 <= k2 && k2 <= 5 && -5 <= k3 && k3 <= 5 && 0 <= theta && theta <= 1.5)

    val t1 = cos(theta)
    v1 * t1 + (k2 * v3 - k3 * v2) * sin(theta) + k1 * (k1 * v1 + k2 * v2 + k3 * v3) * (1 - t1)
  } ensuring (res => res +/- 1.7e-08)


  def sinxx10(x: Real): Real = {
    require(0.01 <= x && x <= 1.5)

    val t1 = sin(x)
    (3 * x * x * x - 5 * x + 2) * t1 * t1 + (x * x * x + 5 * x) * t1 - 2*x*x - x - 2
  } ensuring(res => res +/- 2.51e-09)


  def xu1(x1: Real, x2: Real): Real = {
    require(0.01 <= x1 && x1 <= 0.75 && 0.01 <= x2 && x2 <= 1.5)

    2 * sin(x1) + 0.8 * cos(2 * x1) + 7 * sin(x2) - x1
  } ensuring(res => res +/- 4.24e-10)

  def xu2(x1: Real, x2: Real): Real = {
    require(0.01 <= x1 && x1 <= 1.5 && 0.01 <= x2 && x2 <= 0.5)

    1.4 * sin(3 * x2) + 3.1 * cos(2 * x2) - x2 + 4 * sin(2 * x1)
  } ensuring(res => res +/- 5.8e-10)

   def pendulum1(t: Real, w: Real): Real = {
     require(0.01 <= t && t <= 1.6 && -5 <= w && w <= 5)

     val h: Real = 0.01
     val L: Real = 2.0
     val m: Real = 1.5
     val g: Real = 9.80665
     val k1w = -g/L * sin(t)
     val k2t = w + h/2*k1w
     val tNew = t + h*k2t
     tNew
   } ensuring(res => res +/- 4.79e-11)

   def pendulum2(t: Real, w: Real): Real = {
     require(0.05 <= t && t <= 1.5 && -5 <= w && w <= 5)

     val h: Real = 0.01
     val L: Real = 2.0
     val m: Real = 1.5
     val g: Real = 9.80665
     val k1t = w
     val k2w = -g/L * sin(t + h/2*k1t)
     val wNew = w + h*k2w
     wNew
   } ensuring(res => res +/- 1.07e-10)

  // Gaussian Naive Bayes classifier
  def predictGaussianNB(f0: Real, f1: Real, f2: Real, f3: Real, sigma0: Real, sigma1: Real, sigma2: Real, sigma3: Real,
                        theta0: Real, theta1: Real, theta2: Real, theta3: Real, prior: Real): Real = {
    require(0.12 <= sigma0 && sigma0 <= 0.40 && 0.09 <= sigma1 && sigma1 <= 0.15 &&
      0.02 <= sigma2 && sigma2 <= 0.30 && 0.01 <= sigma3 && sigma3 <= 0.08 &&
      5.0 <= theta0 && theta0 <= 6.6 && 2.7 <= theta1 && theta1 <= 3.5 &&
      1.4 <= theta2 && theta2 <= 5.6 && 0.2 <= theta3 && theta3 <= 2.1 &&
      0.25 <= prior && prior <= 0.5 &&
      4.0 <= f0 && f0 <= 8.0 && 2.0 <= f1 && f1 <= 4.5 &&
      1.0 <= f2 && f2 <= 7.0 && 0.0 <= f3 && f3 <= 2.5)

    val pi: Real = 3.141
    val sum = log(2.0 * pi * sigma0) + log(2.0 * pi * sigma1) + log(2.0 * pi * sigma2) + log(2.0 * pi * sigma3)

    val nij = -0.5 * sum

    val sum2 = (f0 - theta0) * (f0 - theta0) / sigma0 +
      (f1 - theta1) * (f1 - theta1) / sigma1 +
      (f2 - theta2) * (f2 - theta2) / sigma2 +
      (f3 - theta3) * (f3 - theta3) / sigma3

    -0.5 * sum - 0.5 * sum2 + log(prior)
  } ensuring (res => res +/- 4.15e-07)

  // C-Support Vector Classification with rbf kernel
  def predictSVC(f0: Real, f1: Real, f2: Real, f3: Real, vectors0: Real, vectors1: Real,
                 vectors2: Real, vectors3: Real, coefficient: Real, intercept: Real, factor: Real): Real = {
    require(4.0 <= f0 && f0 <= 7.0 && 2.0 <= f1 && f1 <= 4.5 &&
      1.0 <= f2 && f2 <= 6.0 && 0.0 <= f3 && f3 <= 2.5 &&
      4.5 <= vectors0 && vectors0 <= 5.9 && 2.2 <= vectors1 && vectors1 <= 4.4 &&
      1.3 <= vectors2 && vectors2 <= 4.9 && 0.2 <= vectors3 && vectors3 <= 2.3 &&
      -0.12 <= intercept && intercept <= 0.06 && -1 <= coefficient && coefficient <= 1.0 &&
      5 <= factor && factor <= 50)

    val gamma: Real = 0.1
    val k = (vectors0 - f0) * (vectors0 - f0) + (vectors1 - f1) * (vectors1 - f1) + (vectors2 - f2) * (vectors2 - f2) + (vectors3 - f3) * (vectors3 - f3)
    val kernel = exp(gamma * k)

    factor * coefficient * kernel + intercept
  } ensuring (res => res +/- 1.46e-06)

  def predictMLPLogistic(f0: Real, f1: Real, f2: Real, f3: Real, weights_0_0: Real,
                         weights_0_1: Real, weights_0_2: Real, weights_0_3: Real, weights_1_0: Real,
                         weights_1_1: Real, weights_1_2: Real, bias_0: Real, bias_1: Real): Real = {

    require(4.0 <= f0 && f0 <= 8.0 && 2.0 <= f1 && f1 <= 4.5 && 1.0 <= f2 && f2 <= 7.0 && 0.0 <= f3 && f3 <= 2.5 &&
      -0.3 <= weights_0_0 && weights_0_0 <= 0.3 && -0.5 <= weights_0_1 && weights_0_1 <= 0.0 &&
      -0.2 <= weights_0_2 && weights_0_2 <= 0.1 && 0.1 <= weights_0_3 && weights_0_3 <= 0.3 &&
      -0.4 <= weights_1_0 && weights_1_0 <= 0.8 && -0.3 <= weights_1_1 && weights_1_1 <= 0.3 &&
      0.0 <= weights_1_2 && weights_1_2 <= 0.4 &&
      0.0 <= bias_0 && bias_0 <= 0.5 && -0.4 <= bias_1 && bias_1 <= 0.5)

    val n1 = f0 * weights_0_0 + f1 * weights_0_1 + f2 * weights_0_2 + f3 * weights_0_3 + bias_0
    val hidden = 1.0 / (1.0 + exp(-n1))

    val n2 = hidden * weights_1_0 + hidden * weights_1_1 + hidden * weights_1_2 + bias_1
    1.0 / (1.0 + exp(-n2))

  } ensuring (res => res +/- 2.15e-06) // for Fixed32 Abs error: 0.00021461025199390595 Real range: [0.2514148950272635, 0.8787756292228485]

}
