

import daisy.lang._
import Real._

/*
  Real2Float

  Optimization problems
  A Collection of Test Problems for
  Constrained Global Optimization Algorithms,
  Floudas, Pardalos 1990

*/
object Floudas {



  def floudas26(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real,
    x7: Real, x8: Real, x9: Real, x10: Real): Real = {
    require(0 <= x1 && x1 <= 1 && 0 <= x2 && x2 <= 1 && 0 <= x3 && x3 <= 1 && 0 <= x4 && x4 <= 1 &&
      0 <= x5 && x5 <= 1 && 0 <= x6 && x6 <= 1 && 0 <= x7 && x7 <= 1 && 0 <= x8 && x8 <= 1 &&
      0 <= x9 && x9 <= 1 && 0 <= x10 && x10 <= 1 &&
      2 * x1 +6*x2 +1*x3 +0*x4 +3*x5 + 3*x6 +2*x7 +6*x8 +2*x9+2*x10 - 4  >= 0 &&
      22-(6*x1 -5*x2 +8*x3 -3*x4 +0*x5+1*x6 +3*x7 +8*x8 +9*x9-3*x10)>= 0 &&
      -(5*x1 +6*x2 +5*x3 +3*x4 +8*x5 -8*x6 +9*x7 +2*x8 +0*x9-9*x10) - 6 >= 0 &&
      -(9*x1 +5*x2 +0*x3 -9*x4 +1*x5 -8*x6 +3*x7 -9*x8 -9*x9-3*x10) - 23 >= 0 &&
      -(-(8*x1) +7*x2 -4*x3 -5*x4 -9*x5 +1*x6 -7*x7 -1*x8 +3*x9-2*x10) - 12 >= 0)

    48*x1 + 42*x2 + 48*x3 + 45*x4 + 44*x5 + 41*x6 + 47*x7 + 42*x8 + 45*x9 + 46*x10 -
      50*(x1*x1 + x2*x2 + x3*x3 + x4*x4 + x5*x5 + x6*x6 + x7*x7 + x8*x8 + x9*x9 + x10*x10)

  }// 5.15e-13

  def floudas33(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(0 <= x1 && x1 <= 6 && 0 <= x2 && x2 <= 6 && 1 <= x3 && x3 <= 5 &&
      0 <= x4 && x4 <= 6 && 1 <= x5 && x5 <= 5 && 0 <= x6 && x6 <= 10 &&
      (x3 -3)*(x3 -3) + x4 - 4 >= 0 &&
      (x5 -3)*(x5 -3) + x6 - 4 >= 0 &&
      2 - x1 + 3 * x2 >= 0 &&
      2 + x1 - x2 >= 0 &&
      6 - x1 - x2 >= 0 &&
      x1 + x2 - 2 >= 0)

    - (25 * (x1 -2)*(x1 -2)) - (x2 -2)* (x2 -2) - (x3 -1)*(x3 -1) -
      (x4 -4)*(x4 -4) - (x5 - 1)*(x5 - 1) - (x6 - 4)* (x6 - 4)
  } //5.81e–13

  def floudas34(x1: Real, x2: Real, x3: Real): Real = {
    require(0 <= x1 && x1 <= 2 && 0 <= x2 && x2 <= 2 && 0 <= x3 && x3 <= 3 &&
      4 - x1 - x2 - x3 >= 0 &&
      6 - 3 * x2 - x3 >= 0 &&
      2*x1 - 0.75 - 2*x3 + 4*x1*x1 - 4*x1*x2 + 4*x1*x3 + 2*x2*x2 - 2*x2*x3 + 2*x3*x3 >= 0)

    -2 * x1 + x2 - x3

  } //2.67e - 15

  def floudas46(x1: Real, x2: Real): Real = {
    require(0 <= x1 && x1 <= 3 && 0 <= x2 && x2 <= 4 &&
      2 * x1*x1*x1*x1 - 8 * x1*x1*x1 + 8 * x1* x1 - x2 >= 0 &&
      4 * x1*x1*x1*x1 - 32 * x1*x1*x1 + 88 * x1*x1 - 96*x1 + 36 - x2 >= 0)

    -x1 - x2
  } //1.38e–15

  def floudas47(x1: Real, x2: Real): Real = {
    require(0 <= x1 && x1 <= 2 && 0 <= x2 && x2 <= 3 &&
      -2 * x1*x1*x1*x1 + 2 - x2 >= 0)

    -12*x1 -7*x2 +x2 *x2

  } //1.01e–14


  // from FPTaylor github
  def floudas1(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(0 <= x1 && x1 <= 6 && 0 <= x2 && x2 <= 6 && 1 <= x3 && x3 <= 5 &&
      0 <= x4 && x4 <= 6 && 1 <= x5 && x5 <= 5 && 0 <= x6 && x6 <= 10 &&
      (x3 - 3) * (x3 - 3) + x4 - 4 >= 0 && (x5 - 3) * (x5 - 3) + x6 - 4 >= 0 &&
      2 - x1 + 3 * x2 >= 0 &&  2 + x1 - x2 >= 0 &&  6 - x1 - x2 >= 0 && x1 + x2 - 2 >= 0)

    -25 * ((x1 - 2) * (x1 - 2)) - ((x2 - 2) * (x2 - 2)) - ((x3 - 1) * (x3 - 1)) -
      ((x4 - 4) * (x4 - 4)) - ((x5 - 1) * (x5 - 1)) - ((x6 - 4) * (x6 - 4))
  }

}

