import daisy.lang._
import Real._

object RangeRegressionFunctions {
  // from testcases/relative/wideranges

  def doppler(u: Real, v: Real, T: Real): Real = {
    require(-100.0 <= u && u <= 1000 && 2 <= v && v <= 200000 && -300 <= T && T <= 500)

    (- (331.4 + 0.6 * T) *v) / ((331.4 + 0.6 * T + u)*(331.4 + 0.6 * T + u))

  }

  def sine(x: Real): Real = {
    require(x > 0.825 && x < 1.7)
    x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0
  }


  def sineOrder3(x: Real): Real = {
    require(-2.0 < x && x < -1.125)
    0.954929658551372 * x -  0.12900613773279798*(x*x*x)
  }


  def sqroot(x: Real): Real = {
    require(x >= 0.0 && x < 1.925) // FPTaylor uses [ 0; 1 ]
    1.0 + 0.5*x - 0.125*x*x + 0.0625*x*x*x - 0.0390625*x*x*x*x
  }

  def bspline0(u: Real): Real = {
    require(0 <= u && u <= 0.875)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  }

  def bspline1(u: Real): Real = {
    require(0.875 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  }

  def bspline2(u: Real): Real = {
    require(0.5 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  }

  def bspline3(u: Real): Real = {
    require(0.125 <= u && u <= 10)
    -u*u*u / 6.0
  }

  def rigidBody1(x1: Real, x2: Real, x3: Real): Real = {
    require(-1500.0 <= x1 && x1 <= -0.0001 && 0.1 <= x2 && x2 <= 15.0 && -15.0 <= x3 && x3 <= -0.1)

    -x1*x2 - 2*x2*x3 - x1 - x3
  }

  def rigidBody2(x1: Real, x2: Real, x3: Real): Real = {
    require(-1500.0 <= x1 && x1 <= -1.125 && -15.0 <= x2 && x2 <= -11.25 &&
      -15.0 <= x3 && x3 <= -11.25)

    2*(x1*x2*x3) + (3*x3*x3) - x2*(x1*x2*x3) + (3*x3*x3) - x2
  }

  def turbine1(v: Real, w: Real, r: Real): Real = {
    require(-5.5 <= v && v <= -0.3 && 0.001 <= w && w <= 1.9 && 3.8 <= r && r <= 10.8)

    3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5

  }

  def turbine2(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -3.3 && -0.4 <= w && w <= -0.01 && 3.8 <= r && r <= 16.25)

    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5

  }

  def turbine3(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 2.85 <= r && r <= 8.5)

    3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5

  }

  def kepler0(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(4 <= x1 && x1 <= 6.36 && 0.0001 <= x2 && x2 <= 0.00015 && 4.63 <= x3 && x3 <= 6306 &&
      -10 <= x4 && x4 <= -0.01 && 4 <= x5 && x5 <= 6.36 && 4 <= x6 && x6 <= 6.36)

    x2 * x5 + x3 * x6 - x2 * x3 - x5 * x6 + x1 * (-x1 + x2 + x3 - x4 + x5 + x6)

  } // 1.15e-15


  def kepler1(x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(4 <= x1 && x1 <= 6.36 && 0.04 <= x2 && x2 <= 0.0636 && 40 <= x3 && x3 <= 6300.6 &&
      0.001 <= x4 && x4 <= 0.015)

    x1 * x4 * (-x1 + x2 + x3 - x4) + x2 * (x1 - x2 + x3 + x4) + x3 * (x1 + x2 - x3 + x4) -
      x2 * x3 * x4 - x1 * x3 - x1 * x2 - x4

  } // 4.50e–13

  def kepler2(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(4 <= x1 && x1 <= 6.36 && 0 <= x2 && x2 <= 0 && 40 <= x3 && x3 <= 63.6 &&
      0 <= x4 && x4 <= 0 && 4 <= x5 && x5 <= 6.36 && 4 <= x6 && x6 <= 6000.36)

      x1 * x4 * (-x1 + x2 + x3 - x4 + x5 + x6) + x2 * x5 * (x1 - x2 + x3 + x4 - x5 + x6) +
        x3* x6 * (x1 + x2 - x3 + x4 + x5 - x6) - x2 * x3 * x4 -
          x1* x3* x5 - x1 * x2 * x6 - x4 * x5 * x6

  }

  def himmilbeau(x1: Real, x2: Real) = {
    require(20 <= x1 && x1 <= 100 && -2<= x2 && x2 <= 20)

    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)

  }

  // def pendulum1(t: Real, w: Real): Real = {
  //   require(-2 <= t && t <= 2 && -5 <= w && w <= 5)

  //   t + 0.01*(w + 0.01/2*(-9.80665/2.0 * sin(t)))

  // }


  // def pendulum2(t: Real, w: Real): Real = {
  //   require(-2 <= t && t <= 2 && -5 <= w && w <= 5)

  //   w + 0.01*(-9.80665/2.0 * sin(t + 0.01/2*w))

  // }

  // def analysis1(x: Real): Real = {
  //   require(x > -1.57079632679 && x < 1.57079632679)
  //   sin(x) * sin(x) * cos(x) * cos(x)
  // }

  // def analysis2(x: Real): Real = {
  //   require(-1.1 <= x && x < 0.9)

  //   ((1 / cos(x)) * (1 / cos(x))) / (4 + (tan(x)) * (tan(x)))
  // }

  // // from FPTaylor
  // def logExp(x: Real): Real = {
  //   require(-8 <= x && x <= 8)

  //   log(1 + exp(x))
  // }

  // // from FPTaylor
  // def sphere(r: Real, x: Real, lat: Real, lon: Real): Real = {
  //   require(0 <= r && r <= 10 && -10 <= x && x <= 10 &&
  //     -1.570796 <= lat && lat <= 1.570796 && -3.14159265 <= lon && lon <= 3.14159265)

  //   x + r * sin(lat) * cos(lon)
  // }


  /*def jetEngine(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -20 <= x2 && x2 <= 5)
    x1 + (
      (2*x1*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1))*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1) - 3) +
     x1*x1*(4*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1))-6))
      *(x1*x1 + 1) +
    3*x1*x1*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1)) + x1*x1*x1 + x1 + 3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)))

  }*/

}