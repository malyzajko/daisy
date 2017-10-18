import daisy.lang._
import Real._


object Transcendentals {


  def pendulum1(t: Real, w: Real): Real = {
    require(-2 <= t && t <= 2 && -5 <= w && w <= 5)

    t + 0.01*(w + 0.01/2*(-9.80665/2.0 * sin(t)))

  }


  def pendulum2(t: Real, w: Real): Real = {
    require(-2 <= t && t <= 2 && -5 <= w && w <= 5)

    w + 0.01*(-9.80665/2.0 * sin(t + 0.01/2*w))

  }

  def analysis1(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    sin(x) * sin(x) * cos(x) * cos(x)
  }

  def analysis2(x: Real): Real = {
    require(-1.1 <= x && x < 0.9)

    ((1 / cos(x)) * (1 / cos(x))) / (4 + (tan(x)) * (tan(x)))
  }

  // from FPTaylor
  def logExp(x: Real): Real = {
    require(-8 <= x && x <= 8)

    log(1 + exp(x))
  }

  // from FPTaylor
  def sphere(r: Real, x: Real, lat: Real, lon: Real): Real = {
    require(0 <= r && r <= 10 && -10 <= x && x <= 10 &&
      -1.570796 <= lat && lat <= 1.570796 && -3.14159265 <= lon && lon <= 3.14159265)

    x + r * sin(lat) * cos(lon)
  }

}