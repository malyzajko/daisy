import daisy.lang._
import Real._


object Transcendentals {


  def pendulum1(t: Real, w: Real): Real = {
    require(-2 <= t && t <= 2 && -5 <= w && w <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val g: Real = 9.80665
    val k1w = -g/L * sin(t)
    val k2t = w + h/2*k1w
    t + h*k2t

  }


  def pendulum2(t: Real, w: Real): Real = {
    require(-2 <= t && t <= 2 && -5 <= w && w <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val g: Real = 9.80665
    val k1t = w
    val k2w = -g/L * sin(t + h/2*k1t)
    w + h*k2w

  }

  def analysis1(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    val x1 = sin(x)
    val x2 = cos(x)
    x1 * x1 * x2 * x2
  }

  def analysis2(x: Real): Real = {
    require(-1.1 <= x && x < 0.9)
    val x1 = 1 / cos(x)
    val x2 = tan(x)
    (x1 * x1) / (4 + x2 * x2)
  }

  // from FPTaylor
  def logExp(x: Real): Real = {
    require(-8 <= x && x <= 8)
    val e = exp(x)
    val t = 1 + e
    log(t)
  }

  // from FPTaylor
  def sphere(r: Real, x: Real, lat: Real, lon: Real): Real = {
    require(0 <= r && r <= 10 && -10 <= x && x <= 10 &&
      -1.570796 <= lat && lat <= 1.570796 && -3.14159265 <= lon && lon <= 3.14159265)

    x + r * sin(lat) * cos(lon)
  }

}