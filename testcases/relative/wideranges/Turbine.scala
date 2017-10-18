

import daisy.lang._
import Real._


object Turbine {

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
}