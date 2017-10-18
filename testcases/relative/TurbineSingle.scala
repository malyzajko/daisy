import daisy.lang._
import Real._

object TurbineSingle {
  def turbine1(v: Real): Real = {
    require(-4.5 <= v && v <= -0.3)
    3 + 2/(v*v) - 0.125*(3-2*v)*(v*v*v*v)/(1-v) - 4.5
  }
  def turbine2(v: Real): Real = {
    require(-4.5 <= v && v <= -0.3)
    6*v - 0.5 * v * (v*v*v*v) / (1-v) - 2.5
  }
  def turbine3(v: Real): Real = {
    require(-4.5 <= v && v <= -0.3)
    3 - 2/(v*v) - 0.125 * (1+2*v) * (v*v*v*v) / (1-v) - 0.5
  }
}