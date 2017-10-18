
import daisy.lang._
import Real._


object Sine {

  def sine(x: Real): Real = {
    require(x > 0.875 && x < 1.57079632679)
    x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0
  }


  def sineOrder3(x: Real): Real = {
    require(-2.0 < x && x < -1.125)
    0.954929658551372 * x -  0.12900613773279798*(x*x*x)
  }


}