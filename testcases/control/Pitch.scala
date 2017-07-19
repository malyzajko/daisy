import daisy.lang._
import Real._


object Pitch {

  // s1, s2, s3, y1: <1, 32, 30>, [-1, 1]
  def out1(s1: Real, s2: Real, s3: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1)
    (0.120200) * state1 + (-42.565500) * state2 + (-1.000100) * state3
  }

  def state1(s1: Real, s2: Real, s3: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= y1 && y1 <= 1)
    (0.999715) * state1 + (0.046781) * state2 + (-0.000333) * state3 + (0.000100) * y1
  }

  def state2(s1: Real, s2: Real, s3: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= y1 && y1 <= 1)
    (-0.000011) * state1 + (0.998710) * state2 + (-0.000020) * state3 + (0.000000) * y1
  }

  def state3(s1: Real, s2: Real, s3: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= y1 && y1 <= 1)
    (-0.000000) * state1 + (0.056663) * state2 + (0.998299) * state3 + (0.001700) * y1
  }

}