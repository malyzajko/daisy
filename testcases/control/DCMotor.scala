import daisy.lang._
import Real._


object DCMotor {

  // s1, s2, s3, y1: <1, 16, 14>, [-1, 1]
  def out1(s1: Real, s2: Real, s3: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1)
    (-0.112900) * s1 + (-0.021100) * s2 + (-0.009300) * s3
  }

  def state1(s1: Real, s2: Real, s3: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= y1 && y1 <= 1)
    (0.960883) * s1 + (0.000949) * s2 + (-0.000004) * s3 + (0.039000) * y1
  }

  def state2(s1: Real, s2: Real, s3: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= y1 && y1 <= 1)
    (-0.602449) * s1 + (0.899089) * s2 + (-0.013648) * s3 + (0.370000) * y1
  }

  def state3(s1: Real, s2: Real, s3: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= y1 && y1 <= 1)
    (-0.009134) * s1 + (-0.011434) * s2 + (-0.002232) * s3 + (-0.017500) * y1
  }

}