import daisy.lang._
import Real._


object Bicycle {

  // s1, s2, s3, y1: <1, 16, 14>, [-1, 1]
  def out1(s1: Real, s2: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1)
    (-3.025300) * s1 + (-12.608900) * s2
  }

  def state1(s1: Real, s2: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= y1 && y1 <= 1)
    (0.961270) * s1 + (-0.095962) * s2 + (0.013200) * y1
  }

  def state2(s1: Real, s2: Real, y1: Real) = {
    require(-1 <= s1 && s1 <= 1 && -1 <= s2 && s2 <= 1 && -1 <= y1 && y1 <= 1)
    (-0.058217) * s1 + (0.727430) * s2 + (0.102100) * y1
  }


}