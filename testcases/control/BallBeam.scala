import daisy.lang._
import Real._


object BallBeam {

  // s1 <1, 16, 14>, s2, s3, s4: <1, 16, 15>
  def out(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(0 <= s1 && s1 <= 1 && -0.5 <= s2 && s2 <= 0.5 && 0 <= s3 && s3 <= 0.5 && 0 <= s4 && s4 <= 0.5)
    (-1828.6) * s1 + (-1028.6) * s2 + (-2008.0) * s3 + (-104.0) * s4
  }



}