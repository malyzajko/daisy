import daisy.lang._
import Real._


object BallBeamSingle {

  // s1 <1, 16, 14>, s2, s3, s4: <1, 16, 15>
  def BallBeamSingle_out(s1: Real) = { // , s2: Real, s3: Real, s4: Real
    require(0 <= s1 && s1 <= 1) // && -0.5 <= s2 && s2 <= 0.5 && 0 <= s3 && s3 <= 0.5 && 0 <= s4 && s4 <= 0.5)
    (-1828.6) * s1 + (-1028.6) * -0.487 + (-2008.0) * 0.25 + (-104.0) * 0.375
  }



}