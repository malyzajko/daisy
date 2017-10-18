import daisy.lang._
import Real._


object InvertedPendulum {

  // s1: <1, 16, 9>  s2: <1, 16, 11>   s3,s4: <1, 16, 15>
  def invPendulum(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(0.005 <= s1 && s1 <= 5000 &&
        0.005 <= s2 && s2 <= 1000 &&
      -0.785 <= s3 && s3 <= -0.005 &&
      -0.785 <= s4 && s4 <= -0.005)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  }



}