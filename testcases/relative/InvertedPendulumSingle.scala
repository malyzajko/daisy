import daisy.lang._
import Real._


object InvertedPendulumSingle {

  // s1: <1, 16, 9>  s2: <1, 16, 11>   s3,s4: <1, 16, 15>
  def invertedPendulum(s1: Real) = {
    require(-50 <= s1 && s1 <= 50)
    val s2:Real = 1.2 // -10 <= s2 && s2 <= 10
    val s3: Real = 0.78// -0.785 <= s3 && s3 <= 0.785
    val s4: Real = -0.75
    1.0000 * s1 + 1.6567 * 1.2 + (-18.6854) * 0.78 + (-3.4594) * (-0.75)
  }



}