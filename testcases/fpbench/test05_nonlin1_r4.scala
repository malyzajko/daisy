import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test05_nonlin1_r4 {


  def test05_nonlin1_r4(x: Real): Real = {
    require(((1.00001 < x) && (x < 2.0)))
    val r1: Real = (x - 1.0)
    val r2: Real = (x * x)
    (r1 / (r2 - 1.0))
  }

}
