import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test05_nonlin1_test2 {


  def test05_nonlin1_test2(x: Real): Real = {
    require(((1.00001 < x) && (x < 2.0)))
    (1.0 / (x + 1.0))
  }

}
