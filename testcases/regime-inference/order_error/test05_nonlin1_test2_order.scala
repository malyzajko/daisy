import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test05_nonlin1_test2_order {


  def test05_nonlin1_test2_order(x: Real): Real = {
    require(((1.00001 < x) && (x < 2.0)))
    val _ret7: Real = (1.0 / (x + 1.0))
    _ret7
  } ensuring((res) => (res +/- 1.3877676786344661e-17))

}
