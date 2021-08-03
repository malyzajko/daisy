import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_3_order {


  def nmse_example_3_3_order(x: Real, eps: Real): Real = {
    require(((x >= 0.0) && (x <= 10.0) && (eps >= -1.0) && (eps <= 1.0)))
    val _ret1: Real = (sin((x + eps)) - sin(x))
    _ret1
  } ensuring((res) => (res +/- 2.8512039721654203e-16))

}
