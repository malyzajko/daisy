import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_9_order {


  def nmse_example_3_9_order(x: Real): Real = {
    require(((x >= 0.001) && (x <= 1.5)))
    val _ret16: Real = ((1.0 / x) - (1.0 / tan(x)))
    _ret16
  } ensuring((res) => (res +/- 1.5049609296690161e-12))

}
