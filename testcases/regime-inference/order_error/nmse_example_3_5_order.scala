import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_5_order {


  def nmse_example_3_5_order(N: Real): Real = {
    require(((N >= -1.0) && (N <= 1.0)))
    val _ret3: Real = (atan((N + 1.0)) - atan(N))
    _ret3
  } ensuring((res) => (res +/- 4.773959005888173e-17))

}
