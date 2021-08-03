import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_3_64 {


  def nmse_example_3_3_64(x: Real, eps: Real): Real = {
    require(((x >= 0.0) && (x <= 10.0) && (eps >= -1.0) && (eps <= 1.0)))
    val _ret1: Real = (sin((x + eps)) - sin(x))
    _ret1
  } ensuring((res) => (res +/- 1.4256019860827102e-15))

}
