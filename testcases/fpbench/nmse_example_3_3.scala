import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_3 {


  def nmse_example_3_3(x: Real, eps: Real): Real = {
    require(((x >= 0.0) && (x <= 10.0) && (eps >= -1.0) && (eps <= 1.0)))
    (sin((x + eps)) - sin(x))
  }

}
