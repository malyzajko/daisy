import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_5 {


  def nmse_example_3_5(N: Real): Real = {
    require(((N >= -1.0) && (N <= 1.0)))
    (atan((N + 1.0)) - atan(N))
  }

}
