import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_7 {


  def nmse_example_3_7(x: Real): Real = {
    require(((x >= -10.0) && (x <= 10.0)))
    (exp(x) - 1.0)
  }

}
