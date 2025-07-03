import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_9 {


  def nmse_example_3_9(x: Real): Real = {
    require(((x >= 0.001) && (x <= 1.5)))
    ((1.0 / x) - (1.0 / tan(x)))
  }

}
