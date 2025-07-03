import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_6 {


  def nmse_example_3_6(x: Real): Real = {
    require(((x >= 0.001) && (x <= 10.0)))
    ((1.0 / sqrt(x)) - (1.0 / sqrt((x + 1.0))))
  }

}
