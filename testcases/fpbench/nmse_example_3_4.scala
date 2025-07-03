import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_4 {


  def nmse_example_3_4(x: Real): Real = {
    require(((x >= 0.1) && (x <= 3.0)))
    ((1.0 - cos(x)) / sin(x))
  }

}
