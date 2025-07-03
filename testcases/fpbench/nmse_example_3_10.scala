import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_10 {


  def nmse_example_3_10(x: Real): Real = {
    require(((0.001 < x) && (x < 0.9999)))
    (log((1.0 - x)) / log((1.0 + x)))
  }

}
