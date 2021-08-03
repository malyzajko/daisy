import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_10_order {


  def nmse_example_3_10_order(x: Real): Real = {
    require(((0.001 < x) && (x < 0.9999)))
    val _ret17: Real = (log((1.0 - x)) / log((1.0 + x)))
    _ret17
  } ensuring((res) => (res +/- 1.1447832758579006e-12))

}
