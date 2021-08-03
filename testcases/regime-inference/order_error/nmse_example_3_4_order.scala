import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_4_order {


  def nmse_example_3_4_order(x: Real): Real = {
    require(((x >= 0.1) && (x <= 3.0)))
    val _ret2: Real = ((1.0 - cos(x)) / sin(x))
    _ret2
  } ensuring((res) => (res +/- 3.047354112169549e-15))

}
