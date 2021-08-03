import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_4_64 {


  def nmse_example_3_4_64(x: Real): Real = {
    require(((x >= 0.1) && (x <= 3.0)))
    val _ret2: Real = ((1.0 - cos(x)) / sin(x))
    _ret2
  } ensuring((res) => (res +/- 1.5236770560847745e-14))

}
