import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_1_64 {


  def nmse_example_3_1_64(x: Real): Real = {
    require(((x >= 0.001) && (x <= 10.0)))
    val _ret: Real = (sqrt((x + 1.0)) - sqrt(x))
    _ret
  } ensuring((res) => (res +/- 6.966252857639077e-15))

}
