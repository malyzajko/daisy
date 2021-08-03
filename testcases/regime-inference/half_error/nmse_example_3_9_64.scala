import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_9_64 {


  def nmse_example_3_9_64(x: Real): Real = {
    require(((x >= 0.001) && (x <= 1.5)))
    val _ret16: Real = ((1.0 / x) - (1.0 / tan(x)))
    _ret16
  } ensuring((res) => (res +/- 7.524804648345081e-12))

}
