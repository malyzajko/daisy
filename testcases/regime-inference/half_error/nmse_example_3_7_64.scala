import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_7_64 {


  def nmse_example_3_7_64(x: Real): Real = {
    require(((x >= -10.0) && (x <= 10.0)))
    val _ret14: Real = (exp(x) - 1.0)
    _ret14
  } ensuring((res) => (res +/- 1.2510199895923933e-11))

}
