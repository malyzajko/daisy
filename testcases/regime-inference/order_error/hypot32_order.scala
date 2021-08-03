import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object hypot32_order {


  def hypot32_order(x1: Real, x2: Real): Real = {
    require(((1.0 <= x1) && (x1 <= 100.0) && (1.0 <= x2) && (x2 <= 100.0)))
    val _ret9: Real = sqrt(((x1 * x1) + (x2 * x2)))
    _ret9
  } ensuring((res) => (res +/- 2.509814318816891e-14))

}
