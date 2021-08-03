import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sqrt_add_64 {


  def sqrt_add_64(x: Real): Real = {
    require(((1.0 <= x) && (x <= 1000.0)))
    val _ret2: Real = (1.0 / (sqrt((x + 1.0)) + sqrt(x)))
    _ret2
  } ensuring((res) => (res +/- 4.58213314598234e-15))

}
