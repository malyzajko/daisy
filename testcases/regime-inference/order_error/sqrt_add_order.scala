import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sqrt_add_order {


  def sqrt_add_order(x: Real): Real = {
    require(((1.0 <= x) && (x <= 1000.0)))
    val _ret2: Real = (1.0 / (sqrt((x + 1.0)) + sqrt(x)))
    _ret2
  } ensuring((res) => (res +/- 9.164266291964679e-16))

}
