import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object floudas2_order {


  def floudas2_order(x1: Real, x2: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 3.0) && (0.0 <= x2) && (x2 <= 4.0) && (((((2.0 * ((x1 * x1) * (x1 * x1))) - ((8.0 * (x1 * x1)) * x1)) + ((8.0 * x1) * x1)) - x2) >= 0.0) && (((((((4.0 * ((x1 * x1) * (x1 * x1))) - ((32.0 * (x1 * x1)) * x1)) + ((88.0 * x1) * x1)) - (96.0 * x1)) + 36.0) - x2) >= 0.0)))
    val _ret4: Real = (-(x1) - x2)
    _ret4
  } ensuring((res) => (res +/- 1.1102230246251565e-16))

}
