import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object floudas2 {


  def floudas2(x1: Real, x2: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 3.0) && (0.0 <= x2) && (x2 <= 4.0) && (((((2.0 * ((x1 * x1) * (x1 * x1))) - ((8.0 * (x1 * x1)) * x1)) + ((8.0 * x1) * x1)) - x2) >= 0.0) && (((((((4.0 * ((x1 * x1) * (x1 * x1))) - ((32.0 * (x1 * x1)) * x1)) + ((88.0 * x1) * x1)) - (96.0 * x1)) + 36.0) - x2) >= 0.0)))
    (-(x1) - x2)
  }

}
