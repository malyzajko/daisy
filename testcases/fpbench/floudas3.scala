import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object floudas3 {


  def floudas3(x1: Real, x2: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && (((-2.0 * ((x1 * x1) * (x1 * x1))) + 2.0) >= x2)))
    (((-12.0 * x1) - (7.0 * x2)) + (x2 * x2))
  }

}
