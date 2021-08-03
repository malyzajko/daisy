import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object rigidBody1_64 {


  def rigidBody1_64(x1: Real, x2: Real, x3: Real): Real = {
    require(((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0)))
    val _ret3: Real = (((-((x1 * x2)) - ((2.0 * x2) * x3)) - x1) - x3)
    _ret3
  } ensuring((res) => (res +/- 1.474376176702208e-13))

}
