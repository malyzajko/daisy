import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object rigidBody2_64 {


  def rigidBody2_64(x1: Real, x2: Real, x3: Real): Real = {
    require(((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0)))
    val _ret4: Real = (((((((2.0 * x1) * x2) * x3) + ((3.0 * x3) * x3)) - (((x2 * x1) * x2) * x3)) + ((3.0 * x3) * x3)) - x2)
    _ret4
  } ensuring((res) => (res +/- 1.8033130544381493e-11))

}
