import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sqrt_add {


  def sqrt_add(x: Real): Real = {
    require(((1.0 <= x) && (x <= 1000.0)))
    (1.0 / (sqrt((x + 1.0)) + sqrt(x)))
  }

}
