import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object hypot32 {


  def hypot32(x1: Real, x2: Real): Real = {
    require(((1.0 <= x1) && (x1 <= 100.0) && (1.0 <= x2) && (x2 <= 100.0)))
    sqrt(((x1 * x1) + (x2 * x2)))
  }

}
