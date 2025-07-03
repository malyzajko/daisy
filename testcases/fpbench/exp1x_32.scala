import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object exp1x_32 {


  def exp1x_32(x: Real): Real = {
    require(((0.01 <= x) && (x <= 0.5)))
    ((exp(x) - 1.0) / x)
  }

}
