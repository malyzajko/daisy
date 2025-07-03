import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object exp1x {


  def exp1x(x: Real): Real = {
    require(((0.01 <= x) && (x <= 0.5)))
    ((exp(x) - 1.0) / x)
  }

}
