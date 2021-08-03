import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object exp1x_64 {


  def exp1x_64(x: Real): Real = {
    require(((0.01 <= x) && (x <= 0.5)))
    val _ret3: Real = ((exp(x) - 1.0) / x)
    _ret3
  } ensuring((res) => (res +/- 2.2120607221585666e-14))

}
