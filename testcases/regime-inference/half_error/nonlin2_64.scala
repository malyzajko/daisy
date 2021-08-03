import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nonlin2_64 {


  def nonlin2_64(x: Real, y: Real): Real = {
    require(((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0)))
    val t: Real = (x * y)
    val _ret13: Real = ((t - 1.0) / ((t * t) - 1.0))
    _ret13
  } ensuring((res) => (res +/- 3.017150056695785e-11))

}
