import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sqroot_64 {


  def sqroot_64(x: Real): Real = {
    require(((0.0 <= x) && (x <= 1.0)))
    val _ret13: Real = ((((1.0 + (0.5 * x)) - ((0.125 * x) * x)) + (((0.0625 * x) * x) * x)) - ((((0.0390625 * x) * x) * x) * x))
    _ret13
  } ensuring((res) => (res +/- 2.6090075907266464e-16))

}
