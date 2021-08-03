import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object himmilbeau_64 {


  def himmilbeau_64(x1: Real, x2: Real): Real = {
    require(((-5.0 <= x1) && (x1 <= 5.0) && (-5.0 <= x2) && (x2 <= 5.0)))
    val a: Real = (((x1 * x1) + x2) - 11.0)
    val b: Real = ((x1 + (x2 * x2)) - 7.0)
    val _ret16: Real = ((a * a) + (b * b))
    _ret16
  } ensuring((res) => (res +/- 5.000444502911706e-13))

}
