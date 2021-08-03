import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object himmilbeau_order {


  def himmilbeau_order(x1: Real, x2: Real): Real = {
    require(((-5.0 <= x1) && (x1 <= 5.0) && (-5.0 <= x2) && (x2 <= 5.0)))
    val a: Real = (((x1 * x1) + x2) - 11.0)
    val b: Real = ((x1 + (x2 * x2)) - 7.0)
    val _ret16: Real = ((a * a) + (b * b))
    _ret16
  } ensuring((res) => (res +/- 1.0000889005823412e-13))

}
