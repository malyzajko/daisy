import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object floudas_order {


  def floudas_order(x1: Real, x2: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && ((x1 + x2) <= 2.0)))
    val _ret5: Real = (x1 + x2)
    _ret5
  } ensuring((res) => (res +/- 6.661338147750939e-17))

}
