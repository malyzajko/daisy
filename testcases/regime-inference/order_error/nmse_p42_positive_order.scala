import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_p42_positive_order {


  def nmse_p42_positive_order(a: Real, b: Real, c: Real): Real = {
    require(((a >= 0.001) && (a <= 10.0) && (b >= 70.0) && (b <= 120.0) && (c >= -10.0) && (c <= 10.0) && ((b * b) >= (4.0 * (a * c))) && (a != 0.0)))
    val _ret11: Real = ((-(b) + sqrt(((b * b) - (4.0 * (a * c))))) / (2.0 * a))
    _ret11
  } ensuring((res) => (res +/- 7.837119582900882e-10))

}
