import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_p42_negative {


  def nmse_p42_negative(a: Real, b: Real, c: Real): Real = {
    require(((a >= -10.0) && (a <= 10.0) && (b >= -10.0) && (b <= 10.0) && (c >= -10.0) && (c <= 10.0) && ((b * b) >= (4.0 * (a * c))) && (a != 0.0)))
    ((-(b) - sqrt(((b * b) - (4.0 * (a * c))))) / (2.0 * a))
  }

}
