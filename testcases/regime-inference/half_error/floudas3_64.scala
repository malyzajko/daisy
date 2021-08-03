import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object floudas3_64 {


  def floudas3_64(x1: Real, x2: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && (((-2.0 * ((x1 * x1) * (x1 * x1))) + 2.0) >= x2)))
    val _ret5: Real = (((-12.0 * x1) - (7.0 * x2)) + (x2 * x2))
    _ret5
  } ensuring((res) => (res +/- 5.0404125317982105e-15))

}
