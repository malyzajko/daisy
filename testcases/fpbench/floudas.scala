import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object floudas {


  def floudas(x1: Real, x2: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 2.0) && (0.0 <= x2) && (x2 <= 3.0) && ((x1 + x2) <= 2.0)))
    (x1 + x2)
  }

}
