import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object kepler1_64 {


  def kepler1_64(x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36)))
    val _ret9: Real = ((((((((x1 * x4) * (((-(x1) + x2) + x3) - x4)) + (x2 * (((x1 - x2) + x3) + x4))) + (x3 * (((x1 + x2) - x3) + x4))) - ((x2 * x3) * x4)) - (x1 * x3)) - (x1 * x2)) - x4)
    _ret9
  } ensuring((res) => (res +/- 1.9112687728769138e-13))

}
