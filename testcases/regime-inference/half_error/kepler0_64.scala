import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object kepler0_64 {


  def kepler0_64(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36) && (4.0 <= x5) && (x5 <= 6.36) && (4.0 <= x6) && (x6 <= 6.36)))
    val _ret8: Real = (((((x2 * x5) + (x3 * x6)) - (x2 * x3)) - (x5 * x6)) + (x1 * (((((-(x1) + x2) + x3) - x4) + x5) + x6)))
    _ret8
  } ensuring((res) => (res +/- 4.8490100823528336e-14))

}
