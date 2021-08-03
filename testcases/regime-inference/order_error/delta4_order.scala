import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object delta4_order {


  def delta4_order(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.3504) && (4.0 <= x2) && (x2 <= 6.3504) && (4.0 <= x3) && (x3 <= 6.3504) && (4.0 <= x4) && (x4 <= 6.3504) && (4.0 <= x5) && (x5 <= 6.3504) && (4.0 <= x6) && (x6 <= 6.3504)))
    val _ret: Real = ((((((-(x2) * x3) - (x1 * x4)) + (x2 * x5)) + (x3 * x6)) - (x5 * x6)) + (x1 * (((((-(x1) + x2) + x3) - x4) + x5) + x6)))
    _ret
  } ensuring((res) => (res +/- 9.63984447821531e-15))

}
