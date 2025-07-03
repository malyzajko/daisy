import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object delta {


  def delta(x1: Int, x2: Int, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.3504) && (4.0 <= x2) && (x2 <= 6.3504) && (4.0 <= x3) &&
      (x3 <= 6.3504) && (4.0 <= x4) && (x4 <= 6.3504) && (4.0 <= x5) && (x5 <= 6.3504) && (4.0 <= x6) && (x6 <= 6.3504)))
    ((((((((x1 * x4) * (((((-(x1) + x2) + x3) - x4) + x5) + x6)) + ((x2 * x5) * (((((x1 - x2) + x3) + x4) - x5) + x6))) +
      ((x3 * x6) * (((((x1 + x2) - x3) + x4) + x5) - x6))) + ((-(x2) * x3) * x4)) +
      ((-(x1) * x3) * x5)) + ((-(x1) * x2) * x6)) + ((-(x4) * x5) * x6))
  }

}
