import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object kepler2_order {


  def kepler2_order(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36) && (4.0 <= x5) && (x5 <= 6.36) && (4.0 <= x6) && (x6 <= 6.36)))
    val _ret10: Real = ((((((((x1 * x4) * (((((-(x1) + x2) + x3) - x4) + x5) + x6)) + ((x2 * x5) * (((((x1 - x2) + x3) + x4) - x5) + x6))) + ((x3 * x6) * (((((x1 + x2) - x3) + x4) + x5) - x6))) - ((x2 * x3) * x4)) - ((x1 * x3) * x5)) - ((x1 * x2) * x6)) - ((x4 * x5) * x6))
    _ret10
  } ensuring((res) => (res +/- 1.9581844590277344e-13))

}
