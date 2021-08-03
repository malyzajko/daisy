import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object floudas1_order {


  def floudas1_order(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 6.0) && (0.0 <= x2) && (x2 <= 6.0) && (1.0 <= x3) && (x3 <= 5.0) && (0.0 <= x4) && (x4 <= 6.0) && (0.0 <= x5) && (x5 <= 6.0) && (0.0 <= x6) && (x6 <= 10.0) && (((((x3 - 3.0) * (x3 - 3.0)) + x4) - 4.0) >= 0.0) && (((((x5 - 3.0) * (x5 - 3.0)) + x6) - 4.0) >= 0.0) && (((2.0 - x1) + (3.0 * x2)) >= 0.0) && (((2.0 + x1) - x2) >= 0.0) && (((6.0 - x1) - x2) >= 0.0) && (((x1 + x2) - 2.0) >= 0.0)))
    val _ret3: Real = ((((((-25.0 * ((x1 - 2.0) * (x1 - 2.0))) - ((x2 - 2.0) * (x2 - 2.0))) - ((x3 - 1.0) * (x3 - 1.0))) - ((x4 - 4.0) * (x4 - 4.0))) - ((x5 - 1.0) * (x5 - 1.0))) - ((x6 - 4.0) * (x6 - 4.0)))
    _ret3
  } ensuring((res) => (res +/- 4.494182803682634e-14))

}
