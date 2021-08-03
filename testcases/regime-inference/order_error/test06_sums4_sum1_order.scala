import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test06_sums4_sum1_order {


  def test06_sums4_sum1_order(x0: Real, x1: Real, x2: Real, x3: Real): Real = {
    require(((-1.0e-05 < x0) && (x0 < 1.00001) && (0.0 < x1) && (x1 < 1.0) && (0.0 < x2) && (x2 < 1.0) && (0.0 < x3) && (x3 < 1.0)))
    val _ret8: Real = (((x0 + x1) + x2) + x3)
    _ret8
  } ensuring((res) => (res +/- 1.3322676295501878e-16))

}
