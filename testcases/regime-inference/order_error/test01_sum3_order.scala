import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test01_sum3_order {


  def test01_sum3_order(x0: Real, x1: Real, x2: Real): Real = {
    require(((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0)))
    val p0: Real = ((x0 + x1) - x2)
    val p1: Real = ((x1 + x2) - x0)
    val p2: Real = ((x2 + x0) - x1)
    val _ret2: Real = ((p0 + p1) + p2)
    _ret2
  } ensuring((res) => (res +/- 3.552713678800501e-16))

}
