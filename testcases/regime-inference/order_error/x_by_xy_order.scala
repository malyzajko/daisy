import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object x_by_xy_order {


  def x_by_xy_order(x: Real, y: Real): Real = {
    require(((1.0 <= x) && (x <= 4.0) && (1.0 <= y) && (y <= 4.0)))
    val _ret7: Real = (x / (x + y))
    _ret7
  } ensuring((res) => (res +/- 4.329869796038114e-17))

}
