import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object bspline3_order {


  def bspline3_order(u: Real): Real = {
    require(((0.0 <= u) && (u <= 1.0)))
    val _ret15: Real = (-(((u * u) * u)) / 6.0)
    _ret15
  } ensuring((res) => (res +/- 1.0639637319324418e-17))

}
