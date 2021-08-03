import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object bspline3_64 {


  def bspline3_64(u: Real): Real = {
    require(((0.0 <= u) && (u <= 1.0)))
    val _ret15: Real = (-(((u * u) * u)) / 6.0)
    _ret15
  } ensuring((res) => (res +/- 5.3198186596622086e-17))

}
