import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object bspline3 {


  def bspline3(u: Real): Real = {
    require(((0.0 <= u) && (u <= 1.0)))
    (-(((u * u) * u)) / 6.0)
  }

}
