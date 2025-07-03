import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object jacobisMethodX1 {


  def jacobisMethodX1(a11: Real, b1: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(((0.001 < a11) && (a11 < 10.0) && (0.001 < b1) && (b1 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
    val x_n1: Real = ((((b1 / a11) - ((0.1 / a11) * x2)) - ((0.2 / a11) * x3)) + ((0.3 / a11) * x4))
    x_n1
  }

}
