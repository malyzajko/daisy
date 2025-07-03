import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object jacobisMethodX2 {


  def jacobisMethodX2(a22: Real, b2: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(((0.001 < a22) && (a22 < 10.0) && (0.001 < b2) && (b2 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
    val x_n2: Real = ((((b2 / a22) - ((0.3 / a22) * x1)) + ((0.1 / a22) * x3)) - ((0.2 / a22) * x4))
    x_n2
  }

}
