import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object jacobisMethodX4 {


  def jacobisMethodX4(a44: Real, b4: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(((0.001 < a44) && (a44 < 10.0) && (0.001 < b4) && (b4 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
    val x_n4: Real = ((((b4 / a44) + ((0.1 / a44) * x1)) - ((0.2 / a44) * x2)) - ((0.3 / a44) * x3))
    x_n4
  }

}
