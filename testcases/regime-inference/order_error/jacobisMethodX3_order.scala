import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object jacobisMethodX3_order {


  def jacobisMethodX3_order(a33: Real, b3: Real, x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(((0.001 < a33) && (a33 < 10.0) && (0.001 < b3) && (b3 < 10.0) && (0.0 < x1) && (x1 < 10.0) && (0.0 < x2) && (x2 < 10.0) && (0.0 < x3) && (x3 < 10.0) && (0.0 < x4) && (x4 < 10.0)))
    val x_n3: Real = ((((b3 / a33) - ((0.2 / a33) * x1)) + ((0.3 / a33) * x2)) - ((0.1 / a33) * x4))
    x_n3
  } ensuring((res) => (res +/- 1.155246359687387e-09))

}
