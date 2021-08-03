import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sine_newton_order {


  def sine_newton_order(x0: Real): Real = {
    require(((-1.0 < x0) && (x0 < 1.0)))
    val x_2: Real = (x0 - ((((x0 - (((x0 * x0) * x0) / 6.0)) + (((((x0 * x0) * x0) * x0) * x0) / 120.0)) + (((((((x0 * x0) * x0) * x0) * x0) * x0) * x0) / 5040.0)) / (((1.0 - ((x0 * x0) / 2.0)) + ((((x0 * x0) * x0) * x0) / 24.0)) + ((((((x0 * x0) * x0) * x0) * x0) * x0) / 720.0))))
    x_2
  } ensuring((res) => (res +/- 1.7935577169868928e-16))

}
