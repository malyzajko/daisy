import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object pendulum_order {


  def pendulum_order(t0: Real, w0: Real): Real = {
    require(((-2.0 < t0) && (t0 < 2.0) && (-5.0 < w0) && (w0 < 5.0)))
    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665
    val t_1: Real = t0
    val w_1: Real = w0
    val n: Real = 0.0
    val k1w: Real = ((-(g) / L) * sin(t_1))
    val k2t: Real = (w_1 + ((h / 2.0) * k1w))
    val t_3: Real = (t_1 + (h * k2t))
    t_3
  } ensuring((res) => (res +/- 4.609208950430291e-17))

}
