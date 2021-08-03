import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object turbine2_order {


  def turbine2_order(v: Real, w: Real, r: Real): Real = {
    require(((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)))
    val _ret7: Real = (((6.0 * v) - (((0.5 * v) * (((w * w) * r) * r)) / (1.0 - v))) - 2.5)
    _ret7
  } ensuring((res) => (res +/- 4.367865466529768e-15))

}
