import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object turbine2 {


  def turbine2(v: Int, w: Int, r: Real): Real = {
    require(((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)))
    (((6.0 * v) - (((0.5 * v) * (((w * w) * r) * r)) / (1.0 - v))) - 2.5)
  }

}
