import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object turbine1 {


  def turbine1(v: Real, w: Int, r: Int): Real = {
    require(((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)))
    (((3.0 + (2.0 / (r * r))) - (((0.125 * (3.0 - (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 4.5)
  }

}
