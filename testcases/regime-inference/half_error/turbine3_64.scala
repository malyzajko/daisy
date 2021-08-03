import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object turbine3_64 {


  def turbine3_64(v: Real, w: Real, r: Real): Real = {
    require(((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)))
    val _ret8: Real = (((3.0 - (2.0 / (r * r))) - (((0.125 * (1.0 + (2.0 * v))) * (((w * w) * r) * r)) / (1.0 - v))) - 0.5)
    _ret8
  } ensuring((res) => (res +/- 8.266203045255964e-15))

}
