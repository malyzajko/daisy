import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object doppler3 {


  def doppler3(u: Real, v: Real, T: Real): Real = {
    require(((-30.0 <= u) && (u <= 120.0) && (320.0 <= v) && (v <= 20300.0) && (-50.0 <= T) && (T <= 30.0)))
    val t1: Real = (331.4 + (0.6 * T))
    ((-(t1) * v) / ((t1 + u) * (t1 + u)))
  }

}
