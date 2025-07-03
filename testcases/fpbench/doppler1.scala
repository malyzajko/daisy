import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object doppler1 {


  def doppler1(u: Real, v: Real, T: Real): Real = {
    require(((-100.0 <= u) && (u <= 100.0) && (20.0 <= v) && (v <= 20000.0) && (-30.0 <= T) && (T <= 50.0)))
    val t1: Real = (331.4 + (0.6 * T))
    ((-(t1) * v) / ((t1 + u) * (t1 + u)))
  }

}
