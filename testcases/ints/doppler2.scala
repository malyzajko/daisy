import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object doppler2 {


  def doppler2(u: Real, v: Int, T: Real): Real = {
    require(((-125.0 <= u) && (u <= 125.0) && (15.0 <= v) && (v <= 25000.0) && (-40.0 <= T) && (T <= 60.0)))
    val t1: Real = (331.4 + (0.6 * T))
    ((-(t1) * v) / ((t1 + u) * (t1 + u)))
  }

}
