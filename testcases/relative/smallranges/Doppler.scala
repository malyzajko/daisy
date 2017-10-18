

import daisy.lang._
import Real._


object Doppler {
  def doppler(u: Real, v: Real, T: Real): Real = {
    require(-100.0 <= u && u <= 100 && 20 <= v && v <= 20000 && -30 <= T && T <= 50)

//    val t1 = 331.4 + 0.6 * T
    (- (331.4 + 0.6 * T) *v) / ((331.4 + 0.6 * T + u)*(331.4 + 0.6 * T + u))

  }


}