

import daisy.lang._
import Real._


object Doppler {
  def doppler(u: Real, v: Real, T: Real): Real = {
    require(-100.0 <= u && u <= 1000 && 2 <= v && v <= 200000 && -300 <= T && T <= 500)

//    val t1 = 331.4 + 0.6 * T
    (- (331.4 + 0.6 * T) *v) / ((331.4 + 0.6 * T + u)*(331.4 + 0.6 * T + u))

  }


}