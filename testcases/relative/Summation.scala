

import daisy.lang._
import Real._


object Summation {

  def summ(u: Real, v: Real): Real = { //
    require(2.0 <= u && u <= 10 && 1 <= v && v <= 10)//

    u + u * u

  }


}
