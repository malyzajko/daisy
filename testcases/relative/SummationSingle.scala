

import daisy.lang._
import Real._


object SummationSingle {

  def summ(u: Real): Real = { 
    require(0.0 <= u && u <= 10)

    u + u

  }


}
