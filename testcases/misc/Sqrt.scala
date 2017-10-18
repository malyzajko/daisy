

import daisy.lang._
import Real._


object SqrtApproximations {


  def sqroot(x: Real): Real = {
    require(x >= 0.0 && x < 10.0)
    1.0 + 0.5*x - 0.125*x*x + 0.0625*x*x*x - 0.0390625*x*x*x*x
  }


}