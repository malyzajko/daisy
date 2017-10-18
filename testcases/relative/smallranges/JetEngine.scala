

import daisy.lang._
import Real._


object JetEngine {


  def jetEngine(x1: Real, x2: Real): Real = {
    require(4 <= x1 && x1 <= 4.65 && 1 <= x2 && x2 <= 5)

    x1 + ((2*x1*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1))*
      ((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1) - 3) + x1*x1*(4*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1))-6))*
      (x1*x1 + 1) + 3*x1*x1*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1)) + x1*x1*x1 + x1 +
      3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)))
  }

}