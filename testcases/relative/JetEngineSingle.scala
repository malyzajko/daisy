

import daisy.lang._
import Real._

object JetEngineSingle {
  def jetEngine(x1: Real): Real = {
    require(-5 <= x1 && x1 <= 5)
//    val t = (3*x1*x1 + 2*x1 - x1)
//    x1 + ((2*x1*(t/(x1*x1 + 1))*
//      (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
//      (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
//      3*((3*x1*x1 + 2*x1 -x1)/(x1*x1 + 1)))

    x1 + ((2*x1*((3*x1*x1 + 2*x1 - x1)/(x1*x1 + 1))*
      ((3*x1*x1 + 2*x1 - x1)/(x1*x1 + 1) - 3) + x1*x1*(4*((3*x1*x1 + 2*x1 - x1)/(x1*x1 + 1))-6))*
      (x1*x1 + 1) + 3*x1*x1*((3*x1*x1 + 2*x1 - x1)/(x1*x1 + 1)) + x1*x1*x1 + x1 +
      3*((3*x1*x1 + 2*x1 -x1)/(x1*x1 + 1)))

  }
}