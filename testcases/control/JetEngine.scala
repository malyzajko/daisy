import daisy.lang._
import Real._


object JetEngine {

  // x1: <1, 16, 12>    x2: <1, 16, 10>
  def out(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -20 <= x2 && x2 <= 5)
    x1 + (
      (2*x1*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1))*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1) - 3) +
     x1*x1*(4*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1))-6))
      *(x1*x1 + 1) +
    3*x1*x1*((3*x1*x1 + 2*x2 - x1)/(x1*x1 + 1)) + x1*x1*x1 + x1 + 3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)))

  }



}