

import daisy.lang._
import Real._


object Filters {

  def filterIteration1 (x0: Real) = {
    require(-2 <= x0 && x0 <= 2)
    (0.7) * x0
  }

  def filterIteration2 (x1: Real, x0: Real) = {
    require(-2 <= x0 && x0 <= 2 && -2 <= x1 && x1 <= 2)
    (0.7) * x1 - (1.3) * x0 + (1.4) * ((0.7) * x0)
  }

  def filterIteration3 (x2: Real, x1: Real, x0: Real) = {
    require(-2 <= x0 && x0 <= 2 && -2 <= x1 && x1 <= 2 && -2 <= x2 && x2 <= 2)

    val t1 = ((0.7) * x0)
    (0.7) * x2 - (1.3) * x1 + (1.1) * x0 + (1.4) * ((0.7) * x1 - (1.3) * x0 + (1.4) * t1) - (0.7) * t1
  }

  def filterIteration4 (x3: Real, x2: Real, x1: Real, x0: Real) = {
    require(-2 <= x0 && x0 <= 2 && -2 <= x1 && x1 <= 2 && -2 <= x2 && x2 <= 2 && -2 <= x3 && x3 <= 2)

    val t1 = (0.7) * x1 - (1.3) * x0 + (1.4) * ((0.7) * x0)

    (0.7) * x3 - (1.3) * x2 + (1.1) * x1 + (1.4) * ((0.7) * x2 - (1.3) * x1 + (1.1) * x0 + (1.4) *
      (t1) - (0.7) * ((0.7) * x0)) - (0.7) * (t1)
  }

}