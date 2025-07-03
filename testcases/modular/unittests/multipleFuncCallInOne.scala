import daisy.lang._
import Real._

object multipleFuncCallInOne {

  def g(g1: Real): Real = {
    require((10 <= g1) && (g1 <= 100))
    (g1 * g1)
  }

  def f(f1: Real, f2:Real) = {
    require((10 <= f1) && (f1 <= 100) && (10 <= f2) && (f2 <= 100))
    g(f1) + g(f2)
  }

  def h(h1: Real): Real = {
    require(10 <= h1 && h1 <= 100)
    g(h1) + h1 * f(h1, h1)
  }
}
