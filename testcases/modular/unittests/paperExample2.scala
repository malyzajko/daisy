import daisy.lang._
import Real._

object paperExample2 {

  def g(g1: Real): Real = {
    require((20 <= g1) && (g1 <= 200))
    (g1 * g1)
  }

  def r(r1: Real, r2:Real) = {
    require((10 <= r1) && (r1 <= 100) && (10 <= r2) && (r2 <= 100))
      r1 + r2
  }

  def p(p1: Real, p2: Real): Real = {
    require((10 <= p1) && (p1 <= 100) && (10 <= p2) && (p2 <= 100))
    g(r(p1, p2))
  }
}
