import daisy.lang._
import Real._

object UltimateUnitTest {

/*  def f(f1: Real, f2:Real) = {
    require((10 <= f1) && (f1 <= 200) && (10 <= f2) && (f2 <= 200))
    f1 * f2
  }

  def g(g1: Real): Real = {
    require((10 <= g1) && (g1 <= 200))
    f(g1, g1)
  }

  def r(r1: Real, r2:Real) = {
    require((10 <= r1) && (r1 <= 100) && (10 <= r2) && (r2 <= 100))
    r1 + r2
  }

  def p(p1: Real, p2: Real): Real = {
    require((10 <= p1) && (p1 <= 100) && (10 <= p2) && (p2 <= 100))
    g(r(p1, p2))
  }*/

  def g(g1: Real): Real = {
    require((10 <= g1) && (g1 <= 200))
    g1
  }

  def r(r1: Real): Real = {
    require((10 <= r1) && (r1 <= 200))
    r1
  }

  def f(f1: Real) = {
  require((10 <= f1) && (f1 <= 200))
   g(r(f1))
  }

  def h(h1: Real) = {
    require((10 <= h1) && (h1 <= 200))
    f(h1)
  }
}
