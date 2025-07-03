import daisy.lang._
import Real._
object FunctionAsArgExample {

  def g(g1: Real): Real = {
    require((10 <= g1) && (g1 <= 100))
    (g1 * g1)
  }

  def f(f1: Real) = {
    require((100 <= f1) && (f1 <= 10000))
    f1 * f1 * f1
  }

  def k(k1: Real): Real = {
    require(10 <= k1 && k1 <= 100)
    f(g(k1))
  }
}
