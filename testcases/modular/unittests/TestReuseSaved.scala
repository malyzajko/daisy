import daisy.lang._
import Real._
object TestReuseSaved {

  def g(g1: Real): Real = {
    require((0 <= g1) && (g1 <= 10000))
    (g1 * g1)
  }

  def f(f1: Real) = {
    require((1 <= f1) && (f1 <= 10))
    g(g(g(f1)))
  }


}
