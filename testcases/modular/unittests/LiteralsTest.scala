import daisy.lang._
import Real._

object LiteralsTest {


  def g(g1: Real): Real = {
    require((10 <= g1) && (g1 <= 200))
    g1 * g1
  }

  def r(r1: Real): Real = {
    require((10 <= r1) && (r1 <= 100))
    g(11.0) + r1
  }
}
