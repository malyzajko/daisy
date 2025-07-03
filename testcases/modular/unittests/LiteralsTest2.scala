import daisy.lang._
import Real._

object LiteralsTest2 {


  def g(g1: Real, g2: Real): Real = {
    require((10 <= g1) && (g1 <= 122) && (10 <= g2) && (g2 <= 122))
    g1 * g2
  }

  def r(r1: Real): Real = {
    require((10 <= r1) && (r1 <= 100))
    g(g(11.0, 11.0), r1)
  }
}
