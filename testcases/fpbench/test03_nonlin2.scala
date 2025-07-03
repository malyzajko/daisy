import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test03_nonlin2 {


  def test03_nonlin2(x: Real, y: Real): Real = {
    require(((0.0 < x) && (x < 1.0) && (-1.0 < y) && (y < -0.1)))
    ((x + y) / (x - y))
  }

}
