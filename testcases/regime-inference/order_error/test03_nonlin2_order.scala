import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test03_nonlin2_order {


  def test03_nonlin2_order(x: Real, y: Real): Real = {
    require(((0.0 < x) && (x < 1.0) && (-1.0 < y) && (y < -0.1)))
    val _ret4: Real = ((x + y) / (x - y))
    _ret4
  } ensuring((res) => (res +/- 8.372931977381427e-16))

}
