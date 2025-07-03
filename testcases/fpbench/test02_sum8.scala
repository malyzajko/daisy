import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test02_sum8 {


  def test02_sum8(x0: Real, x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real, x7: Real): Real = {
    require(((1.0 < x0) && (x0 < 2.0) && (1.0 < x1) && (x1 < 2.0) && (1.0 < x2) && (x2 < 2.0) && (1.0 < x3) && (x3 < 2.0) && (1.0 < x4) && (x4 < 2.0) && (1.0 < x5) && (x5 < 2.0) && (1.0 < x6) && (x6 < 2.0) && (1.0 < x7) && (x7 < 2.0)))
    (((((((x0 + x1) + x2) + x3) + x4) + x5) + x6) + x7)
  }

}
