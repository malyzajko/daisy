import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object i6 {


  def i6(x: Real, y: Real): Real = {
    require(((0.1 <= x) && (x <= 10.0) && (-5.0 <= y) && (y <= 5.0)))
    sin((x * y))
  }

}
