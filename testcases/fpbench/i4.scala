import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object i4 {


  def i4(x: Real, y: Real): Real = {
    require(((0.1 <= x) && (x <= 10.0) && (-5.0 <= y) && (y <= 5.0)))
    sqrt((x + (y * y)))
  }

}
