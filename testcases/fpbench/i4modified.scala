import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object i4modified {


  def i4modified(x: Real, y: Real): Real = {
    require(((0.1 <= x) && (x <= 10.0) && (0.0 <= y) && (y <= 5.0)))
    sqrt((x + (y * y)))
  }

}
