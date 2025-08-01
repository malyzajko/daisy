import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sum {


  def sum(x0: Real, x1: Real, x2: Real): Real = {
    require(((1.0 <= x0) && (x0 <= 2.0) && (1.0 <= x1) && (x1 <= 2.0) && (1.0 <= x2) && (x2 <= 2.0)))
    val p0: Real = ((x0 + x1) - x2)
    val p1: Real = ((x1 + x2) - x0)
    val p2: Real = ((x2 + x0) - x1)
    ((p0 + p1) + p2)
  }

}
