import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object jetEngine {


  def jetEngine(x1: Real, x2: Real): Real = {
    require(((-5.0 <= x1) && (x1 <= 5.0) && (-20.0 <= x2) && (x2 <= 5.0)))
    val t: Real = ((((3.0 * x1) * x1) + (2.0 * x2)) - x1)
    val t2: Real = ((((3.0 * x1) * x1) - (2.0 * x2)) - x1)
    val d: Real = ((x1 * x1) + 1.0)
    val s: Real = (t / d)
    val s2: Real = (t2 / d)
    (x1 + (((((((((2.0 * x1) * s) * (s - 3.0)) + ((x1 * x1) * ((4.0 * s) - 6.0))) * d) + (((3.0 * x1) * x1) * s)) + ((x1 * x1) * x1)) + x1) + (3.0 * s2)))
  }

}
