import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sec4_example {


  def sec4_example(x: Real, y: Real): Real = {
    require(((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0)))
    val t: Real = (x * y)
    ((t - 1.0) / ((t * t) - 1.0))
  }

}
