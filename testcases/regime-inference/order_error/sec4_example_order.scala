import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sec4_example_order {


  def sec4_example_order(x: Real, y: Real): Real = {
    require(((1.001 <= x) && (x <= 2.0) && (1.001 <= y) && (y <= 2.0)))
    val t: Real = (x * y)
    val _ret1: Real = ((t - 1.0) / ((t * t) - 1.0))
    _ret1
  } ensuring((res) => (res +/- 6.034300113391569e-12))

}
