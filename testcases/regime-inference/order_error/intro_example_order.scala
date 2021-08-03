import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object intro_example_order {


  def intro_example_order(t: Real): Real = {
    require(((0.0 <= t) && (t <= 999.0)))
    val _ret: Real = (t / (t + 1.0))
    _ret
  } ensuring((res) => (res +/- 3.773640185227694e-13))

}
