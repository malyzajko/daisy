import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object intro_example_64 {


  def intro_example_64(t: Real): Real = {
    require(((0.0 <= t) && (t <= 999.0)))
    val _ret: Real = (t / (t + 1.0))
    _ret
  } ensuring((res) => (res +/- 1.886820092613847e-12))

}
