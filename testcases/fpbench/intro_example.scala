import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object intro_example {


  def intro_example(t: Real): Real = {
    require(((0.0 <= t) && (t <= 999.0)))
    (t / (t + 1.0))
  }

}
