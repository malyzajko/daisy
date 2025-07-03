import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sine {


  def sine(x: Real): Real = {
    require(((-1.57079632679 < x) && (x < 1.57079632679)))
    (((x - (((x * x) * x) / 6.0)) + (((((x * x) * x) * x) * x) / 120.0)) - (((((((x * x) * x) * x) * x) * x) * x) / 5040.0))
  }

}
