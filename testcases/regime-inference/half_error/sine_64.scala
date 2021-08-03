import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sine_64 {


  def sine_64(x: Real): Real = {
    require(((-1.57079632679 < x) && (x < 1.57079632679)))
    val _ret12: Real = (((x - (((x * x) * x) / 6.0)) + (((((x * x) * x) * x) * x) / 120.0)) - (((((((x * x) * x) * x) * x) * x) * x) / 5040.0))
    _ret12
  } ensuring((res) => (res +/- 2.630549972666982e-16))

}
