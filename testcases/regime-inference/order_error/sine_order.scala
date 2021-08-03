import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sine_order {


  def sine_order(x: Real): Real = {
    require(((-1.57079632679 < x) && (x < 1.57079632679)))
    val _ret12: Real = (((x - (((x * x) * x) / 6.0)) + (((((x * x) * x) * x) * x) / 120.0)) - (((((((x * x) * x) * x) * x) * x) * x) / 5040.0))
    _ret12
  } ensuring((res) => (res +/- 5.261099945333963e-17))

}
