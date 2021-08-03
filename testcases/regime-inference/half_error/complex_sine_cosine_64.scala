import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object complex_sine_cosine_64 {


  def complex_sine_cosine_64(re: Real, im: Real): Real = {
    require(((re >= -10.0) && (re <= 10.0) && (im >= -10.0) && (im <= 10.0)))
    val _ret1: Real = ((0.5 * sin(re)) * (exp(-(im)) - exp(im)))
    _ret1
  } ensuring((res) => (res +/- 1.2823420314189409e-11))

}
