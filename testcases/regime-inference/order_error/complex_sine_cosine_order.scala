import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object complex_sine_cosine_order {


  def complex_sine_cosine_order(re: Real, im: Real): Real = {
    require(((re >= -10.0) && (re <= 10.0) && (im >= -10.0) && (im <= 10.0)))
    val _ret1: Real = ((0.5 * sin(re)) * (exp(-(im)) - exp(im)))
    _ret1
  } ensuring((res) => (res +/- 2.5646840628378817e-12))

}
