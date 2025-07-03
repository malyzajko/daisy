import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object complex_sine_cosine {


  def complex_sine_cosine(re: Real, im: Real): Real = {
    require(((re >= -10.0) && (re <= 10.0) && (im >= -10.0) && (im <= 10.0)))
    ((0.5 * sin(re)) * (exp(-(im)) - exp(im)))
  }

}
