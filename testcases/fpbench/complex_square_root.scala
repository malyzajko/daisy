import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object complex_square_root {


  def complex_square_root(re: Real, im: Real): Real = {
    require(((re >= 0.001) && (re <= 10.0) && (im >= 0.001) && (im <= 10.0)))
    (0.5 * sqrt((2.0 * (sqrt(((re * re) + (im * im))) + re))))
  }

}
