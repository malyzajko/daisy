import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object squareRoot3Invalid {


  def squareRoot3Invalid(x: Real): Real = {
    require(((0.0 < x) && (x < 10.0)))
    val temp: Real = if ((x < 0.0001)) {
      (1.0 + (0.5 * x))
    }
    else {
      sqrt((1.0 + x))
    }
    temp
  }

}
