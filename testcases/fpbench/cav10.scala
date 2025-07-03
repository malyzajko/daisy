import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object cav10 {


  def cav10(x: Real): Real = {
    require(((0.0 < x) && (x < 10.0)))
    val temp: Real = if ((((x * x) - x) >= 0.0)) {
      (x / 10.0)
    }
    else {
      ((x * x) + 2.0)
    }
    temp
  }

}
