import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_section_3_5 {


  def nmse_section_3_5(a: Real, x: Real): Real = {
    require(((a >= -10.0) && (a <= 10.0) && (x >= -10.0) && (x <= 10.0)))
    (exp((a * x)) - 1.0)
  }

}
