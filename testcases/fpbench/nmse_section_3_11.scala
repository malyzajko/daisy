import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_section_3_11 {


  def nmse_section_3_11(x: Real): Real = {
    require(((x >= 0.1) && (x <= 10.0)))
    (exp(x) / (exp(x) - 1.0))
  }

}
