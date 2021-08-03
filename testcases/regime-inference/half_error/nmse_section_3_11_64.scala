import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_section_3_11_64 {


  def nmse_section_3_11_64(x: Real): Real = {
    require(((x >= 0.1) && (x <= 10.0)))
    val _ret23: Real = (exp(x) / (exp(x) - 1.0))
    _ret23
  } ensuring((res) => (res +/- 2.2033426656185264e-13))

}
