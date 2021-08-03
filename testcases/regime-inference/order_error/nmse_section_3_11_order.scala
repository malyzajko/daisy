import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_section_3_11_order {


  def nmse_section_3_11_order(x: Real): Real = {
    require(((x >= 0.1) && (x <= 10.0)))
    val _ret23: Real = (exp(x) / (exp(x) - 1.0))
    _ret23
  } ensuring((res) => (res +/- 4.406685331237053e-14))

}
