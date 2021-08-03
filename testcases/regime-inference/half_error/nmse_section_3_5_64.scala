import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_section_3_5_64 {


  def nmse_section_3_5_64(a: Real, x: Real): Real = {
    require(((a >= -10.0) && (a <= 10.0) && (x >= -10.0) && (x <= 10.0)))
    val _ret22: Real = (exp((a * x)) - 1.0)
    _ret22
  } ensuring((res) => (res +/- 3.379676890160464e+29))

}
