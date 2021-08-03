import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_5_64 {


  def nmse_problem_3_3_5_64(x: Real, eps: Real): Real = {
    require(((x >= -10.0) && (x <= 10.0) && (eps >= -1.0) && (eps <= 1.0)))
    val _ret8: Real = (cos((x + eps)) - cos(x))
    _ret8
  } ensuring((res) => (res +/- 1.7208456881689926e-15))

}
