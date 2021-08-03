import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_4_64 {


  def nmse_problem_3_4_4_64(x: Real): Real = {
    require(((x >= 2.0) && (x <= 12.0)))
    val _ret21: Real = sqrt(((exp((2.0 * x)) - 1.0) / (exp(x) - 1.0)))
    _ret21
  } ensuring((res) => (res +/- 7.725405363401836e-13))

}
