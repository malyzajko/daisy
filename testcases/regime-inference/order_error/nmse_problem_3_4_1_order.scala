import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_1_order {


  def nmse_problem_3_4_1_order(x: Real): Real = {
    require(((x >= 0.1) && (x <= 10.0)))
    val _ret18: Real = ((1.0 - cos(x)) / (x * x))
    _ret18
  } ensuring((res) => (res +/- 3.3073853385872643e-13))

}
