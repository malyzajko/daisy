import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_3_order {


  def nmse_problem_3_3_3_order(x: Real): Real = {
    require(((x >= 0.0001) && (x <= 0.9999)))
    val _ret7: Real = (((1.0 / (x + 1.0)) - (2.0 / x)) + (1.0 / (x - 1.0)))
    _ret7
  } ensuring((res) => (res +/- 1.1107687436508033e-09))

}
