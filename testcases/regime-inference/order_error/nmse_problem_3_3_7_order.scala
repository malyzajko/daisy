import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_7_order {


  def nmse_problem_3_3_7_order(x: Real): Real = {
    require(((x >= -10.0) && (x <= 10.0)))
    val _ret10: Real = ((exp(x) - 2.0) + exp(-(x)))
    _ret10
  } ensuring((res) => (res +/- 2.683938908175685e-12))

}
