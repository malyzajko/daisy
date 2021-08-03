import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_2_order {


  def nmse_problem_3_3_2_order(x: Real, eps: Real): Real = {
    require(((x >= -1.5) && (x <= 1.5) && (eps >= -0.01) && (eps <= 0.01)))
    val _ret6: Real = (tan((x + eps)) - tan(x))
    _ret6
  } ensuring((res) => (res +/- 4.4412320121572934e-15))

}
