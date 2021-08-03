import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_2_64 {


  def nmse_problem_3_3_2_64(x: Real, eps: Real): Real = {
    require(((x >= -1.5) && (x <= 1.5) && (eps >= -0.01) && (eps <= 0.01)))
    val _ret6: Real = (tan((x + eps)) - tan(x))
    _ret6
  } ensuring((res) => (res +/- 2.2206160060786467e-14))

}
