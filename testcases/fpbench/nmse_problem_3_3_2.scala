import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_2 {


  def nmse_problem_3_3_2(x: Real, eps: Real): Real = {
    require(((x >= -1.5) && (x <= 1.5) && (eps >= -0.01) && (eps <= 0.01)))
    (tan((x + eps)) - tan(x))
  }

}
