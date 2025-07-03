import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_5 {


  def nmse_problem_3_3_5(x: Real, eps: Real): Real = {
    require(((x >= -10.0) && (x <= 10.0) && (eps >= -1.0) && (eps <= 1.0)))
    (cos((x + eps)) - cos(x))
  }

}
