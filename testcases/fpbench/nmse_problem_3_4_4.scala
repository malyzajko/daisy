import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_4 {


  def nmse_problem_3_4_4(x: Real): Real = {
    require(((x >= 2.0) && (x <= 12.0)))
    sqrt(((exp((2.0 * x)) - 1.0) / (exp(x) - 1.0)))
  }

}
