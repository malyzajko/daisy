import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_7 {


  def nmse_problem_3_3_7(x: Real): Real = {
    require(((x >= -10.0) && (x <= 10.0)))
    ((exp(x) - 2.0) + exp(-(x)))
  }

}
