import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_1 {


  def nmse_problem_3_3_1(x: Real): Real = {
    require(((x >= 0.001) && (x <= 1.5)))
    ((1.0 / (x + 1.0)) - (1.0 / x))
  }

}
