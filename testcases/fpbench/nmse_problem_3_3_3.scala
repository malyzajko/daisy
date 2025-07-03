import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_3 {


  def nmse_problem_3_3_3(x: Real): Real = {
    require(((x >= 0.0001) && (x <= 0.9999)))
    (((1.0 / (x + 1.0)) - (2.0 / x)) + (1.0 / (x - 1.0)))
  }

}
