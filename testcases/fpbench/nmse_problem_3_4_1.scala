import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_1 {


  def nmse_problem_3_4_1(x: Real): Real = {
    require(((x >= 0.1) && (x <= 10.0)))
    ((1.0 - cos(x)) / (x * x))
  }

}
