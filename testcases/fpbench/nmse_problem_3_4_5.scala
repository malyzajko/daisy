import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_5 {


  def nmse_problem_3_4_5(x: Real): Real = {
    require(((x >= 0.0001) && (x <= 10.0)))
    ((x - sin(x)) / (x - tan(x)))
  }

}
