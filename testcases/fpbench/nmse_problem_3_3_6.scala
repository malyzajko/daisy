import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_6 {


  def nmse_problem_3_3_6(N: Real): Real = {
    require(((N >= 0.0001) && (N <= 10.0)))
    (log((N + 1.0)) - log(N))
  }

}
