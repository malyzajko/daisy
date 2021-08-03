import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_6_order {


  def nmse_problem_3_3_6_order(N: Real): Real = {
    require(((N >= 0.0001) && (N <= 10.0)))
    val _ret9: Real = (log((N + 1.0)) - log(N))
    _ret9
  } ensuring((res) => (res +/- 8.883727165002187e-13))

}
