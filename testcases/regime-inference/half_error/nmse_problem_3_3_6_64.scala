import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_6_64 {


  def nmse_problem_3_3_6_64(N: Real): Real = {
    require(((N >= 0.0001) && (N <= 10.0)))
    val _ret9: Real = (log((N + 1.0)) - log(N))
    _ret9
  } ensuring((res) => (res +/- 4.441863582501094e-12))

}
