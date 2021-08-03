import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_1_64 {


  def nmse_problem_3_4_1_64(x: Real): Real = {
    require(((x >= 0.1) && (x <= 10.0)))
    val _ret18: Real = ((1.0 - cos(x)) / (x * x))
    _ret18
  } ensuring((res) => (res +/- 1.6536926692936323e-12))

}
