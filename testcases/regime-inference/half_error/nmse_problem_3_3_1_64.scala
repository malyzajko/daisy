import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_1_64 {


  def nmse_problem_3_3_1_64(x: Real): Real = {
    require(((x >= 0.001) && (x <= 1.5)))
    val _ret5: Real = ((1.0 / (x + 1.0)) - (1.0 / x))
    _ret5
  } ensuring((res) => (res +/- 5.5568022405706574e-11))

}
