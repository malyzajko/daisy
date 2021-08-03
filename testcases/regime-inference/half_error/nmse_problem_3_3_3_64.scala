import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_3_3_64 {


  def nmse_problem_3_3_3_64(x: Real): Real = {
    require(((x >= 0.0001) && (x <= 0.9999)))
    val _ret7: Real = (((1.0 / (x + 1.0)) - (2.0 / x)) + (1.0 / (x - 1.0)))
    _ret7
  } ensuring((res) => (res +/- 5.553843718254017e-09))

}
