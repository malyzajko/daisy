import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_3_order {


  def nmse_problem_3_4_3_order(eps: Real): Real = {
    require(((-0.9999 < eps) && (eps < 0.9999)))
    val _ret20: Real = log(((1.0 - eps) / (1.0 + eps)))
    _ret20
  } ensuring((res) => (res +/- 9.26064635767177e-11))

}
