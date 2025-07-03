import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_3 {


  def nmse_problem_3_4_3(eps: Real): Real = {
    require(((-0.9999 < eps) && (eps < 0.9999)))
    log(((1.0 - eps) / (1.0 + eps)))
  }

}
