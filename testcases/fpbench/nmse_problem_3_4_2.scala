import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_2 {


  def nmse_problem_3_4_2(a: Real, b: Real, eps: Real): Real = {
    require(((a >= 5.0) && (a <= 15.0) && (b >= -15.0) && (b <= -5.0) && (eps >= 0.9) && (eps <= 1.1)))
    ((eps * (exp(((a + b) * eps)) - 1.0)) / ((exp((a * eps)) - 1.0) * (exp((b * eps)) - 1.0)))
  }

}
