import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_4_2_order {


  def nmse_problem_3_4_2_order(a: Real, b: Real, eps: Real): Real = {
    require(((a >= 5.0) && (a <= 15.0) && (b >= -15.0) && (b <= -5.0) && (eps >= 0.9) && (eps <= 1.1)))
    val _ret19: Real = ((eps * (exp(((a + b) * eps)) - 1.0)) / ((exp((a * eps)) - 1.0) * (exp((b * eps)) - 1.0)))
    _ret19
  } ensuring((res) => (res +/- 1.578608434482766e-14))

}
