import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_problem_3_2_1_negative_order {


  def nmse_problem_3_2_1_negative_order(a: Real, b2: Real, c: Real): Real = {
    require(((a >= 0.001) && (a <= 10.0) && (b2 >= 20.0) && (b2 <= 30.0) && (c >= -10.0) && (c <= 10.0) && ((b2 * b2) >= (a * c)) && (a != 0.0)))
    val _ret13: Real = ((-(b2) - sqrt(((b2 * b2) - (a * c)))) / a)
    _ret13
  } ensuring((res) => (res +/- 5.379479946435702e-09))

}
