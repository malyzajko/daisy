import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object exp1x_log_64 {


  def exp1x_log_64(x: Real): Real = {
    require(((0.01 <= x) && (x <= 0.5)))
    val _ret6: Real = ((exp(x) - 1.0) / log(exp(x)))
    _ret6
  } ensuring((res) => (res +/- 5.5655358637138217e-14))

}
