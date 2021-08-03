import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object exp1x_order {


  def exp1x_order(x: Real): Real = {
    require(((0.01 <= x) && (x <= 0.5)))
    val _ret3: Real = ((exp(x) - 1.0) / x)
    _ret3
  } ensuring((res) => (res +/- 4.424121444317134e-15))

}
