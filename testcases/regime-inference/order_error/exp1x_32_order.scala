import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object exp1x_32_order {


  def exp1x_32_order(x: Real): Real = {
    require(((0.01 <= x) && (x <= 0.5)))
    val _ret4: Real = ((exp(x) - 1.0) / x)
    _ret4
  } ensuring((res) => (res +/- 4.424121444317134e-15))

}
