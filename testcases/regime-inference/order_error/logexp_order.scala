import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object logexp_order {


  def logexp_order(x: Real): Real = {
    require(((-8.0 <= x) && (x <= 8.0)))
    val e: Real = exp(x)
    val _ret: Real = log((1.0 + e))
    _ret
  } ensuring((res) => (res +/- 4.809923705419734e-16))

}
