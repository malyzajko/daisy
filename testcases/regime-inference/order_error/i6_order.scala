import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object i6_order {


  def i6_order(x: Real, y: Real): Real = {
    require(((0.1 <= x) && (x <= 10.0) && (-5.0 <= y) && (y <= 5.0)))
    val _ret15: Real = sin((x * y))
    _ret15
  } ensuring((res) => (res +/- 1.2656542480726785e-15))

}
