import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object i4modified_order {


  def i4modified_order(x: Real, y: Real): Real = {
    require(((0.1 <= x) && (x <= 10.0) && (0.0 <= y) && (y <= 5.0)))
    val _ret14: Real = sqrt((x + (y * y)))
    _ret14
  } ensuring((res) => (res +/- 3.4463140976103824e-16))

}
