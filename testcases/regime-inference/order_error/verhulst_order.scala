import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object verhulst_order {


  def verhulst_order(x: Real): Real = {
    require(((0.1 <= x) && (x <= 0.3)))
    val r: Real = 4.0
    val K: Real = 1.11
    val _ret9: Real = ((r * x) / (1.0 + (x / K)))
    _ret9
  } ensuring((res) => (res +/- 2.510786134688567e-17))

}
