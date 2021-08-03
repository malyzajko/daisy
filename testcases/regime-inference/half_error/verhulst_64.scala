import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object verhulst_64 {


  def verhulst_64(x: Real): Real = {
    require(((0.1 <= x) && (x <= 0.3)))
    val r: Real = 4.0
    val K: Real = 1.11
    val _ret9: Real = ((r * x) / (1.0 + (x / K)))
    _ret9
  } ensuring((res) => (res +/- 1.2553930673442836e-16))

}
