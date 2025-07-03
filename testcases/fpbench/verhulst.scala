import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object verhulst {


  def verhulst(x: Real): Real = {
    require(((0.1 <= x) && (x <= 0.3)))
    val r: Real = 4.0
    val K: Real = 1.11
    ((r * x) / (1.0 + (x / K)))
  }

}
