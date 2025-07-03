import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object himmilbeau {


  def himmilbeau(x1: Int, x2: Real): Real = {
    require(((-5.0 <= x1) && (x1 <= 5.0) && (-5.0 <= x2) && (x2 <= 5.0)))
    val a: Real = (((x1 * x1) + x2) - 11.0)
    val b: Real = ((x1 + (x2 * x2)) - 7.0)
    ((a * a) + (b * b))
  }

}
