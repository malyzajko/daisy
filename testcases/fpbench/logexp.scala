import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object logexp {


  def logexp(x: Real): Real = {
    require(((-8.0 <= x) && (x <= 8.0)))
    val e: Real = exp(x)
    log((1.0 + e))
  }

}
