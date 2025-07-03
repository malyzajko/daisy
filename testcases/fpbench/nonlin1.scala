import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nonlin1 {


  def nonlin1(z: Real): Real = {
    require(((0.0 <= z) && (z <= 999.0)))
    (z / (z + 1.0))
  }

}
