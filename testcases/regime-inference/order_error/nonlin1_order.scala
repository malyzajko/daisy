import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nonlin1_order {


  def nonlin1_order(z: Real): Real = {
    require(((0.0 <= z) && (z <= 999.0)))
    val _ret12: Real = (z / (z + 1.0))
    _ret12
  } ensuring((res) => (res +/- 3.773640185227694e-13))

}
