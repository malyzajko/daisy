import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nonlin1_64 {


  def nonlin1_64(z: Real): Real = {
    require(((0.0 <= z) && (z <= 999.0)))
    val _ret12: Real = (z / (z + 1.0))
    _ret12
  } ensuring((res) => (res +/- 1.886820092613847e-12))

}
