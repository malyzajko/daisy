import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_5_64 {


  def nmse_example_3_5_64(N: Real): Real = {
    require(((N >= -1.0) && (N <= 1.0)))
    val _ret3: Real = (atan((N + 1.0)) - atan(N))
    _ret3
  } ensuring((res) => (res +/- 2.3869795029440865e-16))

}
