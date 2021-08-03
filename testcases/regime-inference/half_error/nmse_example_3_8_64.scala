import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_8_64 {


  def nmse_example_3_8_64(N: Real): Real = {
    require(((N >= 0.0001) && (N <= 10.0)))
    val _ret15: Real = ((((N + 1.0) * log((N + 1.0))) - (N * log(N))) - 1.0)
    _ret15
  } ensuring((res) => (res +/- 2.776732450335518e-12))

}
