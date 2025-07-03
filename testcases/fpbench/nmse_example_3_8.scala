import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_8 {


  def nmse_example_3_8(N: Real): Real = {
    require(((N >= 0.0001) && (N <= 10.0)))
    ((((N + 1.0) * log((N + 1.0))) - (N * log(N))) - 1.0)
  }

}
