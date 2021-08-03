import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_6_64 {


  def nmse_example_3_6_64(x: Real): Real = {
    require(((x >= 0.001) && (x <= 10.0)))
    val _ret4: Real = ((1.0 / sqrt(x)) - (1.0 / sqrt((x + 1.0))))
    _ret4
  } ensuring((res) => (res +/- 7.0510880829262414e-12))

}
