import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test05_nonlin1_test2_64 {


  def test05_nonlin1_test2_64(x: Real): Real = {
    require(((1.00001 < x) && (x < 2.0)))
    val _ret7: Real = (1.0 / (x + 1.0))
    _ret7
  } ensuring((res) => (res +/- 6.93883839317233e-17))

}
