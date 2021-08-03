import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test05_nonlin1_r4_64 {


  def test05_nonlin1_r4_64(x: Real): Real = {
    require(((1.00001 < x) && (x < 2.0)))
    val r1: Real = (x - 1.0)
    val r2: Real = (x * x)
    val _ret6: Real = (r1 / (r2 - 1.0))
    _ret6
  } ensuring((res) => (res +/- 4.554330442282578e-08))

}
