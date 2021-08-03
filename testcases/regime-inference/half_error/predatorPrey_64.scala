import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object predatorPrey_64 {


  def predatorPrey_64(x: Real): Real = {
    require(((0.1 <= x) && (x <= 0.3)))
    val r: Real = 4.0
    val K: Real = 1.11
    val _ret10: Real = (((r * x) * x) / (1.0 + ((x / K) * (x / K))))
    _ret10
  } ensuring((res) => (res +/- 7.954768201283204e-17))

}
