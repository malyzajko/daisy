import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object carthesianToPolar_radius_64 {


  def carthesianToPolar_radius_64(x: Real, y: Real): Real = {
    require(((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0)))
    val _ret: Real = sqrt(((x * x) + (y * y)))
    _ret
  } ensuring((res) => (res +/- 1.2549071594084455e-13))

}
