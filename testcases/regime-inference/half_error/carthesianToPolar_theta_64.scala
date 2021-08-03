import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object carthesianToPolar_theta_64 {


  def carthesianToPolar_theta_64(x: Real, y: Real): Real = {
    require(((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0)))
    val pi: Real = 3.14159265359
    val radiant: Real = atan((y / x))
    val _ret1: Real = (radiant * (180.0 / pi))
    _ret1
  } ensuring((res) => (res +/- 4.493750313166275e-12))

}
