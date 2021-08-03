import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object polarToCarthesian_x_64 {


  def polarToCarthesian_x_64(radius: Real, theta: Real): Real = {
    require(((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0)))
    val pi: Real = 3.14159265359
    val radiant: Real = (theta * (pi / 180.0))
    val _ret2: Real = (radius * cos(radiant))
    _ret2
  } ensuring((res) => (res +/- 1.1550486309345004e-14))

}
