import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object polarToCarthesian_x {


  def polarToCarthesian_x(radius: Real, theta: Real): Real = {
    require(((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0)))
    val pi: Real = 3.14159265359
    val radiant: Real = (theta * (pi / 180.0))
    (radius * cos(radiant))
  }

}
