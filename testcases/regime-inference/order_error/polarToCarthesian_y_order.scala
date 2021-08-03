import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object polarToCarthesian_y_order {


  def polarToCarthesian_y_order(radius: Real, theta: Real): Real = {
    require(((1.0 <= radius) && (radius <= 10.0) && (0.0 <= theta) && (theta <= 360.0)))
    val pi: Real = 3.14159265359
    val radiant: Real = (theta * (pi / 180.0))
    val _ret3: Real = (radius * sin(radiant))
    _ret3
  } ensuring((res) => (res +/- 2.2930422114588534e-15))

}
