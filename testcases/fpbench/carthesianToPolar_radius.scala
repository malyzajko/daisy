import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object carthesianToPolar_radius {


  def carthesianToPolar_radius(x: Real, y: Real): Real = {
    require(((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0)))
    sqrt(((x * x) + (y * y)))
  }

}
