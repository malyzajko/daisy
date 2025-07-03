import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object carthesianToPolar_theta {


  def carthesianToPolar_theta(x: Real, y: Real): Real = {
    require(((1.0 <= x) && (x <= 100.0) && (1.0 <= y) && (y <= 100.0)))
    val pi: Real = 3.14159265359
    val radiant: Real = atan((y / x))
    (radiant * (180.0 / pi))
  }

}
