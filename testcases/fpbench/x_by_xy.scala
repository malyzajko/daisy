import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object x_by_xy {


  def x_by_xy(x: Real, y: Real): Real = {
    require(((1.0 <= x) && (x <= 4.0) && (1.0 <= y) && (y <= 4.0)))
    (x / (x + y))
  }

}
