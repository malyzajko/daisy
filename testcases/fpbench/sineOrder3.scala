import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sineOrder3 {


  def sineOrder3(x: Real): Real = {
    require(((-2.0 < x) && (x < 2.0)))
    ((0.954929658551372 * x) - (0.12900613773279798 * ((x * x) * x)))
  }

}
