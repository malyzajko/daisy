import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sineOrder3_64 {


  def sineOrder3_64(x: Real): Real = {
    require(((-2.0 < x) && (x < 2.0)))
    val _ret14: Real = ((0.954929658551372 * x) - (0.12900613773279798 * ((x * x) * x)))
    _ret14
  } ensuring((res) => (res +/- 4.579884356320049e-16))

}
