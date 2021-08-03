import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sineOrder3_order {


  def sineOrder3_order(x: Real): Real = {
    require(((-2.0 < x) && (x < 2.0)))
    val _ret14: Real = ((0.954929658551372 * x) - (0.12900613773279798 * ((x * x) * x)))
    _ret14
  } ensuring((res) => (res +/- 9.1597687126401e-17))

}
