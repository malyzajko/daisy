import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object rump_from_C_64 {


  def rump_from_C_64(a: Real, b: Real): Real = {
    require(((70000.0 <= a) && (a <= 80000.0) && (30000.0 <= b) && (b <= 34000.0)))
    val b2: Real = (b * b)
    val b4: Real = (b2 * b2)
    val b6: Real = (b4 * b2)
    val b8: Real = (b4 * b4)
    val a2: Real = (a * a)
    val firstexpr: Real = (((((11.0 * a2) * b2) - b6) - (121.0 * b4)) - 2.0)
    val _ret1: Real = ((((333.75 * b6) + (a2 * firstexpr)) + (5.5 * b8)) + (a / (2.0 * b)))
    _ret1
  } ensuring((res) => (res +/- 7.640395021362779e+21))

}
