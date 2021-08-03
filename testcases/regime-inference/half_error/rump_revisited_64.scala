import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object rump_revisited_64 {


  def rump_revisited_64(a: Real, b: Real): Real = {
    require(((70000.0 <= a) && (a <= 80000.0) && (30000.0 <= b) && (b <= 34000.0)))
    val b2: Real = (b * b)
    val b4: Real = (b2 * b2)
    val b6: Real = (b4 * b2)
    val b8: Real = (b4 * b4)
    val a2: Real = (a * a)
    val firstexpr: Real = ((((11.0 * a2) * b2) - (121.0 * b4)) - 2.0)
    val _ret2: Real = (((((333.75 - a2) * b6) + (a2 * firstexpr)) + (5.5 * b8)) + (a / (2.0 * b)))
    _ret2
  } ensuring((res) => (res +/- 6.939979835484373e+21))

}
