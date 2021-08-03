import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object rump_with_pow_64 {


  def rump_with_pow_64(a: Real, b: Real): Real = {
    require(((70000.0 <= a) && (a <= 80000.0) && (30000.0 <= b) && (b <= 34000.0)))
    val _ret: Real = ((((333.75 * (((((b * b) * b) * b) * b) * b)) + ((a * a) * (((((11.0 * (a * a)) * (b * b)) - (((((b * b) * b) * b) * b) * b)) - (121.0 * (((b * b) * b) * b))) - 2.0))) + (5.5 * (((((((b * b) * b) * b) * b) * b) * b) * b))) + (a / (2.0 * b)))
    _ret
  } ensuring((res) => (res +/- 6.567821157161128e+21))

}
