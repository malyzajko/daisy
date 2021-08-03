import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object carbonGas_64 {


  def carbonGas_64(v: Real): Real = {
    require(((0.1 <= v) && (v <= 0.5)))
    val p: Real = 35000000.0
    val a: Real = 0.401
    val b: Real = 4.27e-05
    val t: Real = 300.0
    val n: Real = 1000.0
    val k: Real = 1.3806503e-23
    val _ret11: Real = (((p + ((a * (n / v)) * (n / v))) * (v - (n * b))) - ((k * n) * t))
    _ret11
  } ensuring((res) => (res +/- 4.4564135334614325e-09))

}
