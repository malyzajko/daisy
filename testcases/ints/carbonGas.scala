import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object carbonGas {


  def carbonGas(v: Real): Real = {
    require(((0.1 <= v) && (v <= 0.5)))
    val p: Int = 35000000
    val a: Real = 0.401
    val b: Real = 4.27e-05
    val t: Int = 300
    val n: Int = 1000
    val k: Real = 1.3806503e-23
    (((p + ((a * (n / v)) * (n / v))) * (v - (n * b))) - ((k * n) * t))
  }

}
