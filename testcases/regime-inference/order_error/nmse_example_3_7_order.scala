import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_7_order {


  def nmse_example_3_7_order(x: Real): Real = {
    require(((x >= -10.0) && (x <= 10.0)))
    val _ret14: Real = (exp(x) - 1.0)
    _ret14
  } ensuring((res) => (res +/- 2.5020399791847868e-12))

}
