import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object nmse_example_3_6_order {


  def nmse_example_3_6_order(x: Real): Real = {
    require(((x >= 0.001) && (x <= 10.0)))
    val _ret4: Real = ((1.0 / sqrt(x)) - (1.0 / sqrt((x + 1.0))))
    _ret4
  } ensuring((res) => (res +/- 1.4102176165852484e-12))

}
