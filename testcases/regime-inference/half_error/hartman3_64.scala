import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object hartman3_64 {


  def hartman3_64(x1: Real, x2: Real, x3: Real): Real = {
    require(((0.0 <= x1) && (x1 <= 1.0) && (0.0 <= x2) && (x2 <= 1.0) && (0.0 <= x3) && (x3 <= 1.0)))
    val e1: Real = (((3.0 * ((x1 - 0.3689) * (x1 - 0.3689))) + (10.0 * ((x2 - 0.117) * (x2 - 0.117)))) + (30.0 * ((x3 - 0.2673) * (x3 - 0.2673))))
    val e2: Real = (((0.1 * ((x1 - 0.4699) * (x1 - 0.4699))) + (10.0 * ((x2 - 0.4387) * (x2 - 0.4387)))) + (35.0 * ((x3 - 0.747) * (x3 - 0.747))))
    val e3: Real = (((3.0 * ((x1 - 0.1091) * (x1 - 0.1091))) + (10.0 * ((x2 - 0.8732) * (x2 - 0.8732)))) + (30.0 * ((x3 - 0.5547) * (x3 - 0.5547))))
    val e4: Real = (((0.1 * ((x1 - 0.03815) * (x1 - 0.03815))) + (10.0 * ((x2 - 0.5743) * (x2 - 0.5743)))) + (35.0 * ((x3 - 0.8828) * (x3 - 0.8828))))
    val exp1: Real = exp(-(e1))
    val exp2: Real = exp(-(e2))
    val exp3: Real = exp(-(e3))
    val exp4: Real = exp(-(e4))
    val _ret6: Real = -(((((1.0 * exp1) + (1.2 * exp2)) + (3.0 * exp3)) + (3.2 * exp4)))
    _ret6
  } ensuring((res) => (res +/- 3.1654459915291385e-14))

}
