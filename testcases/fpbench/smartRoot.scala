import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object smartRoot {


  def smartRoot(c: Real): Real = {
    require(((c >= -10.0) && (c <= 10.0) && (-2.0 <= c) && (c <= 2.0) && ((12.25 - ((3.0 * c) * 4.0)) > 0.1)))
    val a: Real = 3.0
    val b: Real = 3.5
    val a_1: Real = 3.0
    val b_1: Real = 3.5
    val discr: Real = ((b_1 * b_1) - ((a_1 * c) * 4.0))
    val temp: Real = if ((((b_1 * b_1) - (a_1 * c)) > 10.0)) {
      val temp_1: Real = if ((b_1 > 0.0)) {
        ((c * 2.0) / (-(b_1) - sqrt(discr)))
      }
      else {
        val temp_2: Real = if ((b_1 < 0.0)) {
          ((-(b_1) + sqrt(discr)) / (a_1 * 2.0))
        }
        else {
          ((-(b_1) + sqrt(discr)) / (a_1 * 2.0))
        }
        temp_2
      }
      temp_1
    }
    else {
      ((-(b_1) + sqrt(discr)) / (a_1 * 2.0))
    }
    temp
  }

}
