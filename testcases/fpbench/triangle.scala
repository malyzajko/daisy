import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object triangle {


  def triangle(a: Real, b: Real, c: Real): Real = {
    require(((1.0 <= a) && (a <= 9.0) && (4.71 <= b) && (b <= 4.89) && (4.71 <= c) && (c <= 4.89)))
    val s: Real = (((a + b) + c) / 2.0)
    sqrt((((s * (s - a)) * (s - b)) * (s - c)))
  }

}
