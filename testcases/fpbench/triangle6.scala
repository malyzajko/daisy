import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object triangle6 {


  def triangle6(a: Real, b: Real, c: Real): Real = {
    require(((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-06)) && ((a + c) > (b + 1.0e-06)) && ((b + c) > (a + 1.0e-06))))
    val s: Real = (((a + b) + c) / 2.0)
    sqrt((((s * (s - a)) * (s - b)) * (s - c)))
  }

}
