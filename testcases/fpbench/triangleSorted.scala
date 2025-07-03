import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object triangleSorted {


  def triangleSorted(a: Real, b: Real, c: Real): Real = {
    require(((1.0 <= a) && (a <= 9.0) && (1.0 <= b) && (b <= 9.0) && (1.0 <= c) && (c <= 9.0) && ((a + b) > (c + 1.0e-06)) && ((a + c) > (b + 1.0e-06)) && ((b + c) > (a + 1.0e-06)) && (a < c) && (b < c)))
    val temp: Real = if ((a < b)) {
      (sqrt(((((c + (b + a)) * (a - (c - b))) * (a + (c - b))) * (c + (b - a)))) / 4.0)
    }
    else {
      (sqrt(((((c + (a + b)) * (b - (c - a))) * (b + (c - a))) * (c + (a - b)))) / 4.0)
    }
    temp
  }

}
