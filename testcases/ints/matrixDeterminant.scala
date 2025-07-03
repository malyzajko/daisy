import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object matrixDeterminant {


  def matrixDeterminant(a: Int, b: Real, c: Real, d: Real, e: Real, f: Int, g: Real, h: Real, i: Real): Real = {
    require(((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0)))
    (((((a * e) * i) + ((b * f) * g)) + ((c * d) * h)) - ((((c * e) * g) + ((b * d) * i)) + ((a * f) * h)))
  }

}
