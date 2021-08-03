import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object matrixDeterminant_64 {


  def matrixDeterminant_64(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require(((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0)))
    val _ret5: Real = (((((a * e) * i) + ((b * f) * g)) + ((c * d) * h)) - ((((c * e) * g) + ((b * d) * i)) + ((a * f) * h)))
    _ret5
  } ensuring((res) => (res +/- 1.751487843648647e-12))

}
