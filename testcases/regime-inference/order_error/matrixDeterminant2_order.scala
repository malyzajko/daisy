import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object matrixDeterminant2_order {


  def matrixDeterminant2_order(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require(((-10.0 <= a) && (a <= 10.0) && (-10.0 <= b) && (b <= 10.0) && (-10.0 <= c) && (c <= 10.0) && (-10.0 <= d) && (d <= 10.0) && (-10.0 <= e) && (e <= 10.0) && (-10.0 <= f) && (f <= 10.0) && (-10.0 <= g) && (g <= 10.0) && (-10.0 <= h) && (h <= 10.0) && (-10.0 <= i) && (i <= 10.0)))
    val _ret6: Real = (((a * (e * i)) + ((g * (b * f)) + (c * (d * h)))) - ((e * (c * g)) + ((i * (b * d)) + (a * (f * h)))))
    _ret6
  } ensuring((res) => (res +/- 3.502975687297294e-13))

}
