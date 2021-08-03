import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object matrixDeterminant2modified_64 {


  def matrixDeterminant2modified_64(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require(((5.0 <= a) && (a <= 50.0) && (5.0 <= b) && (b <= 50.0) && (2.0 <= c) && (c <= 12.0) && (0.0 <= d) && (d <= 5.0) && (-10.0 <= e) && (e <= -1.0) && (3.0 <= f) && (f <= 42.0) && (-10.0 <= g) && (g <= 10.0) && (-0.5 <= h) && (h <= 1.5) && (-11.0 <= i) && (i <= -8.0)))
    val _ret7: Real = (((a * (e * i)) + ((g * (b * f)) + (c * (d * h)))) - ((e * (c * g)) + ((i * (b * d)) + (a * (f * h)))))
    _ret7
  } ensuring((res) => (res +/- 9.950512636081044e-12))

}
