import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object matrix3 {

  def determinant(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((1.0 <= a) && (a <=  10.0) && (1.0 <= b) && (b <=  10.0) && (1.0 <= c) && (c <=  10.0) &&
      (1.0 <= d) && (d <=  10.0) && (1.0 <= e) && (e <=  10.0) && (1.0 <= f) && (f <=  10.0) &&
      (1.0 <= g) && (g <=  10.0) && (1.0 <= h) && (h <=  10.0) && (1.0 <= i) && (i <=  10.0))

    (a * e * i + b * f * g + c * d * h) - (c * e * g + b * d * i + a * f * h)
  }

  def determinant_new(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((1.0 <= a) && (a <=  10.0) && (1.0 <= b) && (b <=  10.0) && (1.0 <= c) && (c <=  10.0) &&
      (1.0 <= d) && (d <=  10.0) && (1.0 <= e) && (e <=  10.0) && (1.0 <= f) && (f <=  10.0) &&
      (1.0 <= g) && (g <=  10.0) && (1.0 <= h) && (h <=  10.0) && (1.0 <= i) && (i <=  10.0))

    (a * (e * i) + (g * (b * f) + c * (d * h))) - (e * (c * g) + (i * (b * d) + a * (f * h)))
  }

  def transposedEq(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((1.0 <= a) && (a <=  10.0) && (1.0 <= b) && (b <=  10.0) && (1.0 <= c) && (c <=  10.0) &&
      (1.0 <= d) && (d <=  10.0) && (1.0 <= e) && (e <=  10.0) && (1.0 <= f) && (f <=  10.0) &&
      (1.0 <= g) && (g <=  10.0) && (1.0 <= h) && (h <=  10.0) && (1.0 <= i) && (i <=  10.0))

    val det: Real = determinant(a, b, c, d, e, f, g, h, i)
    val transDet: Real = determinant(a, d, g, b, e, h, c, f, i)

    transDet - det
  }

  def transposedEqV2(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((1.0 <= a) && (a <=  10.0) && (1.0 <= b) && (b <=  10.0) && (1.0 <= c) && (c <=  10.0) &&
      (1.0 <= d) && (d <=  10.0) && (1.0 <= e) && (e <=  10.0) && (1.0 <= f) && (f <=  10.0) &&
      (1.0 <= g) && (g <=  10.0) && (1.0 <= h) && (h <=  10.0) && (1.0 <= i) && (i <=  10.0))

    val det: Real = determinant_new(a, b, c, d, e, f, g, h, i)
    val transDet: Real = determinant_new(a, d, g, b, e, h, c, f, i)

    transDet - det
  }

  def transposedEqV3(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((3.0 <= a) && (a <=  6.0) && (3.0 <= b) && (b <=  6.0) && (3.0 <= c) && (c <=  6.0) &&
      (3.0 <= d) && (d <=  6.0) && (3.0 <= e) && (e <=  6.0) && (3.0 <= f) && (f <=  6.0) &&
      (3.0 <= g) && (g <=  6.0) && (3.0 <= h) && (h <=  6.0) && (3.0 <= i) && (i <=  6.0))

    val det: Real = determinant(a, b, c, d, e, f, g, h, i)
    val transDet: Real = determinant(a, d, g, b, e, h, c, f, i)

    transDet - det
  }

  def transposedEqV4(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((3.0 <= a) && (a <=  6.0) && (3.0 <= b) && (b <=  6.0) && (3.0 <= c) && (c <=  6.0) &&
      (3.0 <= d) && (d <=  6.0) && (3.0 <= e) && (e <=  6.0) && (3.0 <= f) && (f <=  6.0) &&
      (3.0 <= g) && (g <=  6.0) && (3.0 <= h) && (h <=  6.0) && (3.0 <= i) && (i <=  6.0))

    val det: Real = determinant_new(a, b, c, d, e, f, g, h, i)
    val transDet: Real = determinant_new(a, d, g, b, e, h, c, f, i)

    transDet - det
  }
}
