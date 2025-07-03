import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object matrix2 {

  def determinant(a: Real, b: Real, c: Real, d: Real): Real = {
    require((1.0 <= a) && (a <=  10.0) && (1.0 <= b) && (b <=  10.0) &&
      (1.0 <= c) && (c <=  10.0) && (1.0 <= d) && (d <=  10.0))

      a * d - b * c
  }

  def transposedEqV1(a: Real, b: Real, c: Real, d: Real): Real = {
    require((1.0 <= a) && (a <=  10.0) && (1.0 <= b) && (b <=  10.0) &&
      (1.0 <= c) && (c <=  10.0) && (1.0 <= d) && (d <=  10.0))

    val det: Real = determinant(a, b, c, d)
    val trans_11: Real = a
    val trans_12: Real = c
    val trans_21: Real = b
    val trans_22: Real = d
    val transDet: Real = determinant(trans_11, trans_12, trans_21, trans_22)

    transDet - det
  }

  def transposedEqV2(a: Real, b: Real, c: Real, d: Real): Real = {
    require((3.0 <= a) && (a <= 6.0) && (3.0 <= b) && (b <=  6.0) &&
      (3.0 <= c) && (c <=  6.0) && (3.0 <= d) && (d <=  6.0))

    val det: Real = determinant(a, b, c, d)
    val trans_11: Real = a
    val trans_12: Real = c
    val trans_21: Real = b
    val trans_22: Real = d
    val transDet: Real = determinant(trans_11, trans_12, trans_21, trans_22)

    transDet - det
  }
}
