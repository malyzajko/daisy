import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object test04_dqmom9_64 {


  def test04_dqmom9_64(m0: Real, m1: Real, m2: Real, w0: Real, w1: Real, w2: Real, a0: Real, a1: Real, a2: Real): Real = {
    require(((-1.0 < m0) && (m0 < 1.0) && (-1.0 < m1) && (m1 < 1.0) && (-1.0 < m2) && (m2 < 1.0) && (1.0e-05 < w0) && (w0 < 1.0) && (1.0e-05 < w1) && (w1 < 1.0) && (1.0e-05 < w2) && (w2 < 1.0) && (1.0e-05 < a0) && (a0 < 1.0) && (1.0e-05 < a1) && (a1 < 1.0) && (1.0e-05 < a2) && (a2 < 1.0)))
    val v2: Real = ((w2 * (0.0 - m2)) * (-3.0 * ((1.0 * (a2 / w2)) * (a2 / w2))))
    val v1: Real = ((w1 * (0.0 - m1)) * (-3.0 * ((1.0 * (a1 / w1)) * (a1 / w1))))
    val v0: Real = ((w0 * (0.0 - m0)) * (-3.0 * ((1.0 * (a0 / w0)) * (a0 / w0))))
    val _ret5: Real = (0.0 + ((v0 * 1.0) + ((v1 * 1.0) + ((v2 * 1.0) + 0.0))))
    _ret5
  } ensuring((res) => (res +/- 0.8327165406021345))

}
