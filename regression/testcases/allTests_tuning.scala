import scala.annotation.strictfp
import daisy.lang._
import Real._

@strictfp
object allTests_tuning {

  // 0.5 f64
  def doppler_64_05(u: Real, v: Real, T: Real): Real = {
    require(-100.0 <= u && u <= 100 && 20 <= v && v <= 20000 && -30 <= T && T <= 50)

    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))

  } ensuring(res => res +/- 2.25e-13)


  // 0.5 f64
  def kepler0_64_05(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(4 <= x1 && x1 <= 6.36 && 4 <= x2 && x2 <= 6.36 && 4 <= x3 && x3 <= 6.36 &&
      4 <= x4 && x4 <= 6.36 && 4 <= x5 && x5 <= 6.36 && 4 <= x6 && x6 <= 6.36)
    x2 * x5 + x3 * x6 - x2 * x3 - x5 * x6 + x1 * (-x1 + x2 + x3 - x4 + x5 + x6)
  } ensuring(res => res +/- 4.75e-14)


  // 0.5 f64
  def rigidBody1_64_05(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 && -15.0 <= x3 && x3 <= 15)

    -x1*x2 - 2*x2*x3 - x1 - x3
    //(-((x1 * x2)) - (x1 + x3)) - ((x2 * 2.0) * x3) (paper rewriting expression)

  } ensuring(res => res +/- 1.75e-13)

  // 0.5 f64
  def rigidBody2_64_05(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 &&
      -15.0 <= x3 && x3 <= 15)
    2*(x1*x2*x3) + (3*x3*x3) - x2*(x1*x2*x3) + (3*x3*x3) - x2
  } ensuring(res => res +/- 2e-11)

  // 0.5 f64
  def turbine1_64_05(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5
  } ensuring(res => res +/- 4.5e-14)

  // 0.5 f64
  def turbine2_64_05(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 7.5e-14)

  // 0.5 f64
  def turbine3_64_05(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5
  } ensuring(res => res +/- 3.25e-14)

}