
import daisy.lang._
import Real._

/*
  Real2Float
  From a global optimization problem
  A Numerical Evaluation of Several Stochastic Algorithms on
  Selected Continuous Global Optimization problems,
  Ali, Khompatraporn, Zabinsky, 2005

*/
object Himmilbeau {

  // f32
  def himmilbeau_32(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 5.5e-4)

  // 0.5 f32
  def himmilbeau_32_05(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 2.75e-4)

  // 0.1 f32
  def himmilbeau_32_01(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 5.5e-5)

  // 0.01 f32
  def himmilbeau_32_001(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 5.5e-6)

  // f64
  def himmilbeau_64(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 1.5e-12)

  // 0.5 f64
  def himmilbeau_64_05(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 7.5e-13)

  // 0.1 f64
  def himmilbeau_64_01(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 1.5e-13)

  // 0.01 f64
  def himmilbeau_64_001(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 1.5e-14)

  // dbldbl
  def himmilbeau_dbldbl(x1: Real, x2: Real) = {
    require(-5 <= x1 && x1 <= 5 && -5 <= x2 && x2 <= 5)
    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)
  } ensuring(res => res +/- 3.5e-28)

}