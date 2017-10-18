
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

  def himmilbeau(x1: Real, x2: Real) = {
    require(0.1 <= x1 && x1 <= 0.5 && -2<= x2 && x2 <= 2)

    (x1*x1 + x2 - 11)*(x1 * x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7)

  } //1.43e–12

}