import daisy.lang._
import Real._

/*
  Real2Float
  From a global optimization problem
  A Numerical Evaluation of Several Stochastic Algorithms on
  Selected Continuous Global Optimization problems,
  Ali, Khompatraporn, Zabinsky, 2005

*/
object HimmilbeauSingle {

  def himmilbeau(x1: Real) = {
    require(-5 <= x1 && x1 <= 5)
    val x2: Real = 1 // && -5 <= x2 && x2 <= 5
    (x1*x1 + 1 - 11)*(x1 * x1 + 1 - 11) + (x1 + 1*1 - 7)*(x1 + 1*1 - 7)

  } //1.43e–12

}