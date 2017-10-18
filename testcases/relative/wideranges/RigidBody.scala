


import daisy.lang._
import Real._


object RigidBody {

  def rigidBody1(x1: Real, x2: Real, x3: Real): Real = {
    require(-1500.0 <= x1 && x1 <= -0.0001 && 0.1 <= x2 && x2 <= 15.0 && -15.0 <= x3 && x3 <= -0.1)

    -x1*x2 - 2*x2*x3 - x1 - x3
  }

  def rigidBody2(x1: Real, x2: Real, x3: Real): Real = {
    require(-1500.0 <= x1 && x1 <= -1.125 && -15.0 <= x2 && x2 <= -11.25 &&
      -15.0 <= x3 && x3 <= -11.25)

    2*(x1*x2*x3) + (3*x3*x3) - x2*(x1*x2*x3) + (3*x3*x3) - x2
  }

}