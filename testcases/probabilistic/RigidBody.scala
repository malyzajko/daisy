
import daisy.lang._
import Real._


object RigidBody {

  def rigidBody1(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 &&
      -15.0 <= x3 && x3 <= 15)
     -x1*x2 - 2*x2*x3 - x1 - x3

  }

  def rigidBody2(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 &&
      -15.0 <= x3 && x3 <= 15)

    val t1 = x1*x2*x3
    val t2 = 3*x3*x3
    2*(t1) + t2 - x2*(t1) + t2 - x2
  }

}