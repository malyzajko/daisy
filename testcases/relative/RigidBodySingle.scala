import daisy.lang._
import Real._

object RigidBodySingle {
  def rigidBody1(x1: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 ) // -3.75 3.75
    -x1*x1 - 2*x1*x1 - x1 - x1
  }
  def rigidBody2(x1: Real): Real = {
    require(-15 <= x1 && x1 <= 15.0)
    2*(x1*x1*x1) + (3*x1*x1) - x1*(x1*x1*x1) + (3*x1*x1) - x1
  }
}