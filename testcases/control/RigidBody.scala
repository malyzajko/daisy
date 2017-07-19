import daisy.lang._
import Real._


object RigidBody {

  //x1, x2, x3: < 1, 16 11>, [-15, 15]
  def out1(x1: Real, x2: Real, x3: Real): Real = {
    require(-15 <= x1 && x1 <= 15 && -15 <= x2 && x2 <= 15 && -15 <= x3 && x3 <= 15)
    -x1*x2 - 2*x2*x3 - x1 - x3
  }

  // apparently this needs 17 bits, or we've not been accurate enough
  def out2(x1: Real, x2: Real, x3: Real): Real = {
    require(-15 <= x1 && x1 <= 15 && -15 <= x2 && x2 <= 15 && -15 <= x3 && x3 <= 15)
    2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2
  }
}