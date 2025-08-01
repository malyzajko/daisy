import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object runge_kutta_4 {


  def runge_kutta_4(h: Real, y_n: Real, c: Real): Real = {
    require(((0.0 <= y_n) && (y_n <= 100.0) && (1.0e-05 < h) && (h < 0.1) && (50.0 < c) && (c < 200.0)))
    val sixieme: Real = 0.0
    val eps: Real = 0.005
    val k: Real = 1.2
    val v_1: Real = (c - y_n)
    val k1: Real = ((k * v_1) * v_1)
    val v_2: Real = (c - (y_n + ((0.5 * h) * k1)))
    val k2: Real = ((k * v_2) * v_2)
    val v_3: Real = (c - (y_n + ((0.5 * h) * k2)))
    val k3: Real = ((k * v_3) * v_3)
    val v_4: Real = (c - (y_n + (h * k3)))
    val k4: Real = ((k * v_4) * v_4)
    (y_n + ((sixieme * h) * (((k1 + (2.0 * k2)) + (2.0 * k3)) + k4)))
  }

}
