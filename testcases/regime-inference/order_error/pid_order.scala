import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object pid_order {


  def pid_order(m: Real, c: Real): Real = {
    require(((-10.0 < m) && (m < 10.0) && (-10.0 < c) && (c < 10.0)))
    val ki: Real = 0.69006
    val kp: Real = 9.4514
    val kd: Real = 2.8454
    val i_0: Real = 0.0
    val dt: Real = 0.2
    val invdt: Real = 5.0
    val eold: Real = 0.0
    val e_1: Real = (c - m)
    val p_1: Real = (kp * e_1)
    val i_1: Real = (i_0 + ((ki * dt) * e_1))
    val d_1: Real = ((kd * invdt) * (e_1 - eold))
    val r_1: Real = ((p_1 + i_1) + d_1)
    val _ret1: Real = (m + (0.01 * r_1))
    _ret1
  } ensuring((res) => (res +/- 4.3066119331802713e-16))

}
