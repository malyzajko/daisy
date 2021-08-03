import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object n_bodyZmodified_order {


  def n_bodyZmodified_order(x0: Real, y0: Real, z0: Real, vx0: Real, vy0: Real, vz0: Real): Real = {
    require(((0.001 < x0) && (x0 < 6.0) && (0.001 < y0) && (y0 < 6.0) && (0.001 < z0) && (z0 < 0.2) && (-3.0 < vx0) && (vx0 < 3.0) && (-3.0 < vy0) && (vy0 < 3.0) && (-0.1 < vz0) && (vz0 < 0.1)))
    val dt: Real = 0.1
    val solarMass: Real = 39.47841760435743
    val distance: Real = sqrt((((x0 * x0) + (y0 * y0)) + (z0 * z0)))
    val mag: Real = (dt / ((distance * distance) * distance))
    val vzNew: Real = (vz0 - ((z0 * solarMass) * mag))
    val z_2: Real = (z0 + (dt * vzNew))
    z_2
  } ensuring((res) => (res +/- 52054.2823099679))

}
