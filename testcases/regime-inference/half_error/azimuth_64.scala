import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object azimuth_64 {


  def azimuth_64(lat1: Real, lat2: Real, lon1: Real, lon2: Real): Real = {
    require(((0.0 <= lat1) && (lat1 <= 0.4) && (0.5 <= lat2) && (lat2 <= 1.0) && (0.0 <= lon1) && (lon1 <= 3.14159265) && (-3.14159265 <= lon2) && (lon2 <= -0.5)))
    val dLon: Real = (lon2 - lon1)
    val s_lat1: Real = sin(lat1)
    val c_lat1: Real = cos(lat1)
    val s_lat2: Real = sin(lat2)
    val c_lat2: Real = cos(lat2)
    val s_dLon: Real = sin(dLon)
    val c_dLon: Real = cos(dLon)
    val _ret2: Real = atan(((c_lat2 * s_dLon) / ((c_lat1 * s_lat2) - ((s_lat1 * c_lat2) * c_dLon))))
    _ret2
  } ensuring((res) => (res +/- 4.5759847030122484e-14))

}
