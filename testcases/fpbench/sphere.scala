import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object sphere {


  def sphere(x: Real, r: Real, lat: Real, lon: Real): Real = {
    require(((-10.0 <= x) && (x <= 10.0) && (0.0 <= r) && (r <= 10.0) && (-1.570796 <= lat) && (lat <= 1.570796) && (-3.14159265 <= lon) && (lon <= 3.14159265)))
    val sinLat: Real = sin(lat)
    val cosLon: Real = cos(lon)
    (x + ((r * sinLat) * cosLon))
  }

}
