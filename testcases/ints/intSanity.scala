import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object intSanity {

  def intSanity(v: Int, u: Int): Int = {
    require((1.0 <= v) && (v <= 6.0) && (10.0 <= u) && (u <= 200.0))
    val z: Int = 20
    val x: Int = v * 2
    (30 * u) - (v + z) + v * (u - v)
  }
}
