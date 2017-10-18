import daisy.lang._
import Real._

object DopplerSingle {

  def doppler(u: Real): Real = {
    require(-100.0 <= u && u <= 100)

//    val T: Real = 50
//    val v: Real = 2000
//    val t1 = 331.4 + 0.6 * T
    -722800 / ((361.4 + u)*(361.4 + u)) // times instead of Division

  }
}