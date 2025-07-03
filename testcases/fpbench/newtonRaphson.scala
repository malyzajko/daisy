import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object newtonRaphson {


  def newtonRaphson(x: Real): Real = {
    require(((0.0 < x) && (x < 3.0)))
    val f: Real = (((((((x * x) * ((x * x) * x)) - ((10.0 * x) * ((x * x) * x))) + ((40.0 * x) * (x * x))) - ((80.0 * x) * x)) + (80.0 * x)) - 32.0)
    val ff: Real = ((((((5.0 * x) * ((x * x) * x)) - ((40.0 * x) * (x * x))) + ((120.0 * x) * x)) - (160.0 * x)) + 80.0)
    val ret: Real = (x - (f / ff))
    ret
  }

}
