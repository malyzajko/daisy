import daisy.lang._
import Real._
import daisy.lang.Vector._

object avg {

	def avg(x: Vector): Real = {
require(x >= -62.54 && x <= 15.02 && x.size(1000)
	)

        val n: Real = x.length()
        val z = x.fold(0.0)((acc: Real, i: Real) => acc + i)
        z / n
    }


}