import daisy.lang._
import Real._
import daisy.lang.Vector._

object stdDeviation {

	def stdDeviation(x: Vector): Real = {
require(x >= -160.06 && x <= 360.98 && x.size(100)
	)

        val n: Real = x.length()
        val y = x.fold(0.0)((acc: Real, i: Real) => acc + i)
        val avg = y / n

        val z = x.fold(0.0)((acc: Real, i: Real) => {
            acc + pow((i - avg), 2)
        })
        sqrt(z / n)
    }


}