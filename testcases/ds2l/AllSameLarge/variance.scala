import daisy.lang._
import Real._
import daisy.lang.Vector._

object variance {

	def variance(x: Vector): Real = {
require(x >= -252.68 && x <= 72.42 && x.size(10000)
	)

        val n: Real = x.length()
        val y = x.fold(0.0)((acc: Real, i: Real) => acc + i)
        val avg = y / n

        val z = x.fold(0.0)((acc: Real, i: Real) => acc + pow(i - avg, 2))
        z / n
    }


}