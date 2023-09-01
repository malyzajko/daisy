import daisy.lang._
import Real._
import daisy.lang.Vector._

object goubault {

	def goubault(x:Vector, y: Real): Real = {
require(54.86 <= y && y <= 359.03
	 && x >= -270.01 && x <= 385.38 && x.size(1000)
	)

        x.fold(y)((acc: Real, xi: Real) => {0.75 * xi - 0.125 * acc})
    }


}