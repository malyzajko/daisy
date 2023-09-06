import daisy.lang._
import Real._
import daisy.lang.Vector._

object roux1 {

	def roux1(x: Vector): Real = {
require(x >= -58.25 && x <= 61.32 && x.size(100)
	)

        x.fold(0.0)((y: Real, i: Real) => {1.5 * i - 0.7 * y})
    }


}