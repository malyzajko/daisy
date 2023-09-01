import daisy.lang._
import Real._
import daisy.lang.Vector._

object harmonic {

	def harmonic(x: Vector, y: Vector): Vector = {
require(x >= -5.32 && x <= 725.6 && x.size(100)
	 && y >= -432.12 && y <= 78.94 && y.size(100)
	)

        //x1 := x1 + 0.01 * x2
        val x1: Real = y.fold(x.head)((acc: Real, xi: Real) => {acc + 0.01* xi})
        //x2 := -0.01 * x1 + 0.99 * x2
        val x2: Real = x.fold(y.head)((acc: Real, xi: Real) => {-0.01 * xi + 0.99 * acc})
        Vector(List(x1, x2))
    }


}