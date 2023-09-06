import daisy.lang._
import Real._
import daisy.lang.Vector._

object nonlin3 {

	def nonlin3(x: Vector, y: Vector): Vector = {
require(x >= 0.0 && x <= 1.0 && x.size(10000)
	 && y >= 0.0 && y <= 1.0 && y.size(10000)
	)

        //x := x + 0.01 * (-x + y*y)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-acc + yi*yi)})
        //y := y + 0.01 * (-2.0*y + 3.0*x*x)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (-2.0*acc + 3.0*xi*xi)})
        Vector(List(x1, y1))
    }


}