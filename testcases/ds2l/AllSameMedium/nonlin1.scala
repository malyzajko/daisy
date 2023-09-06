import daisy.lang._
import Real._
import daisy.lang.Vector._

object nonlin1 {

	def nonlin1(x: Vector, y: Vector): Vector = {
require(x >= 0.0 && x <= 1.0 && x.size(1000)
	 && y >= 0.0 && y <= 1.0 && y.size(1000)
	)

        //x := x + 0.01 * (-2*x - 3*y + x*x)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-2*acc - 3*yi + acc*acc)})
        //y := y + 0.01 * (x + y)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (xi + acc)})
        Vector(List(x1, y1))
    }


}