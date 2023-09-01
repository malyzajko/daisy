import daisy.lang._
import Real._
import daisy.lang.Vector._

object nonlin2 {

	def nonlin2(x: Vector, y: Vector): Vector = {
require(x >= 0.0 && x <= 1.0 && x.size(100)
	 && x.specV(Set(((0, 0),(0.01, 0.55)), ((1, 11),(0.14, 0.94)), ((19, 29),(0.2, 0.61)),
((30, 30),(0.51, 0.64)), ((38, 42),(0.17, 0.81)), ((43, 43),(0.3, 0.31)),
((46, 46),(0.05, 0.89)), ((47, 49),(0.72, 0.78)), ((52, 62),(0.01, 0.84)),
((63, 73),(0.47, 0.75))))
	 && y >= 0.0 && y <= 1.0 && y.size(100)
	 && y.specV(Set(((0, 2),(0.24, 0.33)), ((3, 3),(0.63, 0.91)), ((4, 4),(0.67, 0.69)),
((5, 5),(0.47, 0.98)), ((7, 8),(0.7, 0.84)), ((9, 10),(0.44, 0.86)),
((12, 19),(0.57, 0.72)), ((20, 21),(0.21, 0.31)), ((22, 28),(0.08, 0.62)),
((50, 60),(0.14, 0.91))))
	)

        //x := x + 0.01 * (-x + 2*x*x + y*y)
        val x1: Real = y.fold(x.head)((acc: Real, yi: Real) => {acc + 0.01 * (-acc + 2*acc*acc + yi*yi)})
        //y := y + 0.01 * (-y + y*y)
        val y1: Real = x.fold(y.head)((acc: Real, xi: Real) => {acc + 0.01 * (-acc + acc*acc)})
        Vector(List(x1, y1))
    }


}