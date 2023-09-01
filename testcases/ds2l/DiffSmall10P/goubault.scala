import daisy.lang._
import Real._
import daisy.lang.Vector._

object goubault {

	def goubault(x:Vector, y: Real): Real = {
require(54.86 <= y && y <= 359.03
	 && x >= -270.01 && x <= 385.38 && x.size(100)
	 && x.specV(Set(((0, 0),(-9.31, 158.0)), ((1, 2),(-92.29, 377.67)), ((3, 3),(3.84, 167.67)),
((4, 14),(-236.35, 218.48)), ((15, 21),(1.37, 309.15)), ((22, 22),(-258.07, 232.45)),
((23, 28),(152.37, 255.76)), ((33, 43),(-13.93, -4.59)), ((59, 61),(14.54, 347.36)),
((63, 73),(-195.6, -167.4))))
	)

        x.fold(y)((acc: Real, xi: Real) => {0.75 * xi - 0.125 * acc})
    }


}