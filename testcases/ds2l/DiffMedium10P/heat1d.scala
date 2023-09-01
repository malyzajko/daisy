import daisy.lang._
import Real._
import daisy.lang.Vector._

object heat1d {

	def heat1d(ax: Vector): Real = {
require(ax >= 1.0 && ax <= 2.0 && ax.size(65)
	 && ax.specV(Set(((0, 0),(1.49, 1.85)), ((1, 7),(1.37, 1.74)), ((15, 15),(1.17, 1.43)),
((18, 20),(1.2, 1.76)), ((24, 30),(1.01, 1.51)), ((38, 44),(1.28, 1.37))))
	)

          if (ax.length() <= 1) {
            ax.head
        } else {
            val coef = Vector(List(0.25, 0.5, 0.25))
            val updCoefs: Vector = ax.slideReduce(3,1)(v =>  (coef*v).sum())
            heat1d(updCoefs)
        }
    }


}