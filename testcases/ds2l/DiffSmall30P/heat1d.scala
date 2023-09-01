import daisy.lang._
import Real._
import daisy.lang.Vector._

object heat1d {

	def heat1d(ax: Vector): Real = {
require(ax >= 1.0 && ax <= 2.0 && ax.size(33)
	 && ax.specV(Set(((0, 0),(1.13, 1.42)), ((1, 4),(1.35, 1.91)), ((13, 16),(1.46, 1.58)),
((5, 7),(1.25, 1.97)), ((9, 10),(1.11, 1.15)), ((11, 12),(1.52, 1.68)),
((17, 19),(1.72, 1.94)), ((20, 21),(1.65, 1.76)), ((29, 31),(1.46, 1.55))))
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