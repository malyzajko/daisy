import daisy.lang._
import Real._
import daisy.lang.Vector._

object heat1d {

	def heat1d(ax: Vector): Real = {
require(ax >= 1.0 && ax <= 2.0 && ax.size(513)
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