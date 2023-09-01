import daisy.lang._
import Real._
import daisy.lang.Vector._

object lorentz {

	def lorentz(m:Matrix): Vector = {
require(m >= 1.0 && m <= 2.0 && m.size(21,3)
	 && m.specM(Set((Set((11, 1), (4, 0), (13, 1), (3, 1), (14, 1)),(1.04, 1.78)), (Set((15, 1), (5, 1), (10, 0), (3, 2), (5, 2)),(1.13, 1.27))))
	)

        val init: Vector = m.row(0)
        m.fold(init)((acc, v) => {
            val x:Real = acc.at(0)
            val y:Real = acc.at(1)
            val z:Real = acc.at(2)
            val tmpx:Real = x + 10.0*(y - x)*0.005
            val tmpy:Real = y + (28.0*x - y - x*z)*0.005
            val tmpz:Real = z + (x*y - 2.666667*z)*0.005
            Vector(List(tmpx,tmpy,tmpz))
        })
    }


}