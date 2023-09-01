import daisy.lang._
import Real._
import daisy.lang.Vector._

object lorentz {

	def lorentz(m:Matrix): Vector = {
require(m >= 1.0 && m <= 2.0 && m.size(31,3)
	 && m.specM(Set((Set((22, 2), (23, 1), (4, 0), (12, 1)),(1.57, 1.83)), (Set((14, 1), (8, 0), (30, 0), (0, 2), (11, 2), (24, 2), (25, 1), (6, 2), (18, 1), (29, 1), (20, 1)),(1.23, 1.39)),
		(Set((12, 0), (14, 0), (17, 2), (28, 2)),(1.24, 1.98)), (Set((28, 1), (21, 1)),(1.27, 1.64)),
		(Set((16, 0)),(1.29, 1.92)), (Set((0, 0), (3, 1)),(1.46, 1.83)),
		(Set((23, 0), (7, 0)),(1.2, 1.33)), (Set((3, 0)),(1.38, 1.5)),
		(Set((21, 2), (7, 2)),(1.11, 1.18))))
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