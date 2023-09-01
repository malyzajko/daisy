import daisy.lang._
import Real._
import daisy.lang.Vector._

object lorentz {

	def lorentz(m:Matrix): Vector = {
require(m >= 1.0 && m <= 2.0 && m.size(41,3)
	 && m.specM(Set((Set((25, 0), (2, 1)),(1.25, 1.92)), (Set((6, 1)),(1.06, 1.35)),
		(Set((18, 0), (5, 1), (20, 0), (0, 2), (32, 0)),(1.06, 1.67)), (Set((34, 2), (2, 2)),(1.2, 1.26)),
		(Set((23, 1), (4, 0), (12, 1), (27, 1)),(1.06, 1.07)), (Set((14, 1), (28, 0), (17, 0)),(1.06, 1.3)),
		(Set((36, 1), (9, 2), (19, 0)),(1.2, 1.53)), (Set((1, 0)),(1.6, 1.68)),
		(Set((38, 1), (18, 1), (7, 1)),(1.41, 1.72)), (Set((22, 1), (12, 0), (34, 0), (31, 1), (23, 0)),(1.21, 1.87)),
		(Set((20, 1)),(1.17, 1.91)), (Set((3, 0), (37, 2)),(1.33, 1.97))))
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