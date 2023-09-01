import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftvector {

	def fftvector(vr: Vector, vi: Vector): Vector = {
require(vr >= 68.9 && vr <= 160.43 && vr.size(4)
	 && vr.specV(Set(((0, 0),(137.89, 142.94)), ((1, 1),(77.44, 95.69)), ((2, 2),(133.26, 159.54)),
((3, 3),(89.63, 114.3))))
	 && vi >= -133.21 && vi <= 723.11 && vi.size(4)
	 && vi.specV(Set(((0, 0),(6.75, 553.64)), ((1, 1),(-124.91, 315.18)), ((2, 2),(158.25, 534.54)),
((3, 3),(372.26, 515.46))))
	)

        /* v: (real part of signal / Fourier coeff., imaginary part of signal / Fourier coeff. ) */
        if (vr.length() == 1)
            Vector(List(vr.head, vi.head))
        else {
            val scalar: Real = 1
            val Pi: Real = 3.1415926
            val n: Int = vr.length()
            val direction: Vector = Vector(List(0.0, -2.0))
            val evens: Vector = fftvector(vr.everyNth(2, 0), vi.everyNth(2, 0))
            val odds: Vector = fftvector(vr.everyNth(2, 1), vi.everyNth(2, 1))

            val resleft: Vector = evens.enumSlideFlatMap(2)((k, xv) => {
                val base: Vector = xv / scalar

                val oddV: Vector = odds.slice(2 * k, 2 * k + 1)
                val expV: Vector = (direction.*(Pi * k / n)).exp()
                val offset: Vector = (oddV x expV) / scalar
                base + offset
            })

            val resright: Vector = evens.enumSlideFlatMap(2)((k, xv) => {
                val base: Vector = xv / scalar

                val oddV: Vector = odds.slice(2 * k, 2 * k + 1)
                val expV: Vector = (direction.*(Pi * k / n)).exp()
                val offset: Vector = (oddV x expV) / scalar
                base - offset
            })

            resleft ++ resright
        }
    }


}