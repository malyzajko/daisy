import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftvector {

	def fftvector(vr: Vector, vi: Vector): Vector = {
require(vr >= 68.9 && vr <= 160.43 && vr.size(4)
	 && vr.specV(Set(((1, 2),(104.81, 107.03)), ((0, 0),(90.19, 153.1))))
	 && vi >= -133.21 && vi <= 723.11 && vi.size(4)
	 && vi.specV(Set(((0, 1),(48.92, 233.58)), ((2, 3),(241.37, 319.14))))
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