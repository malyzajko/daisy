import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftvector {

	def fftvector(vr: Vector, vi: Vector): Vector = {
require(vr >= 68.9 && vr <= 160.43 && vr.size(128)
	 && vr.specV(Set(((0, 2),(103.94, 123.56)), ((3, 3),(90.69, 129.97)), ((4, 4),(131.35, 148.84)),
((5, 15),(129.12, 156.57)), ((28, 34),(92.12, 121.52)), ((55, 65),(128.64, 144.61)),
((68, 70),(69.01, 115.66)), ((74, 79),(87.43, 143.66)), ((99, 109),(104.94, 158.83)),
((111, 111),(110.47, 118.64)), ((113, 117),(123.58, 146.69)), ((118, 122),(110.61, 151.14))))
	 && vi >= -133.21 && vi <= 723.11 && vi.size(128)
	 && vi.specV(Set(((10, 17),(-110.26, 141.88)), ((51, 61),(124.72, 612.16)), ((62, 62),(600.81, 712.06)),
((63, 63),(29.02, 636.77)), ((64, 64),(178.99, 547.27)), ((65, 65),(134.64, 454.91)),
((66, 66),(57.43, 209.41)), ((67, 67),(86.47, 467.1)), ((68, 78),(481.01, 632.65)),
((89, 98),(-59.98, 498.89)), ((99, 99),(440.87, 647.4)), ((102, 112),(228.16, 477.6))))
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