import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftmatrix {

	def fftmatrix(m: Matrix): Matrix = {
require(m >= -326.68 && m <= 677.57 && m.size(128,2)
	 && m.specM(Set((Set((80, 1)),(-99.59, 527.68)), (Set((56, 1), (58, 1), (124, 1), (125, 0), (70, 0), (4, 0), (91, 1)),(-26.63, 344.57)),
		(Set((37, 0), (102, 1), (5, 1), (19, 0), (8, 0), (74, 0)),(28.26, 479.71)), (Set((41, 0), (118, 0), (96, 0), (120, 1), (25, 1), (64, 1), (53, 1)),(37.55, 591.62)),
		(Set((54, 0), (87, 0), (40, 1), (109, 0), (120, 0), (3, 0), (47, 0)),(161.66, 544.27)), (Set((14, 0), (102, 0), (58, 0), (80, 0), (69, 0), (9, 1)),(-197.07, 79.45)),
		(Set((51, 0), (75, 1), (121, 1), (13, 1), (38, 0), (114, 1), (82, 0), (20, 0)),(-292.74, 535.06)), (Set((64, 0), (53, 0), (119, 0), (97, 0)),(161.3, 566.73)),
		(Set((4, 1), (59, 1), (32, 1), (65, 1), (30, 1), (66, 0), (26, 0), (103, 0)),(140.09, 647.87)), (Set((89, 1), (54, 1), (78, 1), (27, 1), (28, 0), (94, 0), (83, 0), (127, 0)),(-19.36, 61.56)),
		(Set((116, 0), (43, 0), (32, 0), (65, 0), (84, 1), (49, 1), (63, 0)),(-188.62, 596.17)), (Set((20, 1), (45, 0), (110, 1), (100, 0), (111, 0)),(202.6, 479.07))))
	)
/* m: (real part of signal / Fourier coeff., imaginary part of signal / Fourier coeff. ) */
        if (m.numRows() == 1)
            m
        else {
            val scalar: Real = 1
            val Pi: Real = 3.1415926
            val n: Int = m.numRows()   /* signal length, has to be power of 2 */
            val direction: Vector = Vector(List(0.0, -2.0))
            val evens: Matrix = fftmatrix(m.everyNth(2, 0))
            val odds: Matrix = fftmatrix(m.everyNth(2, 1))

            val resleft: Matrix = evens.enumRowsMap((k:Int, x:Vector) => {
                //val (k, x) = y
                val base: Vector = x / scalar
                val offset: Vector = (direction.*(Pi * k / n)).exp() x odds.row(k) / scalar // here vector multiplication should be specialy defined for complex numbers? is it the same as cross product of vectors?
                base + offset
            })
            val resright: Matrix = evens.enumRowsMap((k:Int, x:Vector)  => {
                //val (k, x) = y
                val base: Vector = x / scalar
                val offset: Vector = (direction.*(Pi * k / n)).exp() x odds.row(k) / scalar // here vector multiplication should be specialy defined for complex numbers?
                base - offset
            })

            resleft ++ resright
        }
    }


}