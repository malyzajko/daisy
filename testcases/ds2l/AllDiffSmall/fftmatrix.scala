import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftmatrix {

	def fftmatrix(m: Matrix): Matrix = {
require(m >= -326.68 && m <= 677.57 && m.size(4,2)
	 && m.specM(Set((Set((0, 0)),(-41.42, 7.32)), (Set((0, 1)),(-307.64, -199.11)),
		(Set((1, 0)),(-145.12, -10.68)), (Set((1, 1)),(573.71, 625.55)),
		(Set((2, 0)),(283.56, 430.62)), (Set((2, 1)),(-321.9, -76.17)),
		(Set((3, 0)),(4.99, 96.14)), (Set((3, 1)),(-199.84, -13.72))))
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