import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftmatrix {

	def fftmatrix(m: Matrix): Matrix = {
require(m >= -326.68 && m <= 677.57 && m.size(4,2)
	 && m.specM(Set((Set((2, 1), (3, 0)),(569.87, 600.03))))
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