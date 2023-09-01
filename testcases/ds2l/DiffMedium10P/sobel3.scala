import daisy.lang._
import Real._
import daisy.lang.Vector._

object sobel3 {

	def sobel3(im: Matrix): Matrix = {
require(im >= 251.34 && im <= 341.89 && im.size(9,9)
	 && im.specM(Set((Set((0, 7), (1, 2), (3, 4), (4, 6), (8, 0), (6, 7), (0, 2), (2, 2), (2, 8), (5, 2), (6, 2), (3, 8), (8, 7), (6, 8)),(264.69, 334.5))))
	)
// 9x9 input image

        val kh: Matrix = Matrix(List(List(-1, 0, 1), List(-2, 0, 2), List(-1, 0, 1)))
        val kv: Matrix = Matrix(List(List(1, 2, 1), List(0, 0, 0), List(-1, -2, -1)))

        val padded: Matrix = im.pad(1,1)

        // inlined convolute 2d for kh
        val flippedKh: Matrix = (kh.flipud()).fliplr()
        val gx: Matrix = padded.slideReduce(3, 1)(m => {
            val tmp: Matrix = flippedKh * m // element-wise multiplication of the kernel and the image
            tmp.foldElements(0.0)((acc, x) => acc + x)
        })
        // inlined convolute 2d for kv
        val flippedKv: Matrix = (kv.flipud()).fliplr()
        val gy: Matrix = padded.slideReduce(3, 1)(m => {
            val tmp: Matrix = flippedKv * m // element-wise multiplication of the kernel and the image
            tmp.foldElements(0.0)((acc, x) => acc + x)
        })

        val pre: Matrix = gx * gx + gy * gy
        val g: Matrix = pre.sqrt()
        g * 255.0 / g.max()
    }


}