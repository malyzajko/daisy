import daisy.lang._
import Real._
import daisy.lang.Vector._

object sobel3 {

	def sobel3(im: Matrix): Matrix = {
require(im >= 251.34 && im <= 341.89 && im.size(81,81)
	 && im.specM(Set((Set((35, 53)),(285.31, 310.61)), (Set((75, 71), (47, 1)),(289.31, 303.49)),
		(Set((46, 65), (20, 43), (57, 7)),(301.5, 301.85)), (Set((52, 23), (49, 9)),(286.58, 287.93)),
		(Set((80, 42), (29, 58), (48, 42)),(332.33, 340.24)), (Set((33, 51)),(274.8, 332.29)),
		(Set((51, 33)),(266.91, 307.67)), (Set((68, 34)),(273.17, 333.44))))
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