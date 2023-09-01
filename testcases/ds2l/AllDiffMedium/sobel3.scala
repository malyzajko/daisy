import daisy.lang._
import Real._
import daisy.lang.Vector._

object sobel3 {

	def sobel3(im: Matrix): Matrix = {
require(im >= 251.34 && im <= 341.89 && im.size(9,9)
	 && im.specM(Set((Set((0, 0)),(313.34, 322.63)), (Set((0, 1)),(254.13, 293.72)),
		(Set((0, 2)),(300.61, 305.45)), (Set((0, 3)),(282.56, 323.86)),
		(Set((0, 4)),(308.31, 311.21)), (Set((0, 5)),(251.75, 323.29)),
		(Set((0, 6)),(271.67, 336.51)), (Set((0, 7)),(285.97, 335.19)),
		(Set((0, 8)),(326.02, 328.73)), (Set((1, 0)),(265.03, 270.41)),
		(Set((1, 1)),(312.49, 320.85)), (Set((1, 2)),(298.42, 305.34)),
		(Set((1, 3)),(254.02, 331.47)), (Set((1, 4)),(313.97, 315.56)),
		(Set((1, 5)),(278.58, 304.88)), (Set((1, 6)),(253.41, 263.84)),
		(Set((1, 7)),(302.91, 335.7)), (Set((1, 8)),(269.07, 330.51)),
		(Set((2, 0)),(334.58, 336.29)), (Set((2, 1)),(262.8, 326.5)),
		(Set((2, 2)),(331.11, 331.54)), (Set((2, 3)),(259.52, 276.2)),
		(Set((2, 4)),(252.27, 258.33)), (Set((2, 5)),(258.89, 310.21)),
		(Set((2, 6)),(275.56, 336.27)), (Set((2, 7)),(260.88, 278.32)),
		(Set((2, 8)),(265.59, 303.12)), (Set((3, 0)),(296.17, 339.53)),
		(Set((3, 1)),(261.29, 309.72)), (Set((3, 2)),(257.66, 282.51)),
		(Set((3, 3)),(292.05, 304.05)), (Set((3, 4)),(252.47, 338.02)),
		(Set((3, 5)),(263.66, 336.22)), (Set((3, 6)),(279.28, 334.83)),
		(Set((3, 7)),(259.87, 308.22)), (Set((3, 8)),(260.92, 274.59)),
		(Set((4, 0)),(324.23, 324.36)), (Set((4, 1)),(322.72, 327.39)),
		(Set((4, 2)),(261.83, 278.19)), (Set((4, 3)),(333.74, 337.24)),
		(Set((4, 4)),(332.06, 337.37)), (Set((4, 5)),(277.88, 296.05)),
		(Set((4, 6)),(277.05, 326.65)), (Set((4, 7)),(253.58, 329.4)),
		(Set((4, 8)),(263.72, 288.56)), (Set((5, 0)),(261.75, 261.94)),
		(Set((5, 1)),(267.9, 337.28)), (Set((5, 2)),(256.0, 263.53)),
		(Set((5, 3)),(273.48, 340.66)), (Set((5, 4)),(295.58, 340.48)),
		(Set((5, 5)),(290.76, 334.09)), (Set((5, 6)),(278.28, 287.83)),
		(Set((5, 7)),(257.63, 307.69)), (Set((5, 8)),(299.35, 317.57)),
		(Set((6, 0)),(291.44, 337.99)), (Set((6, 1)),(281.46, 337.43)),
		(Set((6, 2)),(328.77, 340.73)), (Set((6, 3)),(279.61, 314.08)),
		(Set((6, 4)),(252.27, 253.66)), (Set((6, 5)),(285.92, 303.94)),
		(Set((6, 6)),(312.04, 330.54)), (Set((6, 7)),(279.32, 295.08)),
		(Set((6, 8)),(253.45, 261.8)), (Set((7, 0)),(252.23, 293.6)),
		(Set((7, 1)),(261.04, 288.06)), (Set((7, 2)),(259.66, 336.79)),
		(Set((7, 3)),(296.09, 315.16)), (Set((7, 4)),(251.75, 306.69)),
		(Set((7, 5)),(258.07, 268.96)), (Set((7, 6)),(252.76, 282.15)),
		(Set((7, 7)),(277.73, 334.05)), (Set((7, 8)),(289.09, 300.72)),
		(Set((8, 0)),(265.32, 271.86)), (Set((8, 1)),(297.08, 327.34)),
		(Set((8, 2)),(267.96, 297.87)), (Set((8, 3)),(299.05, 299.75)),
		(Set((8, 4)),(297.31, 320.07)), (Set((8, 5)),(264.49, 265.97)),
		(Set((8, 6)),(274.27, 283.07)), (Set((8, 7)),(254.77, 332.42)),
		(Set((8, 8)),(253.23, 263.19))))
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