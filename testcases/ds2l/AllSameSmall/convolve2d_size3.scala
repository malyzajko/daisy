import daisy.lang._
import Real._
import daisy.lang.Vector._

object convolve2d_size3 {

	def convolve2d_size3(image: Matrix, kernel: Matrix): Matrix = {
require(image >= -153.55 && image <= 291.35 && image.size(3,3)
	 && kernel >= -104.89 && kernel <= 57.21 && kernel.size(3, 3)
	)

        val flippedK: Matrix = (kernel.flipud()).fliplr()
        val padded: Matrix = image.pad(1,1)
        val output: Matrix = padded.slideReduce(3, 1)(m => {
            val tmp: Matrix = flippedK.*(m) // element-wise multiplication of the kernel and the image
            tmp.foldElements(0.0)((acc, x) => acc + x)
        })
        output
    }


}