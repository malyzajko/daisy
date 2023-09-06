import daisy.lang._
import Real._
import daisy.lang.Vector._

object convolve2d_size3 {

	def convolve2d_size3(image: Matrix, kernel: Matrix): Matrix = {
require(image >= -153.55 && image <= 291.35 && image.size(3,3)
	 && image.specM(Set((Set((0, 0)),(-92.56, -19.55)), (Set((0, 1)),(-73.62, -60.65)),
		(Set((0, 2)),(-133.4, 40.21)), (Set((1, 0)),(14.43, 59.49)),
		(Set((1, 1)),(-143.41, 150.52)), (Set((1, 2)),(24.02, 156.55)),
		(Set((2, 0)),(-11.56, 87.21)), (Set((2, 1)),(11.39, 48.93)),
		(Set((2, 2)),(-59.7, -3.91))))
	 && kernel >= -104.89 && kernel <= 57.21 && kernel.size(3, 3)
	 && kernel.specM(Set((Set((0, 0)),(-104.15, 0.13)), (Set((0, 1)),(-33.88, -5.43)),
		(Set((0, 2)),(10.47, 13.69)), (Set((1, 0)),(-62.97, 40.7)),
		(Set((1, 1)),(-16.21, 37.66)), (Set((1, 2)),(-16.37, -2.02)),
		(Set((2, 0)),(-76.11, 13.8)), (Set((2, 1)),(-100.78, -40.07)),
		(Set((2, 2)),(-84.29, -6.81))))
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