import daisy.lang._
import Real._
import daisy.lang.Vector._

object alphaBlending {

	def alphaBlending(b: Matrix, c: Matrix, alpha: Real): Matrix = {
require(0.0 <= alpha && alpha <= 1.0
	 && b >= 223.35 && b <= 530.05 && b.size(500,500)
	 && c >= -253.26 && c <= -108.41 && c.size(500,500)
	)

          b * alpha + c * (1 - alpha)
  }
          


}