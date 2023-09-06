import daisy.lang._
import Real._
import daisy.lang.Vector._

object stdDeviation {

	def stdDeviation(x: Vector): Real = {
require(x >= -160.06 && x <= 360.98 && x.size(100)
	 && x.specV(Set(((0, 0),(-126.33, -31.2)), ((1, 2),(-142.42, 127.86)), ((3, 13),(9.67, 350.69)),
((30, 40),(300.27, 354.64)), ((41, 51),(-116.25, -22.04)), ((54, 58),(-109.82, 99.67)),
((61, 71),(72.83, 209.76)), ((74, 75),(-38.03, 57.13)), ((76, 78),(163.15, 191.18)),
((79, 82),(229.67, 281.25))))
	)

        val n: Real = x.length()
        val y = x.fold(0.0)((acc: Real, i: Real) => acc + i)
        val avg = y / n

        val z = x.fold(0.0)((acc: Real, i: Real) => {
            acc + pow((i - avg), 2)
        })
        sqrt(z / n)
    }


}