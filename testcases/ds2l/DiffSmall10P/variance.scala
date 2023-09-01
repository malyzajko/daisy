import daisy.lang._
import Real._
import daisy.lang.Vector._

object variance {

	def variance(x: Vector): Real = {
require(x >= -252.68 && x <= 72.42 && x.size(100)
	 && x.specV(Set(((0, 2),(-3.44, 11.24)), ((3, 3),(-241.35, -239.46)), ((4, 4),(-232.34, 46.44)),
((5, 14),(-169.12, -9.74)), ((15, 16),(-142.45, 39.44)), ((17, 17),(-164.15, 58.66)),
((18, 18),(-167.45, -52.1)), ((19, 20),(-149.79, -19.7)), ((21, 31),(-251.45, -163.07)),
((63, 73),(-7.02, 45.26))))
	)

        val n: Real = x.length()
        val y = x.fold(0.0)((acc: Real, i: Real) => acc + i)
        val avg = y / n

        val z = x.fold(0.0)((acc: Real, i: Real) => acc + pow(i - avg, 2))
        z / n
    }


}