import daisy.lang._
import Real._
import daisy.lang.Vector._

object roux1 {

	def roux1(x: Vector): Real = {
require(x >= -58.25 && x <= 61.32 && x.size(100)
	 && x.specV(Set(((16, 25),(-46.01, 60.24)), ((37, 47),(-1.48, 39.69)), ((51, 55),(42.26, 51.08)),
((56, 59),(-53.42, -23.13)), ((60, 60),(-44.0, -35.58)), ((61, 62),(11.48, 58.09)),
((65, 75),(-13.74, 52.97)), ((80, 80),(-4.55, 45.31)), ((81, 83),(-27.17, 34.75)),
((87, 97),(-45.6, 54.83))))
	)

        x.fold(0.0)((y: Real, i: Real) => {1.5 * i - 0.7 * y})
    }


}