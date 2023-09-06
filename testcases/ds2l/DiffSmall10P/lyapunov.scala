import daisy.lang._
import Real._
import daisy.lang.Vector._

object lyapunov {

	def lyapunov(x: Vector, weights1: Matrix, weights2: Matrix, bias1: Vector, bias2: Real): Vector = {
require(0.5307131 <= bias2 && bias2 <= 0.5307131
	 && x >= -6.0 && x <= 6.0 && x.size(10)
	 && x.specV(Set(((7, 8),(2.0, 3.31))))
	 && bias1 >= -0.8746956 && bias1 <= 1.1860801 && bias1.size(10)
	 && bias1.specV(Set(((4, 5),(-0.82, 0.71))))
	 && weights1 >= -0.6363012 && weights1 <= 1.0211772 && weights1.size(10,10)
	 && weights1.specM(Set((Set((4, 0), (4, 3), (1, 5), (9, 5), (6, 7), (0, 5), (2, 5)),(-0.45, -0.2))))
	 && weights2 >= -0.80846876 && weights2 <= 1.1081733 && weights2.size(1,10)
	 && weights2.specM(Set((Set((6, 2), (9, 1), (5, 8)),(-0.41, -0.26))))
	)

    val layer1: Vector = (weights1.x(x) + bias1).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer2: Vector = (weights2.x(layer1) + bias2).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    layer2
  }


}