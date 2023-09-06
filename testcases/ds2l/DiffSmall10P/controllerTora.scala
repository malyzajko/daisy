import daisy.lang._
import Real._
import daisy.lang.Vector._

object controllerTora {

	def controllerTora(x: Vector, weights1: Matrix, weights2: Matrix, weights3: Matrix, weights4: Matrix, bias1: Vector, bias2: Vector, bias3: Vector, bias4: Real): Vector = {
require(10.197819 <= bias4 && bias4 <= 10.197819
	 && x >= -2.0 && x <= 2.0 && x.size(10)
	 && x.specV(Set(((6, 7),(-1.83, 1.07))))
	 && bias1 >= 0.040232 && bias1 <= 0.341392 && bias1.size(10)
	 && bias1.specV(Set(((2, 3),(0.12, 0.29))))
	 && bias2 >= 0.082624 && bias2 <= 0.318763 && bias2.size(10)
	 && bias2.specV(Set(((4, 5),(0.09, 0.3))))
	 && bias3 >= 0.096189 && bias3 <= 0.297542 && bias3.size(10)
	 && bias3.specV(Set(((1, 2),(0.1, 0.19))))
	 && weights1 >= -0.374036 && weights1 <= 0.319683 && weights1.size(10,10)
	 && weights1.specM(Set((Set((4, 3), (5, 4), (6, 4), (9, 2), (7, 9), (7, 6), (0, 5), (3, 5), (4, 4), (8, 4), (6, 5), (9, 9), (0, 3)),(0.22, 0.24))))
	 && weights2 >= -0.426394 && weights2 <= 0.323056 && weights2.size(10,10)
	 && weights2.specM(Set((Set((0, 0), (0, 3), (6, 4), (4, 2)),(-0.32, -0.24))))
	 && weights3 >= -0.582338 && weights3 <= 0.566423 && weights3.size(10,10)
	 && weights3.specM(Set((Set((0, 1), (8, 4)),(0.03, 0.17))))
	 && weights4 >= -0.293298 && weights4 <= 0.311236 && weights4.size(1,10)
	 && weights4.specM(Set((Set((3, 7)),(-0.21, 0.31))))
	)

    val layer1 = (weights1.x(x) + bias1).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer2 = (weights2.x(layer1) + bias2).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer3 = (weights3.x(layer2) + bias3).map(el => {
      val relu = Vector(List(el, 0.0))
      relu.max()
    })
    val layer4 = (weights4.x(layer3) + bias4)

    layer4
  }


}