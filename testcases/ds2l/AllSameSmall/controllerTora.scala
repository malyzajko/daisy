import daisy.lang._
import Real._
import daisy.lang.Vector._

object controllerTora {

	def controllerTora(x: Vector, weights1: Matrix, weights2: Matrix, weights3: Matrix, weights4: Matrix, bias1: Vector, bias2: Vector, bias3: Vector, bias4: Real): Vector = {
require(10.197819 <= bias4 && bias4 <= 10.197819
	 && x >= -2.0 && x <= 2.0 && x.size(10)
	 && bias1 >= 0.040232 && bias1 <= 0.341392 && bias1.size(10)
	 && bias2 >= 0.082624 && bias2 <= 0.318763 && bias2.size(10)
	 && bias3 >= 0.096189 && bias3 <= 0.297542 && bias3.size(10)
	 && weights1 >= -0.374036 && weights1 <= 0.319683 && weights1.size(10,10)
	 && weights2 >= -0.426394 && weights2 <= 0.323056 && weights2.size(10,10)
	 && weights3 >= -0.582338 && weights3 <= 0.566423 && weights3.size(10,10)
	 && weights4 >= -0.293298 && weights4 <= 0.311236 && weights4.size(1,10)
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