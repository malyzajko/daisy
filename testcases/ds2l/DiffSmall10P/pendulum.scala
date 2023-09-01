import daisy.lang._
import Real._
import daisy.lang.Vector._

object pendulum {

	def pendulum(t: Vector, w: Vector): Vector = {
require(t >= -2.0 && t <= 2.0 && t.size(100)
	 && t.specV(Set(((1, 1),(-0.3, 1.63)), ((2, 8),(0.2, 1.09)), ((9, 9),(-1.92, 1.55)),
((10, 12),(-1.23, 1.13)), ((13, 23),(-1.02, 0.34)), ((25, 26),(-0.89, 0.63)),
((27, 37),(-0.81, 0.39)), ((45, 55),(-1.0, 0.01)), ((56, 59),(-1.11, 1.55)),
((76, 86),(-0.67, 0.95))))
	 && w >= -5.0 && w <= 5.0 && w.size(100)
	 && w.specV(Set(((35, 45),(-2.09, 1.26)), ((48, 51),(-3.05, 3.78)), ((53, 53),(0.31, 1.5)),
((54, 64),(1.37, 1.63)), ((65, 67),(-4.9, -1.0)), ((69, 79),(-1.37, 0.21)),
((81, 82),(1.3, 4.27)), ((83, 85),(1.53, 1.58)), ((86, 94),(-1.5, 2.94)),
((95, 99),(-3.7, 3.85))))
	)

        val h: Real = 0.01
        val L: Real = 2.0
        val g: Real = 9.80665

        val iter = Vector.zip(t,w)
        val init = Vector(List(t.head, w.head))

        iter.fold(init)((acc, x) => {
            val kt = acc.at(1)
            val kw = -g/L * sin(acc.head)

            val v = Vector(List(kt,kw))
            acc + v*h
        })
    }


}