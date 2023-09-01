import daisy.lang._
import Real._
import daisy.lang.Vector._

object pendulum {

	def pendulum(t: Vector, w: Vector): Vector = {
require(t >= -2.0 && t <= 2.0 && t.size(10000)
	 && w >= -5.0 && w <= 5.0 && w.size(10000)
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