import daisy.lang._
import Real._
import daisy.lang.Vector._

object heat1d {

	def heat1d(ax: Vector): Real = {
require(ax >= 1.0 && ax <= 2.0 && ax.size(65)
	 && ax.specV(Set(((0, 0),(1.19, 1.2)), ((1, 1),(1.46, 1.93)), ((2, 2),(1.26, 1.49)),
((3, 3),(1.11, 1.13)), ((4, 4),(1.01, 1.7)), ((5, 5),(1.55, 1.76)),
((6, 6),(1.68, 1.97)), ((7, 7),(1.26, 1.39)), ((8, 8),(1.01, 1.74)),
((9, 9),(1.38, 1.56)), ((10, 10),(1.15, 1.93)), ((11, 11),(1.44, 1.93)),
((12, 12),(1.01, 1.71)), ((13, 13),(1.27, 1.99)), ((14, 14),(1.32, 1.68)),
((15, 15),(1.19, 1.59)), ((16, 16),(1.24, 1.46)), ((17, 17),(1.15, 1.27)),
((18, 18),(1.86, 1.95)), ((19, 19),(1.78, 1.85)), ((20, 20),(1.36, 1.97)),
((21, 21),(1.22, 1.49)), ((22, 22),(1.39, 1.88)), ((23, 23),(1.3, 1.38)),
((24, 24),(1.1, 1.56)), ((25, 25),(1.2, 1.86)), ((26, 26),(1.28, 1.96)),
((27, 27),(1.6, 1.8)), ((28, 28),(1.23, 1.34)), ((29, 29),(1.07, 1.85)),
((30, 30),(1.81, 1.91)), ((31, 31),(1.65, 1.86)), ((32, 32),(1.03, 1.19)),
((33, 33),(1.56, 1.7)), ((34, 34),(1.91, 1.93)), ((35, 35),(1.06, 1.96)),
((36, 36),(1.29, 1.93)), ((37, 37),(1.75, 1.88)), ((38, 38),(1.38, 1.53)),
((39, 39),(1.05, 1.54)), ((40, 40),(1.56, 1.79)), ((41, 41),(1.2, 1.95)),
((42, 42),(1.58, 1.78)), ((43, 43),(1.31, 1.56)), ((44, 44),(1.12, 1.64)),
((45, 45),(1.27, 1.43)), ((46, 46),(1.75, 1.83)), ((47, 47),(1.25, 1.91)),
((48, 48),(1.05, 1.75)), ((49, 49),(1.58, 1.79)), ((50, 50),(1.33, 1.81)),
((51, 51),(1.48, 1.64)), ((52, 52),(1.15, 1.53)), ((53, 53),(1.27, 1.86)),
((54, 54),(1.4, 1.83)), ((55, 55),(1.57, 1.82)), ((56, 56),(1.48, 1.91)),
((57, 57),(1.37, 1.76)), ((58, 58),(1.16, 1.2)), ((59, 59),(1.32, 1.41)),
((60, 60),(1.36, 1.93)), ((61, 61),(1.64, 1.71)), ((62, 62),(1.34, 1.99)),
((63, 63),(1.13, 1.48)), ((64, 64),(1.0, 1.75))))
	)

          if (ax.length() <= 1) {
            ax.head
        } else {
            val coef = Vector(List(0.25, 0.5, 0.25))
            val updCoefs: Vector = ax.slideReduce(3,1)(v =>  (coef*v).sum())
            heat1d(updCoefs)
        }
    }


}