import scala.annotation.strictfp
import daisy.lang._
import Real._

@strictfp
object matrixXS {


  def determinant(a: Real, b: Real, c: Real, d: Real, e: Real, f: Real, g: Real, h: Real, i: Real): Real = {
    require((0.9 <= a) && (a <= 52.1) && (-40.1 <= b) && (b <= 10.1) && (0.9 <= c) && (c <= 125.1) &&
      (0.9 <= d) && (d <= 98.1) && (0.9 <= e) && (e <= 121.1) && (0.9 <= f) && (f <= 54.1) &&
      (-96.1 <= g) && (g <= 10.0) && (-10.1 <= h) && (h <= 20.1) && (-10.1 <= i) && (i <= 81.1))

    a * ((e * i) - (f * h)) - b * ((d * i) - (f * g)) + c * ((d * h) - (e * g))
  }

  // solving a system of equations using the Cramer's rule variable x
  def solveEquationX(a1: Real, b1: Real, c1: Real, d1: Real, a2: Real, b2: Real, c2: Real, d2: Real, a3: Real, b3: Real,
                     c3: Real, d3: Real): Real = {
    require(((22 <= a1) && (a1 <= 52) && (-40 <= b1) && (b1 <= -10) && (95 <= c1) && (c1 <= 125) && (1.0 <= d1) && (d1 <= 10.0) &&
      (68 <= a2) && (a2 <= 98) && (91 <= b2) && (b2 <= 121) && (24 <= c2) && (c2 <= 54) && (1.0 <= d2) && (d2 <= 10.0) &&
      (-96 <= a3) && (a3 <= -66) && (-10 <= b3) && (b3 <= 20) && (51 <= c3) && (c3 <= 81) && (1.0 <= d3) && (d3 <= 10.0)))


    val d: Real = determinant(a1, b1, c1, a2, b2, c2, a3, b3, c3)
    val d_x: Real = determinant(d1, b1, c1, d2, b2, c2, d3, b3, c3)

    val x: Real = d_x / d
    x
  }

  // solving a system of equations using the Cramer's rule -  variable y
  def solveEquationY(a1: Real, b1: Real, c1: Real, d1: Real, a2: Real, b2: Real, c2: Real, d2: Real, a3: Real, b3: Real,
                     c3: Real, d3: Real): Real = {
    require(((22 <= a1) && (a1 <= 52) && (-40 <= b1) && (b1 <= -10) && (95 <= c1) && (c1 <= 125) && (1.0 <= d1) && (d1 <= 10.0) &&
      (68 <= a2) && (a2 <= 98) && (91 <= b2) && (b2 <= 121) && (24 <= c2) && (c2 <= 54) && (1.0 <= d2) && (d2 <= 10.0) &&
      (-96 <= a3) && (a3 <= -66) && (-10 <= b3) && (b3 <= 20) && (51 <= c3) && (c3 <= 81) && (1.0 <= d3) && (d3 <= 10.0)))


    val d: Real = determinant(a1, b1, c1, a2, b2, c2, a3, b3, c3)
    val d_y: Real = determinant(a1, d1, c1, a2, d2, c2, a3, d3, c3)

    val y: Real = d_y / d
    y
  }

  // solving a system of equations using the Cramer's rule - variable z
  def solveEquationZ(a1: Real, b1: Real, c1: Real, d1: Real, a2: Real, b2: Real, c2: Real, d2: Real, a3: Real, b3: Real,
                     c3: Real, d3: Real): Real = {
    require(((22 <= a1) && (a1 <= 52) && (-40 <= b1) && (b1 <= -10) && (95 <= c1) && (c1 <= 125) && (1.0 <= d1) && (d1 <= 10.0) &&
      (68 <= a2) && (a2 <= 98) && (91 <= b2) && (b2 <= 121) && (24 <= c2) && (c2 <= 54) && (1.0 <= d2) && (d2 <= 10.0) &&
      (-96 <= a3) && (a3 <= -66) && (-10 <= b3) && (b3 <= 20) && (51 <= c3) && (c3 <= 81) && (1.0 <= d3) && (d3 <= 10.0)))


    val d: Real = determinant(a1, b1, c1, a2, b2, c2, a3, b3, c3)
    val d_z: Real = determinant(a1, b1, d1, a2, b2, d2, a3, d3, b3)

    val z: Real = d_z / d
    z
  }

}
