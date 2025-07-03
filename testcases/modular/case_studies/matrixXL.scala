import scala.annotation.strictfp
import daisy.lang._
import Real._

@strictfp
object matrixXL {


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


  def solveEquationsVector(a11: Real, b11: Real, c11: Real, a12: Real, b12: Real, c12: Real, a13: Real, b13: Real, c13: Real,
                             a21: Real, b21: Real, c21: Real, a22: Real, b22: Real, c22: Real, a23: Real, b23: Real, c23: Real,
                             a31: Real, b31: Real, c31: Real, a32: Real, b32: Real, c32: Real, a33: Real, b33: Real, c33: Real,
                             d1: Real, d2: Real, d3: Real): Real = {

    require((23.0 <= a11) && (a11 <= 26.0) && (-39.0 <= b11) && (b11 <= -36.0) && (96.0 <= c11) && (c11 <= 99.0) &&
      (69.0 <= a12) && (a12 <= 72.0) && (92.0 <= b12) && (b12 <= 95.0) && (25.0 <= c12) && (c12 <= 28.0) &&
      (-95.0 <= a13) && (a13 <= -92.0) && (-9.0 <= b13) && (b13 <= -6.0) && (52.0 <= c13) && (c13 <= 55.0) &&
      (26.0 <= a21) && (a21 <= 30.0) && (-36.0 <= b21) && (b21 <= -32.0) && (99.0 <= c21) && (c21 <= 103.0) &&
      (72.0 <= a22) && (a22 <= 76.0) && (95.0 <= b22) && (b22 <= 99.0) && (28.0 <= c22) && (c22 <= 32.0) &&
      (-92.0 <= a23) && (a23 <= -88.0) && (-6.0 <= b23) && (b23 <= -2.0) && (55.0 <= c23) && (c23 <= 59.0) &&
      (30.0 <= a31) && (a31 <= 34.0) && (-32.0 <= b31) && (b31 <= -28.0) && (103.0 <= c31) && (c31 <= 107.0) &&
      (76.0 <= a32) && (a32 <= 80.0) && (99.0 <= b32) && (b32 <= 103.0) && (32.0 <= c32) && (c32 <= 36.0) &&
      (-88.0 <= a33) && (a33 <= -84.0) && (-2.0 <= b33) && (b33 <= 2.0) && (59.0 <= c33) && (c33 <= 63.0) &&
      (1.0 <= d1) && (d1 <= 10.0) && (1.0 <= d2) && (d2 <= 10.0) && (1.0 <= d3) && (d3 <= 10.0))


    val x1: Real = solveEquationX(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)
    val y1: Real = solveEquationY(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)
    val z1: Real = solveEquationZ(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)


    val x2: Real = solveEquationX(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)
    val y2: Real = solveEquationY(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)
    val z2: Real = solveEquationZ(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)


    val x3: Real = solveEquationX(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)
    val y3: Real = solveEquationY(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)
    val z3: Real = solveEquationZ(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)


    val avg = (x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3) / 9.0
    avg

  }

  def solveEquationsVectorXL(a11: Real, b11: Real, c11: Real, a12: Real, b12: Real, c12: Real, a13: Real, b13: Real, c13: Real,
                             a21: Real, b21: Real, c21: Real, a22: Real, b22: Real, c22: Real, a23: Real, b23: Real, c23: Real,
                             a31: Real, b31: Real, c31: Real, a32: Real, b32: Real, c32: Real, a33: Real, b33: Real, c33: Real,
                             a41: Real, b41: Real, c41: Real, a42: Real, b42: Real, c42: Real, a43: Real, b43: Real, c43: Real,
                             a51: Real, b51: Real, c51: Real, a52: Real, b52: Real, c52: Real, a53: Real, b53: Real, c53: Real,
                             a61: Real, b61: Real, c61: Real, a62: Real, b62: Real, c62: Real, a63: Real, b63: Real, c63: Real,
                             d1: Real, d2: Real, d3: Real): Real = {

    require((23.0 <= a11) && (a11 <= 26.0) && (-39.0 <= b11) && (b11 <= -36.0) && (96.0 <= c11) && (c11 <= 99.0) &&
      (69.0 <= a12) && (a12 <= 72.0) && (92.0 <= b12) && (b12 <= 95.0) && (25.0 <= c12) && (c12 <= 28.0) &&
      (-95.0 <= a13) && (a13 <= -92.0) && (-9.0 <= b13) && (b13 <= -6.0) && (52.0 <= c13) && (c13 <= 55.0) &&
      (26.0 <= a21) && (a21 <= 30.0) && (-36.0 <= b21) && (b21 <= -32.0) && (99.0 <= c21) && (c21 <= 103.0) &&
      (72.0 <= a22) && (a22 <= 76.0) && (95.0 <= b22) && (b22 <= 99.0) && (28.0 <= c22) && (c22 <= 32.0) &&
      (-92.0 <= a23) && (a23 <= -88.0) && (-6.0 <= b23) && (b23 <= -2.0) && (55.0 <= c23) && (c23 <= 59.0) &&
      (30.0 <= a31) && (a31 <= 34.0) && (-32.0 <= b31) && (b31 <= -28.0) && (103.0 <= c31) && (c31 <= 107.0) &&
      (76.0 <= a32) && (a32 <= 80.0) && (99.0 <= b32) && (b32 <= 103.0) && (32.0 <= c32) && (c32 <= 36.0) &&
      (-88.0 <= a33) && (a33 <= -84.0) && (-2.0 <= b33) && (b33 <= 2.0) && (59.0 <= c33) && (c33 <= 63.0) &&
      (34.0 <= a41) && (a41 <= 38.0) && (-28.0 <= b41) && (b41 <= -24.0) && (107.0 <= c41) && (c41 <= 111.0) &&
      (80.0 <= a42) && (a42 <= 84.0) && (103.0 <= b42) && (b42 <= 107.0) && (36.0 <= c42) && (c42 <= 40.0) &&
      (-84.0 <= a43) && (a43 <= -80.0) && (2.0 <= b43) && (b43 <= 6.0) && (63.0 <= c43) && (c43 <= 67.0) &&
      (38.0 <= a51) && (a51 <= 42.0) && (-24.0 <= b51) && (b51 <= -20.0) && (111.0 <= c51) && (c51 <= 115.0) &&
      (84.0 <= a52) && (a52 <= 88.0) && (107.0 <= b52) && (b52 <= 111.0) && (40.0 <= c52) && (c52 <= 44.0) &&
      (-80.0 <= a53) && (a53 <= -76.0) && (6.0 <= b53) && (b53 <= 10.0) && (67.0 <= c53) && (c53 <= 71.0) &&
      (42.0 <= a61) && (a61 <= 46.0) && (-20.0 <= b61) && (b61 <= -16.0) && (115.0 <= c61) && (c61 <= 119.0) &&
      (88.0 <= a62) && (a62 <= 92.0) && (111.0 <= b62) && (b62 <= 115.0) && (44.0 <= c62) && (c62 <= 48.0) &&
      (-76.0 <= a63) && (a63 <= -72.0) && (10.0 <= b63) && (b63 <= 14.0) && (71.0 <= c63) && (c63 <= 75.0) &&
      (1.0 <= d1) && (d1 <= 10.0) && (1.0 <= d2) && (d2 <= 10.0) && (1.0 <= d3) && (d3 <= 10.0))


    val x1: Real = solveEquationX(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)
    val y1: Real = solveEquationY(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)
    val z1: Real = solveEquationZ(a11, b11, c11, d1, a12, b12, c12, d2, a13, b13, c13, d3)


    val x2: Real = solveEquationX(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)
    val y2: Real = solveEquationY(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)
    val z2: Real = solveEquationZ(a21, b21, c21, d1, a22, b22, c22, d2, a23, b23, c23, d3)


    val x3: Real = solveEquationX(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)
    val y3: Real = solveEquationY(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)
    val z3: Real = solveEquationZ(a31, b31, c31, d1, a32, b32, c32, d2, a33, b33, c33, d3)

    val x4: Real = solveEquationX(a41, b41, c41, d1, a42, b42, c42, d2, a43, b43, c43, d3)
    val y4: Real = solveEquationY(a41, b41, c41, d1, a42, b42, c42, d2, a43, b43, c43, d3)
    val z4: Real = solveEquationZ(a41, b41, c41, d1, a42, b42, c42, d2, a43, b43, c43, d3)


    val x5: Real = solveEquationX(a51, b51, c51, d1, a52, b52, c52, d2, a53, b53, c53, d3)
    val y5: Real = solveEquationY(a51, b51, c51, d1, a52, b52, c52, d2, a53, b53, c53, d3)
    val z5: Real = solveEquationZ(a51, b51, c51, d1, a52, b52, c52, d2, a53, b53, c53, d3)


    val x6: Real = solveEquationX(a61, b61, c61, d1, a62, b62, c62, d2, a63, b63, c63, d3)
    val y6: Real = solveEquationY(a61, b61, c61, d1, a62, b62, c62, d2, a63, b63, c63, d3)
    val z6: Real = solveEquationZ(a61, b61, c61, d1, a62, b62, c62, d2, a63, b63, c63, d3)

    val avg = (x1 + y1 + z1 + x2 + y2 + z2 + x3 + y3 + z3 + x4 + y4 + z4 + x5 + y5 + z5 + x6 + y6 + z6) / 18.0
    avg
  }
}

