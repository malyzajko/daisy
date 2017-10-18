
import daisy.lang._
import Real._

/*
  These benchmarks were used by the Real2Float tool,
  and come from the proof of the Kepler conjecture
  Introduction to the flyspec project, T.C. Hales, Dagstuhl 2006
*/
object Kepler {

  def kepler0(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(4 <= x1 && x1 <= 6.36 && 0.0001 <= x2 && x2 <= 0.00015 && 4.63 <= x3 && x3 <= 6306 &&
      -10 <= x4 && x4 <= -0.01 && 4 <= x5 && x5 <= 6.36 && 4 <= x6 && x6 <= 6.36)

    x2 * x5 + x3 * x6 - x2 * x3 - x5 * x6 + x1 * (-x1 + x2 + x3 - x4 + x5 + x6)

  } // 1.15e-15


  def kepler1(x1: Real, x2: Real, x3: Real, x4: Real): Real = {
    require(4 <= x1 && x1 <= 6.36 && 0.04 <= x2 && x2 <= 0.0636 && 40 <= x3 && x3 <= 6300.6 &&
      0.001 <= x4 && x4 <= 0.015)

    x1 * x4 * (-x1 + x2 + x3 - x4) + x2 * (x1 - x2 + x3 + x4) + x3 * (x1 + x2 - x3 + x4) -
      x2 * x3 * x4 - x1 * x3 - x1 * x2 - x4

  } // 4.50e–13

  def kepler2(x1: Real, x2: Real, x3: Real, x4: Real, x5: Real, x6: Real): Real = {
    require(4 <= x1 && x1 <= 6.36 && 0 <= x2 && x2 <= 0 && 40 <= x3 && x3 <= 63.6 &&
      0 <= x4 && x4 <= 0 && 4 <= x5 && x5 <= 6.36 && 4 <= x6 && x6 <= 6000.36)

      x1 * x4 * (-x1 + x2 + x3 - x4 + x5 + x6) + x2 * x5 * (x1 - x2 + x3 + x4 - x5 + x6) +
        x3* x6 * (x1 + x2 - x3 + x4 + x5 - x6) - x2 * x3 * x4 -
          x1* x3* x5 - x1 * x2 * x6 - x4 * x5 * x6

  } //2.08e–12
}