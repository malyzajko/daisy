

import daisy.lang._
import Real._


object SolveCubic {

  def solveCubic(a: Real, b: Real, c: Real) = {
    require(5 <= a && a <= 10 && 0 <= b && b <= 5 && 0 <= c && c <= 5)
    (2 * a * a * a - 9 * a * b + 27 * c) / 54 + (a * a - 3 * b) / 9
  }

}