
import daisy.lang._
import Real._

/*
  These benchmarks were used by the Real2Float tool,
  and come from the proof of the Kepler conjecture
  Introduction to the flyspec project, T.C. Hales, Dagstuhl 2006
*/
object KeplerSingle {

  def kepler0(x1: Real): Real = {
    require(4 <= x1 && x1 <= 6.36)

    val x2: Real = 4 // 4 <= x2 && x2 <= 6.36
    val x3: Real = 4.03  //4 <= x3 && x3 <= 6.36
    val x4: Real = 6.36 //4 <= x4 && x4 <= 6.36
    val x5: Real = 6.2 //4 <= x5 && x5 <= 6.36
    val x6: Real = 5.18 //4 <= x6 && x6 <= 6.36

    4 * 6.2 + 4.03 * 5.18 - 4 * 4.03 - 6.2 * 5.18 + x1 * (-x1 + 4 + 4.03 - 6.36 + 6.2 + 5.18)

  } // 1.15e-15


  def kepler1(x1: Real): Real = {
    require(4 <= x1 && x1 <= 6.36)
    val x2: Real = 4 // 4 <= x2 && x2 <= 6.36
    val x3: Real = 4.03  //4 <= x3 && x3 <= 6.36
    val x4: Real = 6.36 //4 <= x4 && x4 <= 6.36

    x1 * 6.36 * (-x1 + 4 + 4.03 - 6.36) + 4 * (x1 - 4 + 4.03 + 6.36) + 4.03 * (x1 + 4 - 4.03 + 6.36) -
      4 * 4.03 * 6.36 - x1 * 4.03 - x1 * 4 - 6.36

  } // 4.50e–13

  def kepler2(x1: Real): Real = {
    require(4 <= x1 && x1 <= 6.36)

    val x2: Real = 4 // 4 <= x2 && x2 <= 6.36
    val x3: Real = 4.03  //4 <= x3 && x3 <= 6.36
    val x4: Real = 6.36 //4 <= x4 && x4 <= 6.36
    val x5: Real = 6.2 //4 <= x5 && x5 <= 6.36
    val x6: Real = 5.18 //4 <= x6 && x6 <= 6.36

      x1 * 6.36 * (-x1 + 4 + 4.03 - 6.36 + 6.2 + 5.18) + 4 * 6.2 * (x1 - 4 + 4.03 + 6.36 - 6.2 + 5.18) +
        4.03* 5.18 * (x1 + 4 - 4.03 + 6.36 + 6.2 - 5.18) - 4 * 4.03 * 6.36 -
          x1* 4.03* 6.2 - x1 * 4 * 5.18 - 6.36 * 6.2 * 5.18

  } //2.08e–12
}