package daisy
package tools

abstract sealed class MonotonicityPhase
case class Rising() extends MonotonicityPhase
case class Falling() extends MonotonicityPhase
case class Mixed() extends MonotonicityPhase

object MonotonicityPhase {

  /** Determines whether the sine function over this interval is rising, falling
   *  or not monotone
   */
  def getMonotonicityPhaseSine(iv: Interval): MonotonicityPhase = {
    def mod(a: BigInt, b: BigInt): BigInt = {(a % b + b) % b}

    val (_, (posLo, posHi)) = iv.reduceRange()

    if (posHi - posLo >= 4) {
      Mixed()
    } else {
      (mod(posLo, 4).toInt, mod(posHi, 4).toInt) match {
        case (0, 0) | (3, 0) | (3, 3) => Rising()
        case (1, 1) | (1, 2) | (2, 2) => Falling()
        case _ => Mixed()
      }
    }
  }
  
  /** Determines whether the sine function over this interval is rising, falling
   *  or not monotone for Interval using MPFRFloat
   */
  def getMonotonicityPhaseSine(iv: MPFRInterval): MonotonicityPhase = {
    def mod(a: BigInt, b: BigInt): BigInt = {(a % b + b) % b}

    val (_, (posLo, posHi)) = iv.reduceRange()

    if (posHi - posLo >= 4) {
      Mixed()
    } else {
      (mod(posLo, 4).toInt, mod(posHi, 4).toInt) match {
        case (0, 0) | (3, 0) | (3, 3) => Rising()
        case (1, 1) | (1, 2) | (2, 2) => Falling()
        case _ => Mixed()
      }
    }
  }
}
