
package daisy

import utils.Rational

import Rational._

package object utils {

  case class DivisionByZeroException(s: String) extends Exception

  case class RationalCannotBeCastToIntException(s: String) extends Exception

  case class NegativeSqrtException(s: String) extends Exception

  case class NoRelativeErrorException(s: String) extends Exception

  def getRelativeError(i: Interval, maxAbsError: Rational): Option[Rational] = {
    if (i.xlo <= zero && zero <= i.xhi) {
      None
    } else {
      Some(max(abs(maxAbsError / i.xlo), abs(maxAbsError / i.xhi)))
    }
  }
}