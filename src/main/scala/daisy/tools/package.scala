// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import tools.Rational

import Rational._

package object tools {

  case class DivisionByZeroException(s: String) extends Exception

  case class DenormalRangeException(s: String) extends Exception

  case class OverflowException(s: String) extends Exception

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