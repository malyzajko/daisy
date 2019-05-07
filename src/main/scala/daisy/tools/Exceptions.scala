// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.tools

case class DivisionByZeroException(s: String) extends Exception

case class DenormalRangeException(s: String) extends Exception

case class OverflowException(s: String) extends Exception

case class RationalCannotBeCastToIntException(s: String) extends Exception

case class NegativeSqrtException(s: String) extends Exception

case class NoRelativeErrorException(s: String) extends Exception

case class NonPositiveLogException(s: String) extends Exception

case class ArcOutOfBoundsException(s: String) extends Exception
