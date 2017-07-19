
package daisy


package object utils {

  case class DivisionByZeroException(s: String) extends Exception

  case class RationalCannotBeCastToIntException(s: String) extends Exception

  case class NegativeSqrtException(s: String) extends Exception
}