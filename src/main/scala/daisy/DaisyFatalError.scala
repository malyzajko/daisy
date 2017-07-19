
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */


package daisy

case class DaisyFatalError(msg: Option[String]) extends Exception(msg.getOrElse(""))

object DaisyFatalError {
  def apply(msg: String) = new DaisyFatalError(Some(msg))
}
