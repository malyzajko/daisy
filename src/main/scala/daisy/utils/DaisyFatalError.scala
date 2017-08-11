// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

case class DaisyFatalError(msg: Option[String]) extends Exception(msg.getOrElse(""))

object DaisyFatalError {
  def apply(msg: String): DaisyFatalError = new DaisyFatalError(Some(msg))
}
