// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import lang.Trees.Program

/**
  A common trait for everything that processes command line options.
 */
trait Component {
  val name: String
  val description: String
  val definedOptions: Set[CmdLineOption[Any]] = Set()
}

trait PhaseComponent extends Component {
  def apply(cfg: Config): DaisyPhase
}