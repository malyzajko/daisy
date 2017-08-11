// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import lang.Trees.Tree

package object solvers {

  class CantResetException(s: SMTLibSolver) extends Exception(s"Unable to reset solver $s")

  case class SMTLIBUnsupportedError(t: Tree, s: SMTLibSolver, reason: Option[String] = None)
    extends Exception(s"$t is unsupported by ${s.targetName}" + reason.map(":\n  " + _).getOrElse(""))

  case class SolverUnsupportedError(t: Tree, s: SMTLibSolver, reason: Option[String] = None)
    extends Exception(s"$t is unsupported by solver ${s.name}" + reason.map(":\n  " + _).getOrElse(""))

}