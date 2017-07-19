
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy

import lang.Trees.Tree

package object solvers {

  class CantResetException(s: SMTLibSolver) extends Exception(s"Unable to reset solver $s")

  case class SMTLIBUnsupportedError(t: Tree, s: SMTLibSolver, reason: Option[String] = None)
    extends Exception(s"$t is unsupported by ${s.targetName}" + reason.map(":\n  " + _ ).getOrElse(""))

  case class SolverUnsupportedError(t: Tree, s: SMTLibSolver, reason: Option[String] = None)
    extends Exception(s"$t is unsupported by solver ${s.name}" + reason.map(":\n  " + _ ).getOrElse(""))

}