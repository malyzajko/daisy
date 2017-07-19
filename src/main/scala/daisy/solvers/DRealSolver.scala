
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy
package solvers

import scala.collection.immutable.Seq

import _root_.smtlib.parser.Commands._
import _root_.smtlib.parser.Terms.SExpr
import _root_.smtlib.printer.{RecursivePrinter => SMTPrinter}
import _root_.smtlib.parser.CommandsResponses.{Error => ErrorResponse, _}

import lang.Trees._
import lang.Identifiers._

object DRealSolver {

  // needs to be populated before the first call to checkSat, currently
  // automatically populated on creation of first Context
  var context: Context = null

  def checkSat(query: Expr): Option[Boolean] = {
    val solver = new DRealSolver(context)
    solver.writeLogic()
    solver.assertConstraint(query)
    val res = solver.checkSat
    solver.free()
    res
  }

  // global counter of "Unknown"s or timeouts
  var unknownCounter = 0
}

class DRealSolver(context: Context) extends SMTLibSolver(context) {

  override def targetName = "dReal"

  // TODO make a parameter?
  val precision = 0.0000000000000000000001

  val interpreterOpts = Seq("--in", "--precision "+precision)

  def writeLogic() = {
    // without this, dReal segfaults
    emit(SetLogic(QF_NRA))
  }

  override protected def emit(cmd: SExpr, rawOut: Boolean = false): SExpr = {
    debugOut foreach { o =>
      SMTPrinter.printSExpr(cmd, o)
      o.write("\n")
      o.flush()
    }

    SMTPrinter.printSExpr(cmd, commandBuffer)
    commandBuffer.write("\n")
    commandBuffer.flush()

    interpreter.eval(cmd) match {
      case err @ ErrorResponse(msg) if !rawOut =>
        reporter.warning(s"Unexpected error from $targetName solver: $msg")
        // Store that there was an error. Now all following check()
        // invocations will return None
        addError()
        err
      case res =>
        res
    }
  }

  // model generation is not supported for dReal
  override protected def getModel(filter: Identifier => Boolean): Model = ???

  def getNewInterpreter = {
    val opts = interpreterOpts
    reporter.debug("Invoking solver "+targetName+" with "+opts.mkString(" "))

    new DRealInterpreter("dReal", opts.toArray)
  }
}
