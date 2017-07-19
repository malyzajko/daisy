
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy
package solvers

import scala.collection.immutable.Seq

import _root_.smtlib.interpreters.Z3Interpreter
import _root_.smtlib.parser.Commands.{SetOption, AttributeOption}
import _root_.smtlib.parser.Terms.{SKeyword, Attribute, SNumeral}

import lang.Trees.Expr

object Z3Solver {

  // needs to be populated before the first call to checkSat, currently
  // automatically populated on creation of first Context
  var context: Context = null

  val timeoutOption = AttributeOption(Attribute(
    SKeyword("timeout"), value = Some(SNumeral(1000))))

  def checkSat(query: Expr): Option[Boolean] = {
    val solver = new Z3Solver(context)
    solver.emit(SetOption(timeoutOption))
    solver.assertConstraint(query)
    val res = solver.checkSat
    solver.free()
    res
  }

  // global counter of "Unknown"s or timeouts
  var unknownCounter = 0
}


class Z3Solver(context: Context) extends SMTLibSolver(context) {

  override def targetName = "z3"

  val interpreterOpts = Seq("-in", "-smt2")


  def getNewInterpreter = {
    val opts = interpreterOpts
    reporter.debug("Invoking solver "+targetName+" with "+opts.mkString(" "))

    new Z3Interpreter("z3", opts.toArray)
  }
}