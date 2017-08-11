// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package solvers

import scala.collection.immutable.Seq
import _root_.smtlib.interpreters.Z3Interpreter
import _root_.smtlib.parser.Commands.{AttributeOption, SetOption}
import _root_.smtlib.parser.Terms._
import lang.Trees.Expr

// TODO has to be singleton
object Z3Solver {

  // needs to be populated before the first call to checkSat, currently
  // automatically populated on creation of first Context
  var context: Context = null

  val timeoutOption = AttributeOption(Attribute(
    SKeyword("timeout"), value = Some(SNumeral(1000))))

  // TODO set the option properly!!!!!!!!
  val decimalOption = AttributeOption(Attribute
  (SKeyword("pp.decimal"), value = Some(SSymbol("true"))))

  def checkSat(query: Expr): Option[Boolean] = {
    val solver = new Z3Solver(context)
    solver.emit(SetOption(timeoutOption))
    solver.assertConstraint(query)
    val res = solver.checkSat
    solver.free()
    res
  }

def checkAndGetModel(query: Expr): Option[Model] = {
  val solver = new Z3Solver(context)
  solver.emit(SetOption(timeoutOption))
  solver.emit(SetOption(decimalOption))
  solver.assertConstraint(query)
  val tmp = solver.checkSat
  val res = tmp match {
    case Some(true) => Some(solver.getModel)
    case Some(false) => None
    case None => None
  }
  solver.free()
  res
}

  // global counter of "Unknown"s or timeouts
  var unknownCounter = 0
}


class Z3Solver(context: Context) extends SMTLibSolver(context) {

  override def targetName: String = "z3"

  val interpreterOpts = Seq("-in", "-smt2")


  def getNewInterpreter: Z3Interpreter = {
    val opts = interpreterOpts
    reporter.debug("Invoking solver " + targetName + " with " + opts.mkString(" "))

    new Z3Interpreter("z3", opts.toArray)
  }
}
