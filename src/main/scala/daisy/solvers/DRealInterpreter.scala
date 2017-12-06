// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package solvers

import scala.concurrent.{Future, blocking, ExecutionContext, Await, TimeoutException}
import scala.concurrent.duration._

import _root_.smtlib.interpreters.ProcessInterpreter
import _root_.smtlib.parser.Commands._
import _root_.smtlib.parser.Terms.{SExpr, SSymbol}
import _root_.smtlib.printer.{RecursivePrinter => SMTPrinter}
import _root_.smtlib.parser.CommandsResponses.{Error => ErrorResponse, _}

class DRealInterpreter(executable: String, args: Array[String])
  extends ProcessInterpreter(executable, args) {

  /**
   * if this is set, we have already seen a check-sat query and the solver is
   * therefore useless.
   */
  private var invalidated: Boolean = false

  /** A custom parsing function for the check-sat result.
   *
   * This is necessary because dReal outputs
   * "delta-sat with delta = <some number>"
   * instead of "sat".
   * It makes use of internal parser functions and can therefore be considered
   * a HACK.
   */
  private def parseCheckSatResult(): SExpr = {
    // In the case of "delta-sat", further tokens which need to be consumed
    // would be following, however as the solver is unusable after a check-sat
    // query anyway, this does not matter.
    parser.parseSymbol match {
      case SSymbol("delta-sat") => CheckSatStatus(SatStatus)
      case SSymbol("unsat") => CheckSatStatus(UnsatStatus)
      case SSymbol("unknown") => CheckSatStatus(UnknownStatus)
      case SSymbol("unsupported") => Unsupported
      case _ => CheckSatStatus(UnknownStatus)
    }
  }

  /** A custom evaluation function for solver queries.
   *
   * This function has to be overridden to handle the inconvenient interactive
   * behavior of dReal. dReal first reads all queries over stdin and waits for
   * the pipe to it being closed (in contrast to writing the answer as soon as
   * the query was read).
   * Therefore, this method just writes all queries to the solver until a
   * check-sat is queried. Then, it closes the pipe, parses the result, and
   * makes sure that no further query is sent to the process.
   * This is a HACK that prohibits any incremental use of the solver.
   */
  override def eval(cmd: SExpr): SExpr = {
    if (invalidated) {
      ErrorResponse("Solver encountered query after check-sat")
    } else {
      try {
        SMTPrinter.printSExpr(cmd, in)
        in.write("\n")
        in.flush

        cmd match {
          case CheckSat() =>
            // if we are checking the query, close the pipe and parse the response
            SMTPrinter.printCommand(Exit(), in)
            in.write("\n")
            in.flush()
            in.close()

            // dReal does not support timeouts, so we have to use this Future
            // construction to achieve similar results.
            val f = Future {
              blocking {
                parseCheckSatResult()
              }
            } (ExecutionContext.global)

            try {
              val p: Int = 1
              Await.result(f, p.second) // TODO parameter?
            } catch {
              case e: TimeoutException => CheckSatStatus(UnknownStatus)
            } finally {
              // now kill the process
              invalidated = true
              this.free()
              kill()
            }

          case _ =>
            // otherwise, just accept further input
            Success
        }
      } catch {
        case (ex: Exception) => {
          if (cmd == CheckSat()) {
            CheckSatStatus(UnknownStatus)
          } else {
            ErrorResponse("Solver encountered exception: " + ex)
          }
        }
      }
    }
  }
}
