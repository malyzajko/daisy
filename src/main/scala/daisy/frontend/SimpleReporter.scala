// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.AbstractReporter

import scala.reflect.internal.util.{Position, NoPosition, FakePos}
import utils.{Position => DaisyPosition, NoPosition => DaisyNoPosition, OffsetPosition => DaisyOffsetPosition}

/** This implements a reporter that calls the callback with every line that a
regular ConsoleReporter would display. */
class SimpleReporter(val settings: Settings, reporter: daisy.Reporter) extends AbstractReporter {
  final val errorLimit = 5

  private def label(severity: Severity): String = severity match {
    case ERROR   => "error"
    case WARNING => "warning"
    case INFO    => null
  }

  private def clabel(severity: Severity): String = {
    val label0 = label(severity)
    if (label0 eq null) "" else label0 + ": "
  }

  /** Prints the message. */
  def printMessage(msg: String, pos: DaisyPosition, severity: Severity): Unit = {
    severity match {
      case ERROR =>
        reporter.error(pos, msg)
      case WARNING =>
        reporter.warning(pos, msg)
      case INFO =>
        reporter.info(pos, msg)
    }
  }

  /** Prints the message with the given position indication. */
  def printMessage(posIn: Position, msg: String, severity: Severity): Unit = {
    val pos = if (posIn eq null) {
      NoPosition
    } else if (posIn.isDefined) {
      posIn.finalPosition
    } else {
      posIn
    }
    pos match {
      case FakePos(fmsg) =>
        printMessage(fmsg + " " + msg, DaisyNoPosition, severity)
      case NoPosition =>
        printMessage(msg, DaisyNoPosition, severity)
      case _ =>
        val lpos = DaisyOffsetPosition(pos.line, pos.column, pos.point, pos.source.file.file)
        printMessage(msg, lpos, severity)
    }
  }

  def print(pos: Position, msg: String, severity: Severity): Unit = {
    printMessage(pos, clabel(severity) + msg, severity)
  }

  def display(pos: Position, msg: String, severity: Severity): Unit = {
    severity.count += 1
    if (severity != ERROR || severity.count <= errorLimit) {
      print(pos, msg, severity)
    }
  }

  def displayPrompt(): Unit = {}
}