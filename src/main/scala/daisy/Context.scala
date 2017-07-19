
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

import lang.Identifiers._
import utils.{Interval, PartialInterval, Rational}

case class Context(
  reporter: Reporter,
  options: Seq[CmdLineOption[Any]],  // all command-line options

  files: Seq[String],
  timers: TimerStorage = new TimerStorage,
  libFiles: Seq[String] = Seq(
    //"library/annotation/package.scala",
    "library/lang/Real.scala"
    ),
  // Information we want to persist through phases,
  // but don't want to pollute the nice and clean trees.
  // If these get too many, move to their own "Summary".
  // indexed by FunDef.id
  inputRanges: Map[Identifier, Map[Identifier, Interval]] = Map(),
  inputErrors: Map[Identifier, Map[Identifier, Rational]] = Map(),
  // for now we only support a single result value, i.e. no tuples
  // this map is indexed by fnc.id -> potentially partial interval bound of result
  // and similar for the errors
  resultRangeBounds: Map[Identifier, PartialInterval] = Map(),
  resultErrorBounds: Map[Identifier, Rational] = Map()
  //requiredOutputRanges: Map[Identifier, Map[Identifier, PartialInterval]] = Map(),
  //requiredOutputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
) {

  // on the first creation of a context, we also update the context variable
  // in Solver, so that it does not get forgotten.
  if (!solvers.Solver.hasContext()) {
    solvers.Solver.setContext(this)
  }

  def hasFlag(name: String): Boolean = {
    options.contains(FlagOption(name))
  }

  def findOption[T: ClassTag](opt: CmdLineOptionDef[T]): Option[T] = {
    options.collectFirst {
      case CmdLineOption(n, value: T) if (n == opt.name) =>
        value
    }
  }
}
