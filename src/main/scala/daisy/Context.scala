// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

import lang.Trees.Expr
import lang.Identifiers._
import tools.{Interval, PartialInterval, Rational, FinitePrecision}
import FinitePrecision.Precision

case class Context(
  reporter: Reporter,
  options: Seq[CmdLineOption[Any]],  // all command-line options

  files: Seq[String],
  timers: TimerStorage = new TimerStorage,
  libFiles: Seq[String] = Seq(
    "library/Real.scala"
    ),
  // Information we want to persist through phases,
  // but don't want to pollute the nice and clean trees.
  // If these get too many, move to their own "Summary".
  // indexed by FunDef.id
  specInputRanges: Map[Identifier, Map[Identifier, Interval]] = Map(),
  specInputErrors: Map[Identifier, Map[Identifier, Rational]] = Map(),
  specMixedPrecisions: Map[Identifier, Map[Identifier, Precision]] = Map(),
  specInferredReturnTypes: Map[Identifier, Precision] = Map(),
  fixedPoint: Boolean = false,

  // for now we only support a single result value, i.e. no tuples
  // this map is indexed by fnc.id -> potentially partial interval bound of result
  // and similar for the errors
  specResultRangeBounds: Map[Identifier, PartialInterval] = Map(),
  specResultErrorBounds: Map[Identifier, Rational] = Map(),

  // the analysed/computed roundoff errors for each function
  resultAbsoluteErrors: Map[Identifier, Rational] = Map(),
  resultRealRanges: Map[Identifier, Interval] = Map(),
  // a None value indicates no relative error could be computed
  resultRelativeErrors: Map[Identifier, Option[Rational]] = Map(),

  // intermediate ranges and errors, which are needed e.g. for fixed-point codegen
  intermediateAbsErrors: Map[Identifier, Map[Expr, Rational]] = Map(),
  // real-valued ranges
  intermediateRanges: Map[Identifier, Map[Expr, Interval]] = Map()
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
