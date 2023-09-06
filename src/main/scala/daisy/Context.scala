// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.reflect.ClassTag
import lang.Trees.{Expr, Program, FunDef}
import lang.Identifiers._
import tools.{DSAbstraction, FinitePrecision, Interval, PartialInterval, Rational}
import FinitePrecision.{FixedPrecision, Precision}
import daisy.Main.ProgramLanguage
import daisy.Main.ProgramLanguage._

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Seq

case class Context(
  file: String,
  lang: ProgramLanguage.Value = ScalaProgram,
  options: Map[String, Any],
  initReport: ArrayBuffer[String] = new ArrayBuffer[String],
  originalProgram: Program = null,
  recomputeAbsErrors: Boolean = false,

  timers: TimerStorage = new TimerStorage,

  libFiles: List[String] = List(System.getProperty("user.dir")+"/library/Real.scala",
    System.getProperty("user.dir")+"/library/Vector.scala"),

  // Information we want to persist through phases,
  // but don't want to pollute the nice and clean trees.
  // If these get too many, move to their own "Summary".
  // indexed by FunDef.id
  codegenOutput: StringBuilder = new StringBuilder(),
  specInputRanges: Map[Identifier, Map[Identifier, Interval]] = Map(),
  specInputErrors: Map[Identifier, Map[Identifier, Rational]] = Map(),
  specInputPrecisions: Map[Identifier, Map[Identifier, Precision]] = Map(),
  specResultPrecisions: Map[Identifier, Precision] = Map(),
  specAdditionalConstraints: Map[Identifier, Expr] = Map(),

  // for now we only support a single result value, i.e. no tuples
  // this map is indexed by fnc.id -> potentially partial interval bound of result
  // and similar for the errors
  specResultRangeBounds: Map[Identifier, PartialInterval] = Map(),
  specResultErrorBounds: Map[Identifier, Rational] = Map(),

  // stored IDs which were used during untyping
  resultTupleIds: Map[Identifier, Seq[Identifier]] = Map(),

  // if Daisy chooses uniform precision, which one it chose
  uniformPrecisions: Map[Identifier, Precision] = Map(),
  assignedPrecisions: Map[Identifier, Map[Identifier, Precision]] = Map(),
  // the analysed/computed roundoff errors for each function
  resultAbsoluteErrors: Map[Identifier, Rational] = Map(),
  resultRealRanges: Map[Identifier, Interval] = Map(),
  resultNumberSamples: Map[Identifier, Long] = Map(),
  // a None value indicates no relative error could be computed
  resultRelativeErrors: Map[Identifier, Option[Rational]] = Map(),

  // PathCond (same as Seq[Boolean]) captures the path to the expression in (possibly nested) if-the-else statements
  // true denotes (part of the) path to expressions in then branch, false - in else branch,
  // empty Seq() means that expression is not inside if-then-else branches

  // intermediate ranges and errors, which are needed e.g. for fixed-point codegen
  intermediateAbsErrors: Map[Identifier, Map[(Expr, Seq[Expr]), Rational]] = Map(),
  // real-valued ranges
  intermediateRanges: Map[Identifier, Map[(Expr, Seq[Expr]), Interval]] = Map(),

  // errors reported by Metalibm
  approxReportedErrors: Map[Identifier, Map[Expr, Rational]] = Map(),

  seed:  Long = -1,
  // wrapper functions needed to call Metalibm generated approximations
  metalibmWrapperFunctions: Seq[String] = Seq(),
  metalibmGeneratedFiles: Seq[String] = Seq(),

  originalFunctions: Map[Identifier, FunDef] = Map(),
  dsAbstractions: Map[Identifier, Map[Expr, DSAbstraction]] = Map(),
  errMsg: Option[String] = None

) {

  val reporter = new DefaultReporter(
    option[List[DebugSection]]("debug").toSet,
    silent = hasFlag("silent"),
    report = initReport
  )

  val fixedPoint: Boolean = option[Precision]("precision") match {
    case FixedPrecision(_) => true
    case _ => option[String]("choosePrecision") == "fixed"
  }

  def option[T: ClassTag](name: String): T = options.get(name) map {
    case x: T => x
    case x: AnyRef => reporter.fatalError(s"Option $name ($x) has wrong type")
  } getOrElse reporter.fatalError(s"Unknown option $name")

  def hasFlag(name: String): Boolean = option[Boolean](name)
}
