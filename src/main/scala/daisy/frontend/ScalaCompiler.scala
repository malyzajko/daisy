  // Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import scala.tools.nsc.{Global,Settings=>NSCSettings}
import scala.reflect.internal.Positions

class ScalaCompiler(settings: NSCSettings, ctx: Context) extends Global(
  settings, new SimpleReporter(settings, ctx.reporter)) with Positions {

  object daisyExtraction extends {
    val global: ScalaCompiler.this.type = ScalaCompiler.this
    val runsAfter = List[String]("refchecks")
    val runsRightAfter = None
    val ctx = ScalaCompiler.this.ctx
  } with DaisyExtraction

  override protected def computeInternalPhases(): Unit = {
    val phs = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      patmat                  -> "translate match expressions",
      superAccessors          -> "add super accessors in traits and nested classes",
      extensionMethods        -> "add extension methods for inline classes",
      pickler                 -> "serialize symbol tables",
      refChecks               -> "reference/override checking, translate nested objects",
      daisyExtraction          -> "extracts daisy trees out of scala trees"
    )
    phs foreach { phasesSet += _._1 }
  }

  class Run extends super.Run {
    override def progress(current: Int, total: Int): Unit = {
      ctx.reporter.onCompilerProgress(current, total)
    }
  }
}