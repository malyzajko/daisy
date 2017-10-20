// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.collection.immutable.Seq
import lang.Trees.{FunDef, Program}

/** One logical analysis/synthesis step, e.g. computing ranges, replacing trig functions.
 * This should be as immutable as possible so that we can (possibly) run it in parallel.
 */
abstract class DaisyPhase extends Pipeline[Program, Program] {
  val cfg: Config
  val name: String
  val shortName: String

  /**
    * Starts timer and logs the start of this phase
    */
  def startRun(): Unit = {
    cfg.timers.get(shortName).start
    cfg.reporter.info(s"\nStarting ${name} phase")
  }

  override def run(ctx: Context, v: Program): (Context, Program)

  /**
    * Stops timer and logs end of this phase. Arguments are passed through.
    */
  def finishRun(ctx: Context, prg: Program): (Context, Program) = {
    cfg.timers.get(shortName).stop
    // cfg.reporter.info(s"Finished ${name} phase\n")
    (ctx,prg)
  }

  def functionsToConsider(prg: Program, requirePrecond: Boolean = true): Seq[FunDef] = {
    var funs = prg.defs
    funs = funs.filter(_.body.isDefined)
    if (requirePrecond) {
      funs = funs.filter(_.precondition.isDefined)
    }
    cfg.option[List[String]]("functions") match {
      case Nil =>
      case fncs => funs = funs.filter(f => fncs.contains(f.id.toString))
    }
    funs
  }
}
