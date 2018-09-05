// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import daisy.lang.Identifiers.Identifier

import scala.collection.immutable.Seq
import lang.Trees.{FunDef, Program}

/** One logical analysis/synthesis step, e.g. computing ranges, replacing trig functions.
 * This should be as immutable as possible so that we can (possibly) run it in parallel.
 */
trait DaisyPhase extends Pipeline[Program, Program] {
  val name: String
  val shortName: String
  // TODO: do we need this?
  val description: String
  val definedOptions: Set[CmdLineOption[Any]] = Set()

  def runPhase(ctx: Context, prog: Program): (Context, Program)

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    ctx.timers.get(shortName).start()
    ctx.reporter.info(s"\nStarting ${name} phase")

    val (_ctx, _prg) = runPhase(ctx, prg)

    _ctx.timers.get(shortName).stop()
    // cfg.reporter.info(s"Finished ${name} phase\n")
    (_ctx, _prg)
  }

  def functionsToConsider(ctx:Context, prg: Program, requirePrecond: Boolean = true): Seq[FunDef] = {
    var funs = prg.defs
    funs = funs.filter(_.body.isDefined)
    if (requirePrecond) {
      funs = funs.filter(_.precondition.isDefined)
    }
    ctx.option[List[String]]("functions") match {
      case Nil =>
      case fncs => funs = funs.filter(f => fncs.contains(f.id.toString))
    }
    funs
  }

  def analyzeConsideredFunctions[T](ctx: Context, prg: Program)(f: FunDef => T): Map[Identifier, T] = {
    functionsToConsider(ctx, prg).map(fnc => fnc.id -> f(fnc)).toMap
  }

  def transformConsideredFunctions(ctx: Context, prg: Program)(f: FunDef => FunDef): Seq[FunDef] = {
    functionsToConsider(ctx, prg).map(fnc => f(fnc))
    // prg.defs.map{ fnc =>
    //   if (functionsToConsider(ctx, prg).contains(fnc)) f(fnc) else fnc
    // }
  }
}
