
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */
package daisy

import lang.Trees.Program

/**
  A common trait for everything that processes command line options.
  */
trait Component {
  val name : String
  val description : String

  val definedOptions : Set[CmdLineOptionDef[Any]] = Set()

  def functionsToConsider(ctx:Context, prg: Program): Seq[String] = {
    ctx.findOption(Main.optionFunctions) match {
      case Some(fncs) => fncs
      case None => prg.defs.map(_.id.toString)
    }
  }
}
