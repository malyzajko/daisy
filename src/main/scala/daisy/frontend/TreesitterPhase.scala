package daisy
package frontend

import daisy.lang.Trees.{Program => DaisyProgram}

object TreesitterPhase extends DaisyPhase {
  override val name = "Treesitter Extraction"
  override val description = "Extracts functions from C and Scala source using Tree-sitter"
  override implicit val debugSection: DebugSection = DebugSectionFrontend

  override def runPhase(ctx: Context, prg: DaisyProgram): (Context, DaisyProgram) = {
    val frontend =
      ctx.lang match {
        case Main.ProgramLanguage.ScalaProgram => new TreesitterScala(ctx)
        case Main.ProgramLanguage.CProgram     => new TreesitterC(ctx)
        case Main.ProgramLanguage.FPCoreProgram => new TreesitterFPCore(ctx)
        case _ =>
          ctx.reporter.fatalError(
            s"Treesitter frontend does not support ${ctx.lang}")
      }
    frontend.run
  }
}
