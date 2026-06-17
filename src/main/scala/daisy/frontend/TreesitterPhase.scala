package daisy
package frontend

import io.github.treesitter.jtreesitter.Node
import daisy.lang.Trees.{Expr => DaisyExpr, Program => DaisyProgram}

object TreesitterPhase extends DaisyPhase with TreesitterC with TreesitterScala {
  override val name = "Treesitter Extraction"
  override val description = "Extracts functions from C and Scala source using Tree-sitter"
  override implicit val debugSection: DebugSection = DebugSectionFrontend

  override protected def preconditionName: String =
    if (currentCtx != null && currentCtx.lang == Main.ProgramLanguage.ScalaProgram) "require"
    else "__PRECOND"

  override protected def convertNode(raw: Node): DaisyExpr =
    if (currentCtx.lang == Main.ProgramLanguage.ScalaProgram) convertScalaNode(raw)
    else convertCNode(raw)

  def runPhase(ctx: Context, prg: DaisyProgram): (Context, DaisyProgram) =
    if (ctx.lang == Main.ProgramLanguage.ScalaProgram) runScalaTreesitter(ctx, prg)
    else runCTreesitter(ctx, prg)
}