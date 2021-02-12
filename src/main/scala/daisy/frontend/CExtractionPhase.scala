// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import lang.Trees._
import scala.reflect.io.File

object CExtractionPhase extends DaisyPhase with CASTExtractor {
  override val name = "C Extraction"
  override val description = "Extraction of trees from the C Compiler"

  override implicit val debugSection = DebugSectionFrontend

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val specFile = ctx.option[Option[String]]("spec")
    specFile match {
      case None => throw new DaisyFatalError(Some("Specification file is not defined"))
      case Some(x) if !File(specFile.get).exists => throw new DaisyFatalError(Some(s"Specification file $x is not found"))
      case _ =>
    }

    val prgOpt = CodeParser.getProgram(ctx.file, specFile.get)

    val prg = prgOpt match {
      case None => ctx.reporter.fatalError("Failed to extract Daisy program. Source: ${ctx.file}")
      case Some(x) => x
    }
    (ctx, prg)
  }
}