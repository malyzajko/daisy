// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import lang.Trees.Program
import utils._

import scala.tools.nsc.{Settings,CompilerCommand}
import java.io.File

object ExtractionPhase extends PhaseComponent {
  override val name = "Scalac Extraction"
  override val description = "Extraction of trees from the Scala Compiler"
  override def apply(cfg: Config) = new ExtractionPhase(cfg, name, "frontend")
}

class ExtractionPhase(val cfg: Config, val name: String, val shortName: String) extends DaisyPhase {
  implicit val debug = DebugSectionFrontend

  def run(ctx: Context, prg: Program): (Context, Program) = {
    startRun()

    val settings = new Settings

    // def getFiles(path: String): Option[Array[String]] =
    //   scala.util.Try(new File(path).listFiles().map{ _.getAbsolutePath }).toOption

    val scalaLib = Option(scala.Predef.getClass.getProtectionDomain.getCodeSource).map{
      _.getLocation.getPath
    }.getOrElse(cfg.reporter.fatalError(
      "No Scala library found."
    ))

    // .orElse( for {
    //   // We are in Eclipse. Look in Eclipse plugins to find scala lib
    //   eclipseHome <- Option(System.getenv("ECLIPSE_HOME"))
    //   pluginsHome = eclipseHome + "/plugins"
    //   plugins <- getFiles(pluginsHome)
    //   path <- plugins.find{ _ contains "scala-library"}
    // } yield path).getOrElse( ctx.reporter.fatalError(
    //   "No Scala library found. If you are working in Eclipse, " +
    //   "make sure to set the ECLIPSE_HOME environment variable to your Eclipse installation home directory"
    // ))

    settings.classpath.value   = scalaLib
    settings.usejavacp.value   = false
    settings.deprecation.value = true
    settings.Yrangepos.value   = true
    settings.skip.value        = List("patmat")

    // this may fail, since the libFiles are relative to the main directory
    val compilerOpts = cfg.option[List[String]]("libFiles").toList ++ List(cfg.option[String]("file"))

    val command = new CompilerCommand(compilerOpts, settings) {
      override val cmdName = "daisy"
    }

    if(command.ok) {
    // Debugging code for classpath crap
    // new scala.tools.util.PathResolver(settings).Calculated.basis.foreach { cp =>
    //   cp.foreach( p =>
    //     cfg.reporter.debug(" => "+p.toString)
    //   )
    // }

    val compiler = new ScalaCompiler(settings, cfg)
    val run = new compiler.Run
    run.compile(command.files)

    compiler.daisyExtraction.compiledProgram match {
        case Some(pgm) => finishRun(ctx, pgm)
        case None => cfg.reporter.fatalError("Failed to extract Daisy program.")
      }
    } else {
      cfg.reporter.fatalError("No input program.")
    }
  }
}