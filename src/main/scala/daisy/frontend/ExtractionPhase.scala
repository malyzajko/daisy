// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import lang.Trees.Program

import scala.tools.nsc.{Settings,CompilerCommand}

object ExtractionPhase extends DaisyPhase {
  override val name = "Scalac Extraction"
  override val shortName = "frontend"
  override val description = "Extraction of trees from the Scala Compiler"

  implicit val debug = DebugSectionFrontend

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val settings = new Settings

    // def getFiles(path: String): Option[Array[String]] =
    //   scala.util.Try(new File(path).listFiles().map{ _.getAbsolutePath }).toOption

    val scalaLib = Option(scala.Predef.getClass.getProtectionDomain.getCodeSource).map{
      _.getLocation.getPath
    }.getOrElse(ctx.reporter.fatalError(
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
    val compilerOpts = ctx.libFiles ++ List(ctx.file)

    val command = new CompilerCommand(compilerOpts, settings) {
      override val cmdName = "daisy"
    }

    if(command.ok) {
    // Debugging code for classpath crap
    // new scala.tools.util.PathResolver(settings).Calculated.basis.foreach { cp =>
    //   cp.foreach( p =>
    //     ctx.reporter.debug(" => "+p.toString)
    //   )
    // }

    val compiler = new ScalaCompiler(settings, ctx)
    val run = new compiler.Run
    run.compile(command.files)

    compiler.daisyExtraction.compiledProgram match {
        case Some(pgm) => (ctx, pgm)
        case None => ctx.reporter.fatalError("Failed to extract Daisy program.")
      }
    } else {
      ctx.reporter.fatalError("No input program.")
    }
  }
}