/* Copyright 2009-2016 EPFL, Lausanne */

package daisy
package frontend

import lang.Trees.Program
import utils._

import scala.tools.nsc.{Settings,CompilerCommand}
import java.io.File

object ExtractionPhase extends {

  val name = "Scalac Extraction"
  val description = "Extraction of trees from the Scala Compiler"

  implicit val debug = DebugSectionFrontend

  def apply(ctx: Context): Program = {
    val timer = ctx.timers.frontend.start

    val settings = new Settings

    // def getFiles(path: String): Option[Array[String]] =
    //   scala.util.Try(new File(path).listFiles().map{ _.getAbsolutePath }).toOption

    val scalaLib = Option(scala.Predef.getClass.getProtectionDomain.getCodeSource).map{
       _.getLocation.getPath
    }.getOrElse( ctx.reporter.fatalError(
      "No Scala library found."
    ))

    //.orElse( for {
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
    val compilerOpts = ctx.libFiles.toList ++ ctx.files.toList

    val command = new CompilerCommand(compilerOpts, settings) {
       override val cmdName = "daisy"
    }

    if(command.ok) {
      // Debugging code for classpath crap
      // new scala.tools.util.PathResolver(settings).Calculated.basis.foreach { cp =>
      //   cp.foreach( p =>
      //     println(" => "+p.toString)
      //   )
      // }

      val compiler = new ScalaCompiler(settings, ctx)
      val run = new compiler.Run
      run.compile(command.files)

      compiler.daisyExtraction.compiledProgram match {
        case Some(pgm) =>
          timer.stop
          pgm

        case None =>
          ctx.reporter.fatalError("Failed to extract Daisy program.")
      }
    } else {
       ctx.reporter.fatalError("No input program.")
    }
  }
}