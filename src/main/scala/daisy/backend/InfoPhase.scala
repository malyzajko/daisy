// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import java.io.FileWriter
import java.io.BufferedWriter
import java.text.SimpleDateFormat
import java.util.Date

import lang.Trees.{Program, Let, Expr}
import tools.Rational
import Rational._

/**
  ??? Description goes here


  Prerequisites:
    -
 */
object InfoPhase extends DaisyPhase {

  override val name = "info phase"
  override val description = "prints interesting information"
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ParamOptionDef("info-log", "which file to write analysis results to, " +
      "default prints nothing, output file is created in rawdata/", "")
    )

  // implicit val debugSection = ???

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"----- Results: -----")
    val timer = ctx.timers.info.start

    var out: BufferedWriter = null

    /* Process relevant options */
    for (opt <- ctx.options) opt match {
      case ParamOption("info-log", file) if file != "" =>
        out = new BufferedWriter(new FileWriter(s"log/$file"))
      case _ => ;
    }

    for (fnc <- prg.defs) if (!fnc.precondition.isEmpty && !fnc.body.isEmpty){

      reporter.info(fnc.id)

      val absError = ctx.resultAbsoluteErrors.get(fnc.id)
      val range = ctx.resultRealRanges.get(fnc.id)

      val absErrorString = absError match {
        case Some(x) => x.toString
        case None => "-"
      }
      val rangeString = range match {
        case Some(x) => x.toString
        case None => "-"
      }

      // if a relative error was already computed print it, otherwise compute here
      val relErrorString: String = ctx.resultRelativeErrors.get(fnc.id) match {
        case Some(Some(x)) =>
          x.toString
        case Some(None) =>
          "n/a"
        case None =>
          (absError, range) match {
            case (Some(e), Some(r)) =>
              if (r.xlo <= zero && zero <= r.xhi) {
                "n/a"
              } else {
                max(abs(e / r.xlo), abs(e / r.xhi)).toString
              }

            case _ => "-"
          }
      }

      val infoString: String =
        s"abs-error: ${absErrorString}, real range: ${rangeString},\nrel-error: ${relErrorString}"
      reporter.info(infoString)

      if (out != null) {
        out.write(fnc.id.toString + " ")
        out.write(infoString.replace("\n", " ") + "\n")
      }
    }


    if (solvers.Solver.unknownCounter != 0) {
      reporter.warning(s"Solver returned unknown or timed out ${solvers.Solver.unknownCounter} times.")

    }

    if (out != null){ out.close }
    timer.stop
    ctx.reporter.info(s"--------------------")
    (ctx, prg)
  }

  private def getLastExpression(e: Expr): Expr = e match {
    case Let(_, _, body) => getLastExpression(body)
    case _ => e
  }

}