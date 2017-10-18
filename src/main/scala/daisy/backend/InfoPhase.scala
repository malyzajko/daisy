// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import java.io.FileWriter
import java.io.BufferedWriter
import java.text.SimpleDateFormat
import java.util.Date

import lang.Trees.{Program, Let, Expr}
import tools.{Interval, Rational}
import Rational._

/**
  ??? Description goes here


  Prerequisites:
    -
 */
object InfoPhase extends PhaseComponent {
  override val name = "Info"
  override val description = "Prints interesting information"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringOption(
      "info-log",
      "Which file to write analysis results to. Default prints nothing, output file is created in rawdata/")
  )
  override def apply(cfg: Config) = new InfoPhase(cfg, name, "info")
}

class InfoPhase(val cfg: Config, val name: String, val shortName: String) extends DaisyPhase {

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    startRun()

    val out = cfg.option[Option[String]]("info-log")
      .map(f => new BufferedWriter(new FileWriter("log/"+f)))
      .orNull

    for (fnc <- functionsToConsider(prg)){

      cfg.reporter.result(fnc.id)

      val absError = ctx.resultAbsoluteErrors.get(fnc.id)
      val range = ctx.resultRealRanges.get(fnc.id)

      val absErrorString = absError match {
        case Some(x) =>
          ctx.specResultErrorBounds.get(fnc.id) match {
            case Some(specError) =>
              if (x > specError) {
                cfg.reporter.warning("Error bound is not satisfied!")
              }
            case _ =>
          }
          x.toString
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
              if (r.includes(zero)) {
                "n/a"
              } else {
                (e / Interval.minAbs(r)).toString
              }

            case _ => "-"
          }
      }

      val infoString: String =
        s"abs-error: ${absErrorString}, real range: ${rangeString},\nrel-error: ${relErrorString}"
      cfg.reporter.result(infoString)

      if (out != null) {
        out.write(fnc.id.toString + " ")
        out.write(infoString.replace("\n", " ") + "\n")
      }
    }


    if (solvers.Solver.unknownCounter != 0) {
      cfg.reporter.warning(s"Solver returned unknown or timed out ${solvers.Solver.unknownCounter} times.")

    }

    if (out != null){ out.close }
    finishRun(ctx, prg)
  }
}
