

package daisy

import java.io.FileWriter
import java.io.BufferedWriter
import java.text.SimpleDateFormat
import java.util.Date

import lang.Trees.{Program, Let, Expr}
import lang.NumAnnotation
import utils.Rational
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
    ParamOptionDef("info-log", "which file to write analysis results to, "+
      "default prints nothing, output file is created in rawdata/", "")
    )

  //implicit val debugSection = ???

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.info.start

    var out: BufferedWriter = null

    /* Process relevant options */
    for (opt <- ctx.options) opt match {
      case ParamOption("info-log", file) if file != "" =>
        out = new BufferedWriter(new FileWriter(s"rawdata/$file"))
      case _ => ;
    }

    for (fnc <- prg.defs) if (!fnc.precondition.isEmpty && !fnc.body.isEmpty){

      reporter.info(fnc.id)

      val absError = ctx.resultAbsoluteErrors(fnc.id)
      val range = ctx.resultRealRanges(fnc.id)
      val relError: Double = if (range.xlo <= zero && zero <= range.xhi) {
          Double.NaN
        } else {
          max(abs(absError / range.xlo), abs(absError / range.xhi)).toDouble
        }

      val infoString: String =
        s"abs-error: ${absError}, real range: ${range},\nrel-error: ${relError}"
      reporter.info(infoString)

      if (out != null) {
        out.write(fnc.id.toString + " ")
        out.write(infoString.replace("\n", " ") + "\n")
      }
    }

    if (solvers.Z3Solver.unknownCounter != 0) {
      reporter.warning(s"Z3 returned unknown or timed out ${solvers.Z3Solver.unknownCounter} times.")
    }

    if (out != null){ out.close }
    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, prg)
  }

  private def getLastExpression(e: Expr): Expr = e match {
    case Let(_, _, body) => getLastExpression(body)
    case _ => e
  }

}