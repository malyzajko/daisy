// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.Seq

import lang.Trees.{Program, Variable, Expr}
import tools.{Interval, Rational}
import Rational._

/**
  ??? Description goes here


  Prerequisites:
    -
 */
object InfoPhase extends DaisyPhase {
  override val name = "Info"
  override val shortName = "info"
  override val description = "Prints interesting information"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringOption(
      "results-csv",
      "Which file to write analysis results to. Output file is created in output/")
  )

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val out = ctx.option[Option[String]]("results-csv")
      .map(new File("output", _))
      .map{ f =>
        val append = f.exists
        val o = new BufferedWriter(new FileWriter(f, append))
        if (!append) {
          if(ctx.hasFlag("dynamic") || ctx.hasFlag("bgrtdynamic")){
            o.write("Function name, Absolute error, Relative error, Real range low, Real range high, Seed, numSamples, \n")
          } else {
            o.write("Function name, Absolute error, Relative error, Real range low, Real range high, \n")
          }
        }
        o
      }

    for (fnc <- functionsToConsider(ctx, prg)){

      ctx.reporter.result(fnc.id)
      fnc.returnType match {
        case lang.Types.TupleType(args) =>
          val errors = ctx.intermediateAbsErrors(fnc.id)
          ctx.reporter.result("Absolute errors:")
          for(resId <- ctx.resultTupleIds(fnc.id)) {
            ctx.reporter.result(s"${resId}: ${errors((Variable(resId), Seq[Expr]()))}")
          }

        case _ =>
          val absError = ctx.resultAbsoluteErrors.get(fnc.id)
          val range = ctx.resultRealRanges.get(fnc.id)
          val relError = ctx.resultRelativeErrors.getOrElse(fnc.id, (absError, range) match {
            case (Some(e), Some(r)) if !r.includes(zero) => Some(e / Interval.minAbs(r))
            case _ => None
          })

          (absError, ctx.specResultErrorBounds.get(fnc.id)) match {
            case (Some(x), Some(spec)) if x > spec =>
              ctx.reporter.warning(s"  Absolute error: $x. Error bound is not satisfied!")
            case (Some(x), _) =>
              ctx.reporter.result(s"  Absolute error: $x")
            case _ =>
          }

          range.foreach(r => ctx.reporter.result(s"  Real range:     $r"))

          relError.foreach(re => ctx.reporter.result(s"  Relative error: $re"))
          
          val numSamples = ctx.resultNumberSamples.get(fnc.id)

          if (out.isDefined) {
            if(ctx.hasFlag("dynamic") || ctx.hasFlag("bgrtdynamic")){
              out.get.write(
                fnc.id + ","+
                absError.map(_.toString).getOrElse("") + "," +
                relError.map(_.toString).getOrElse("") + "," +
                range.map(_.xlo.toString).getOrElse("") + "," +
                range.map(_.xhi.toString).getOrElse("") + "," +
                ctx.seed.toString + "," + 
                numSamples.map(_.toString).getOrElse("") + "\n"
              )
            } else {
              out.get.write(
                //fnc.id + ","+
                absError.map(_.toString).getOrElse("") //+ "," +
                //relError.map(_.toString).getOrElse("") + "," //+
                //range.map(_.xlo.toString).getOrElse("") + "," +
                //range.map(_.xhi.toString).getOrElse("") + "\n"
              )
            }
          }
      }
    }

    if (solvers.Solver.unknownCounter != 0) {
      ctx.reporter.warning(s"Solver returned unknown or timed out ${solvers.Solver.unknownCounter} times.")
    }

    if (out.isDefined) { out.get.close() }
    (ctx, prg)
  }
}
