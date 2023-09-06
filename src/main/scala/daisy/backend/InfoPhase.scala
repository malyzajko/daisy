// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.{ListMap, Seq}
import lang.Trees.{Expr, FunDef, Program, Variable}
import tools.{Interval, MatrixIndex, Rational, VectorIndex}
import Rational._
import daisy.Main.ProgramLanguage
import daisy.lang.Identifiers.FreshIdentifier
import daisy.lang.TreeOps
import daisy.lang.Types.{MatrixType, RealType, VectorType}

/**
  ??? Description goes here


  Prerequisites:
    -
 */
object InfoPhase extends DaisyPhase with opt.CostFunctions {
  override val name = "Info"
  override val description = "Prints interesting information"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringOption(
      "results-csv",
      "Which file to write analysis results to. Output file is created in output/"),
    FlagOption("ds-pre-c", "Print input ranges for DS literals as a C-like for loop."),
    FlagOption("ds-pre-scala", "Print input ranges for DS literals as a C-like for loop.")
  )
  override implicit val debugSection = DebugSectionBackend

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    if (ctx.hasFlag("ds-pre-c")) {
      for (fnc <- functionsToConsider(ctx, prg)) {
        // print preconditions and exit
        printDSPreconditions(fnc, ctx, ProgramLanguage.CProgram)
      }
      return (ctx, prg)
    }
    if (ctx.hasFlag("ds-pre-scala")) {
      for (fnc <- functionsToConsider(ctx, prg)) {
        // print preconditions and exit
        printDSPreconditions(fnc, ctx, ProgramLanguage.ScalaProgram)
      }
      return (ctx, prg)
    }
    if (ctx.hasFlag("print-ast")) {
      for (fnc <- functionsToConsider(ctx, prg)) {
        // call pretty printer
        print(fnc)
      }
      return (ctx, prg)
    }
    val out = ctx.option[Option[String]]("results-csv")
      .map(new File("output", _))
      .map{ f =>
        val append = f.exists
        val o = new BufferedWriter(new FileWriter(f, append))
        if (!append) {
          if(ctx.hasFlag("dynamic")){
            o.write("Function name, Absolute error, Relative error, Real range low, Real range high, Seed, numSamples, \n")
          } else if(ctx.hasFlag("ds")|| ctx.hasFlag("ds-naive")) {
            //ctx.timers.
            o.write("Function name, Absolute error, Real range low, Real range high, Time DSPhase, Time Scala Extraction, Time Spec Preprocessing, Time Total, Error Message\n")
          } else {
            o.write("Function name, Absolute error, Relative error, Real range low, Real range high, \n")
          }
        }
        o
      }

    val funs = functionsToConsider(ctx, prg)// for ApproxPhase functions to consider only contain real valued functions
    for (fnc <- funs){

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

          if (ctx.hasFlag("approx") && !TreeOps.containsApproxNode(fnc.body.get)) {
            val numOps = countOps(fnc.body.get)
            ctx.reporter.result(s"  Number of arithmetic operations in generated code: $numOps")
          }
          val numSamples = ctx.resultNumberSamples.get(fnc.id)

          if (out.isDefined) {
            if(ctx.hasFlag("dynamic")){
              out.get.write(
                fnc.id.toString + ","+
                absError.map(_.toString).getOrElse("") + "," +
                relError.map(_.toString).getOrElse("") + "," +
                range.map(_.xlo.toString).getOrElse("") + "," +
                range.map(_.xhi.toString).getOrElse("") + "," +
                ctx.seed.toString + "," +
                numSamples.map(_.toString).getOrElse("") + "\n"
              )
            } else if(ctx.hasFlag("ds")|| ctx.hasFlag("ds-naive")) {
              val dsPhaseTime = if (ctx.hasFlag("unroll"))
                ctx.timers.get("Dataflow error").end - ctx.timers.get("Dataflow error").beginning
              else
                ctx.timers.get("DSPhase-"+ fnc.id.toString).end - ctx.timers.get("DSPhase-"+ fnc.id.toString).beginning
              //"Function name, Absolute error, Real range low, Real range high, Time DSPhase, Time Spec Preprocessing, Time Total \n"
              out.get.write(
                fnc.id.toString + ","+
                  absError.map(_.toString).getOrElse("") + "," +
                  range.map(_.xlo.toString).getOrElse("") + "," +
                  range.map(_.xhi.toString).getOrElse("") + "," +
                  dsPhaseTime.toString + "," +
                  (ctx.timers.get("Scala extraction").end - ctx.timers.get("Scala extraction").beginning).toString + "," +
                  (ctx.timers.get("Specs processing").end - ctx.timers.get("Specs processing").beginning).toString + "," +
                  (System.currentTimeMillis - ctx.timers.get("total").beginning).toString + "," + // total time (the timer is still running)
                  ctx.errMsg.getOrElse("-") + "\n"
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

  def printDSPreconditions(fnc: FunDef, ctx: Context, lang: ProgramLanguage.Value): Unit = {
    val dsInputRanges = ctx.dsAbstractions(fnc.id)
    if (dsInputRanges.isEmpty) {
      ctx.reporter.warning(f"No input ranges for DS literals in ${fnc.id}. Skipping it.")
      return;
    }
    //val inputs = dsInputRanges.view.filterKeys().toMap // todo check that it's only inputs
    //ctx.reporter.warning(f"Printing ranges for ${fnc.id}. ${ctx.file}")
    if (lang == ProgramLanguage.CProgram) {
      dsInputRanges.foreach({ case (ex, dsa) =>
        val sortedDSAs = ListMap(dsa.indexToRange.toSeq.sortBy(_._1.min): _*)
        sortedDSAs.foreach({ case (inds, range) =>
          val sorted = inds.toSeq.sorted
          //val fromI = sorted.min
          //val toI = sorted.max
          ex.getType match {
            case VectorType(_) =>
              sorted.foreach(xx => {
                val mi = xx.asInstanceOf[VectorIndex]
                print(f"$ex[${mi.i}] = DBETWEEN(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\\n")
              })
            //if (sorted.size == 1) {
            //  print(f"$ex[${fromI.i}] = DBETWEEN(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\\n")
            //} else {
            //  val c = f"for(i=${fromI.i}; i<=${toI.i}; i++) {\\n\     $ex[i] = DBETWEEN(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()}); \\n\ }\\n" // for(i=0;i<N;i++)
            //  print(c)
            //}
            case MatrixType(_) =>
              sorted.foreach(xx => {
                val mi = xx.asInstanceOf[MatrixIndex]
                print(f"$ex[${mi.i}][${mi.j}] = DBETWEEN(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\\n")
              })
            //if (sorted.size == 1) {
            //  val mi = fromI.asInstanceOf[MatrixIndex]
            //  print(f"$ex[${mi.i}][${mi.j}] = DBETWEEN(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\\n")
            //} else {
            //  val mFrom = fromI.asInstanceOf[MatrixIndex]
            //  val mTo = toI.asInstanceOf[MatrixIndex]
            //  val c = f"for(i=${mFrom.i}; i<=${mTo.i}; i++) \\n\  " +
            //    f"for(j=${mFrom.j}; j<=${mTo.j}; j++) " +
            //    f"{\\n\ \     $ex[i][j] = DBETWEEN(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()}); \\n\ }\\n" // for(i=0;i<N;i++)
            //  print(c)
            //}
          }
        })
      })
    } else {
      // Scala
      val filename = System.getProperty("user.dir") + "/output/" + fnc.id + ".scala"
      val fstream = new FileWriter(filename)
      val out = new BufferedWriter(fstream)
      out.append(
        """
          |def randomValue(lo:Double, up:Double): Double = {
          |    import util.Random._
          |    if (lo == up) lo else if (lo < up) between(lo, up) else throw new Exception(f"interval has reversed bounds [$lo, $up]")
          |  }
          |
          |def readCSVVector(filename:String) : Array[(Double,Double)] = {
          |  io.Source.fromFile(filename)
          |    .getLines()
          |    .map(str => {val t = str.split(","); (t(0).toDouble, t(1).toDouble)})
          |    .toArray
          |}
          |
          |def readCSVMatrix(filename:String) : Array[Array[(Double,Double)]] = {
          |  io.Source.fromFile(filename)
          |    .getLines()
          |    .map(_.split(",").map(_.toDouble).sliding(2,2).map(x=> (x.head, x(1))).toArray)
          |    .toArray
          |}
          |  """.stripMargin)
      out.append(
        """
 def main(args: Array[String]): Unit = {
   val iter:Int = args.head.toInt
   val filename_in: String = args(1)
   val filename_out: String = args(2)

   """)
      dsInputRanges.foreach({ case (ex, dsa) =>
        // write the bounds into a file
        val datafile = System.getProperty("user.dir") + "/output/" + fnc.id + f"_${ex}.csv"
        val fstream_data = new FileWriter(datafile)
        val out_data = new BufferedWriter(fstream_data)
        ex.getType match {
          case VectorType(_) =>

            // get all ranges for row i, column j
            val rows = dsa.dsSize
            for {i <- 0 until rows} {
              val range =  dsa.at(i)
              out_data.append(f"${range.xlo.toString},${range.xhi.toString}\n")
            }

            // read the file with the bounds
            out.append(f"""\n val ranges_${ex} = readCSVVector(filename_in + "_${ex}.csv")\n""")
          case MatrixType(_) =>
            out.append(f"""\n val ranges_${ex} = readCSVMatrix(filename_in + "_${ex}.csv")\n""")
            // get all ranges for row i, column j
            val rows = dsa.numRows
            val cols = dsa.numCols
            for {i <- 0 until rows} {
              val ind_dsas = dsa.getAbstractionOnlyAtIndices(dsa.indicesAtRow(i))
              //out.append(f"\nList(")
              for {j <- 0 until cols} {
                val range = ind_dsas.at(i,j)
                out_data.append(f"${range.xlo.toString},${range.xhi.toString}")
                if (j<cols-1)
                  out_data.append(", ")
                else
                  out_data.append("\n")
            }
            }
            //out.append(")\n")
        }

        out_data.close()
      })
    out.append("""
   val errs = (0 until iter).map(i => {
""".stripMargin)
      val reals = fnc.params.filter(x => x.getType == RealType)
      reals.foreach(vd => {
        val range = ctx.specInputRanges(fnc.id)(vd.id)
        out.append(f"\n val ${vd.id.name} = randomValue(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\n")
        out.append(f"\n val mpfr_${vd.id.name} = MPFRFloat.fromDouble(${vd.id.name});\n")
      })
      dsInputRanges.foreach({ case (ex, dsa) =>
        //val sortedDSAs = ListMap(dsa.indexToRange.toSeq.sortBy(_._1.min): _*)
        ex.getType match {
          case RealType =>
            val range = dsa.fullInterval
            out.append(f"\n tmp_$ex = randomValue(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\n")
          case VectorType(_) =>
            out.append(f"\n val tmp_${ex} = Array.ofDim[Double](${dsa.dsSize})\n")
            //sortedDSAs.foreach({ case (inds, range) =>
            //  val sorted = inds.toSeq.sorted
            //  // for large benchmarks individual assignments do not work, not enough memory
            //  val fromI = sorted.min
            //  val toI = sorted.max
            //  out.append(f" for {i <- ${fromI.i} to ${toI.i} } \n  tmp_$ex(i) = randomValue(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\n")
            //  //sorted.foreach(xx => {
            //  //  val mi = xx.asInstanceOf[VectorIndex]
            //  //  out.append(f" tmp_$ex(${mi.i}) = randomValue(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\n")
            //  //})
            //})
            out.append(f" for {i <- 0 until ${dsa.dsSize} } \n { tmp_$ex(i) = randomValue(ranges_${ex}(i)._1, ranges_${ex}(i)._2) }\n")
            out.append(f"\n val ${ex} = tmp_${ex}.toList")
            out.append(f"\n val mpfr_${ex} = ${ex}.map(_x_ => MPFRFloat.fromDouble(_x_))")

          case MatrixType(_) =>
            out.append(f"\n val tmp_${ex} = Array.ofDim[Double](${dsa.numRows},${dsa.numCols})\n")
            //sortedDSAs.foreach({ case (inds, range) =>
            //  val sorted = inds.toSeq.sorted
            //  val fromI = sorted.min.asInstanceOf[MatrixIndex]
            //  val toI = sorted.max.asInstanceOf[MatrixIndex]
            //  out.append(f"\nList(")
            //  //out.append(f" for {i <- ${fromI.i} to ${toI.i}; j <- ${fromI.j} to ${toI.j} } \n  tmp_$ex(i)(j) = randomValue(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\n")
            //  //sorted.foreach(xx => {
            //  //  val mi = xx.asInstanceOf[MatrixIndex]
            //  //  out.append(f" tmp_$ex(${mi.i})(${mi.j}) = randomValue(${range.xlo.doubleValue()}, ${range.xhi.doubleValue()});\n")
            //  //})
            //})
            out.append(f" for {i <- 0 until ${dsa.numRows}; j <- 0 until ${dsa.numCols} } \n { tmp_$ex(i)(j) = randomValue(ranges_${ex}(i)(j)._1, ranges_${ex}(i)(j)._2) }\n")
            out.append(f"\n val ${ex} = tmp_${ex}.map(_.toList).toList")
            out.append(f"\n val mpfr_${ex} = ${ex}.map(row => row.map(_x_ => MPFRFloat.fromDouble(_x_)))")
        }
      })
      val resVar = Variable(FreshIdentifier("res", fnc.returnType))
      out.append(s"\n val ${resVar.id.uniqueName} = ${fnc.id.name}")
      val uniqueArgs = fnc.params.map(a => a.id.name)
      printNary(uniqueArgs, out)
      out.append(s"\n val mpfr_${resVar.id.uniqueName} = mpfr_${fnc.id.name}")
      val mpfrArgs = fnc.params.map(a => f"mpfr_${a.id.name}")
      printNary(mpfrArgs, out)
      out.append("\n // compute errors ")
      fnc.returnType match {
        case RealType => out.append(s"\nval err:MPFRFloat = MPFRFloat.abs(MPFRFloat.fromDouble(${resVar.id.uniqueName}) - mpfr_${resVar.id.uniqueName})")
        case VectorType(_) =>
          out.append(
            f"""
               |\nval err:MPFRFloat = ${resVar.id.uniqueName}.zip(mpfr_${resVar.id.uniqueName}).map({case (del,qel) =>
               |                  val tmp = MPFRFloat.fromDouble(del)
               |                  MPFRFloat.abs(tmp - qel)
               |                }).max
               |                """.stripMargin)
        case MatrixType(_) =>
          out.append(
            f"""
               |\nval err:MPFRFloat = ${resVar.id.uniqueName}.zip(mpfr_${resVar.id.uniqueName}).map({case (drow,qrow) =>
               |               val tmpLst = drow.zip(qrow).map({case (del,qel) =>
               |                  val tmp = MPFRFloat.fromDouble(del)
               |                  MPFRFloat.abs(tmp - qel)
               |                })
               |               tmpLst.max
               |             }).max""".stripMargin)
      }
      out.append(
        f"""
          |err
          |})
          |
          | val fstream = new FileWriter(filename_out, true)
          | val out = new BufferedWriter(fstream)
          | out.append("${fnc.id},"+errs.max.toLongString.replace(',','.')+"\\n")
          | out.close()
          |}
          |""".stripMargin)
      out.close()
    }
  }



  def printNary(uniqueArgs: Seq[Any], out: BufferedWriter): Unit = {
    val sz = uniqueArgs.size
    var c = 0
    out.append("(")
    uniqueArgs.foreach(ex => {
      out.append(s"$ex")
      c += 1;
      if (c < sz) {
        out.append(",")
      }
    })
    out.append(")")
  }
}

