package daisy
package experiment

import lang.Trees._
import lang.Types._
import tools.FinitePrecision._
import tools.Interval
import java.io._

object BenchmarkingPhase extends DaisyPhase {

  override val name = "Benchmarking"
  override val shortName = "benchmarking"
  override val description = "generates the benchmark file"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("bound", 1000, "bound of tests")
  )

  implicit val debugSection = DebugSectionOptimisation

  var reporter: Reporter = null
  var fileName:  String = null
  var fileNameBenchmark:  String = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val bound = ctx.option[Long]("bound").toInt
    reporter  = ctx.reporter

    fileNameBenchmark = System.getProperty("user.dir")+"/output/" + prg.id + "_benchmark.c"
    val benchmark = new PrintWriter(new File(fileNameBenchmark))


    benchmark.write("#include <stdio.h>\n#include <stdlib.h>\n#include <chrono>\n")
    benchmark.write("using namespace std;\nusing namespace std::chrono;\n\n")

    benchmark.write("double fRand(double fMin, double fMax) {\n" +
      "\tdouble f = (double)rand() / RAND_MAX;\n" +
      "\treturn fMin + f * (fMax - fMin);\n}\n\n")


    val prototypes = functionsToConsider(ctx, prg).map( fnc => {
      val params = fnc.params.map(param => typeToString(param.id.getType)).mkString(", ")
      s"${typeToString(fnc.returnType)} ${fnc.id}($params);\n"
    })

    benchmark.write(prototypes.mkString("\n"))

    benchmark.write("\nint main(){\n\n\tsrand (time(NULL));\n\tlong bound = 100000;\n")

    for (fnc <- functionsToConsider(ctx, prg)) {
      benchmark.write("\t{\n")
      benchmark.write("\t\thigh_resolution_clock::time_point _t1 = high_resolution_clock::now();\n")
      benchmark.write("\t\tdouble res = 0.0;\n\t\tlong i = 0;\n\t\twhile(i < bound) {\n")

      val ranges     =  ctx.specInputRanges(fnc.id)
      val errors  =  ctx.specInputErrors(fnc.id)
      
      val params = fnc.params.map(p => {
        val actualRange = ranges(p.id) +/- errors(p.id)
        s"fRand(${actualRange.xlo}, ${actualRange.xhi})"
      })
      benchmark.write(s"\t\t\tres += ${fnc.id}(${params.mkString(",")});\n")

      benchmark.write("\t\t\ti++;\n")
      benchmark.write("\t\t}\n")

      benchmark.write("\t\thigh_resolution_clock::time_point _t2 = high_resolution_clock::now();\n")
      benchmark.write("\t\tduration<double> time_span = duration_cast<duration<double> >(_t2 - _t1);\n")
      
      val kind = if (ctx.hasFlag("metalibm")) "metalibm"
        else "math.h"
      benchmark.write("\t\tprintf(\"" + kind + ", " + fnc.id + "\");\n")
      benchmark.write("\t\tprintf(\", %f\\n\", time_span.count());\n")

      benchmark.write("\t}\n")
    }

    benchmark.write("}\n")
    benchmark.close

    (ctx, prg)
  }

  def getRandomRange(rangeID: Interval): String =  s"nextInput(${rangeID.xlo}, ${rangeID.xhi}, i)"


  def typeToString(arg: TypeTree): String = arg match {
    case FinitePrecisionType(Float32) => "float"
    case FinitePrecisionType(Float64) => "double"
  }

}
