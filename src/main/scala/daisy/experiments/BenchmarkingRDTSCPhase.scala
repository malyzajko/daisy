package daisy
package experiment

import lang.Trees._
import lang.Types._
import tools.FinitePrecision._
import tools.Interval
import java.io._

object BenchmarkingRDTSCPhase extends DaisyPhase {

  override val name = "BenchmarkingRDTSC"
  override val shortName = "benchmarkingRDTSC"
  override val description = "generates the benchmark file with RDTSC counter"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("num-runs", 100000, "number of benchmarking runs")
  )

  implicit val debugSection = DebugSectionOptimisation

  var reporter: Reporter = null
  var fileName:  String = null
  var fileNameBenchmark:  String = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val bound = ctx.option[Long]("num-runs").toInt
    reporter  = ctx.reporter

    fileNameBenchmark = System.getProperty("user.dir")+"/output/" + prg.id + "_benchmark.c"
    val benchmark = new PrintWriter(new File(fileNameBenchmark))


    benchmark.write("#include <stdio.h>\n#include <stdint.h>\n#include <limits.h>\n#include <fstream>\n#include <math.h>\n#include <stdlib.h>\n#include <chrono>\n#include <sys/time.h>\n")
    benchmark.write("using namespace std;\nusing namespace std::chrono;\n\n")

    benchmark.write("double fRand(double fMin, double fMax) {\n" +
      "\tdouble f = (double)rand() / RAND_MAX;\n" +
      "\treturn fMin + f * (fMax - fMin);\n}\n\n")
    benchmark.write("#define READ_TIME_COUNTER(time) \\\n"+
  "__asm__ __volatile__(                               \\\n"+
          "\"xorl %%eax,%%eax\\n\\t\"                  \\\n"+
          "\"cpuid\\n\\t \"                            \\\n"+
          "\"rdtsc\\n\\t \"                            \\\n"+
          "\"movl %%eax,(%0)\\n\\t\"                   \\\n"+
          "\"movl %%edx,4(%0)\\n\\t\"                   \\\n" +
          "\"xorl %%eax,%%eax\\n\\t\"                   \\\n" +
          "\"cpuid\\n\\t\"                            \\\n" +
          ": /* nothing */                            \\\n" +
          ": \"S\"((time))                            \\\n" +
          ": \"eax\", \"ebx\", \"ecx\", \"edx\", \"memory\") \n\n")


    val prototypes = functionsToConsider(ctx, prg).map( fnc => {
      val params = fnc.params.map(param => typeToString(param.id.getType)).mkString(", ")
      s"${typeToString(fnc.returnType)} ${fnc.id}($params);\n"
    })

    benchmark.write(prototypes.mkString("\n"))

    benchmark.write(s"\nint main(int argc, char *args[]){\n\n\tlong bound = ${bound};\n")

    for (fnc <- functionsToConsider(ctx, prg)) {
      benchmark.write("\t{\n")
      benchmark.write("std::ofstream output(args[1]);") //unsafe! need to check for the argc
      benchmark.write("\t\tuint64_t before, after, time;\n")
      

     val ranges     =  ctx.specInputRanges(fnc.id)
      val errors  =  ctx.specInputErrors(fnc.id)

      val params = fnc.params.map(p => {
        val actualRange = ranges(p.id) +/- errors(p.id)
        s"fRand(${actualRange.xlo}, ${actualRange.xhi})"
      })

      benchmark.write("\t\tdouble res = 0.0;\n\t\tlong i = 0;\n\t\twhile(i < bound) {\n")
      benchmark.write(s"\t\t\t\tres += ${fnc.id}(${params.mkString(",")});\n")
      benchmark.write("\t\t\ti++;\n")
      benchmark.write("\t\t}\n")

      benchmark.write("\t\tres = 0.0;\n\t\ti = 0;\n\t\twhile(i < bound) {\n")


      benchmark.write(s"\t\t\tdo{\n")

      benchmark.write(s"\t\t\t\tREAD_TIME_COUNTER(&before);\n")
      benchmark.write(s"\t\t\t\tres += ${fnc.id}(${params.mkString(",")});\n")
      benchmark.write(s"\t\t\t\tREAD_TIME_COUNTER(&after);\n")

      benchmark.write(s"\t\t\t} while (before >= after);\n")
      benchmark.write("\t\t\ttime = after - before;\n")
      
      benchmark.write("\t\t\toutput << time << \",\";")
      
      benchmark.write("\t\t\ti++;\n")
      benchmark.write("\t\t}\n")

      benchmark.write("\t\toutput.close();")
      val kind = if (ctx.hasFlag("metalibm")) "metalibm"
        else "math.h"
      benchmark.write("\t\tprintf(\"" + kind + ", " + fnc.id + " finished benchmarking\\n\");\n")

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
