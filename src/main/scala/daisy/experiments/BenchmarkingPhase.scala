package daisy
package experiment

import lang.Trees._
import lang.Types._
import tools.FinitePrecision._
import tools.Interval
import java.io._

object BenchmarkingPhase extends DaisyPhase {

  override val name = "Benchmarking"
  override val description = "generates the benchmark file"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("bound", 100000, "number of benchmarking runs"),
    FlagOption("RDTSC", "generates the benchmark file with RDTSC counter")
  )
  override implicit val debugSection = DebugSectionExperiment
  var reporter: Reporter = null
  var fileName: String = null
  var fileNameBenchmark: String = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val bound = ctx.option[Long]("bound").toInt
    val rdtsc_flag = ctx.hasFlag("RDTSC")
    reporter  = ctx.reporter

    fileNameBenchmark = System.getProperty("user.dir") +"/output/" + prg.id + "_benchmark.c"
    val benchmark = new PrintWriter(new File(fileNameBenchmark))
    benchmark.write(
      """#include <stdio.h>
        |#include <stdlib.h>
        |#include <chrono>
        |""".stripMargin)

    if (rdtsc_flag){
      benchmark.write(
        s"""#include <stdint.h>
          |#include <limits.h>
          |#include <fstream>
          |#include <math.h>
          |#include <sys/time.h>
          |using namespace std;
          |using namespace std::chrono;
          |
          |""".stripMargin)

      writefRandFunction(benchmark)

      defineReadTimeCounter(benchmark)

      writePrototypes(benchmark,ctx,prg)

      writeMainFunctionForRDTSC(benchmark,ctx,prg,bound)
    }
    else {
      benchmark.write(
        s"""|using namespace std;
           |using namespace std::chrono;
           |""".stripMargin)

      writefRandFunction(benchmark)

      writePrototypes(benchmark,ctx,prg)

      writeMainFunction(benchmark,ctx,prg,bound)
    }

    benchmark.write(
      s"""}
         |""".stripMargin)
    benchmark.close
    (ctx, prg)
  }

  def getRandomRange(rangeID: Interval): String =  s"nextInput(${rangeID.xlo}, ${rangeID.xhi}, i)"


  def typeToString(arg: TypeTree): String = arg match {
    case FinitePrecisionType(Float32) => "float"
    case FinitePrecisionType(Float64) => "double"
  }

  def writefRandFunction(benchmark: PrintWriter): Unit ={
    benchmark.write(
      s"""
         |double fRand(double fMin, double fMax) {
         |    double f = (double)rand() / RAND_MAX;
         |    return fMin + f * (fMax - fMin);
         |}
         |""".stripMargin)
  }
  def defineReadTimeCounter(benchmark: PrintWriter): Unit = {
    benchmark.write(
      s"""
        |#define READ_TIME_COUNTER(time) \\
        |__asm__ __volatile__(            \\
        |"xorl %%eax,%%eax\\n\\t\"        \\
        |"cpuid\\n\\t "                   \\
        |"rdtsc\\n\\t "                   \\
        |"movl %%eax,(%0)\\n\\t"          \\
        |"movl %%edx,4(%0)\\n\\t"         \\
        |"xorl %%eax,%%eax\\n\\t"         \\
        |"cpuid\\n\\t"                    \\
        |: /* nothing */                  \\
        |: "S"((time))                    \\
        |: "eax", "ebx", "ecx", "edx", "memory")
        |
        |
        |""".stripMargin)
  }

  def writePrototypes(benchmark: PrintWriter,ctx: Context,prg: Program): Unit = {
    val prototypes = functionsToConsider(ctx, prg).map(fnc => {
      val params = fnc.params.map(param => typeToString(param.id.getType)).mkString(", ")
      s"${typeToString(fnc.returnType)} ${fnc.id}($params);\n"
    })

    benchmark.write(prototypes.mkString("\n"))
  }
  def writeMainFunctionForRDTSC(benchmark: PrintWriter,ctx: Context,prg: Program,bound: Long): Unit = {
    benchmark.write(
      s"""
         |int main(int argc, char *args[]){
         |    long bound = ${bound};
         |""".stripMargin
    )

    for (fnc <- functionsToConsider(ctx, prg)) {
      benchmark.write(
        s"""    {
           |        std::ofstream output(args[1]);
           |        uint64_t before, after, time;
           |""".stripMargin
      )//unsafe! need to check for the argc

      val ranges     =  ctx.specInputRanges(fnc.id)
      val errors  =  ctx.specInputErrors(fnc.id)

      val params = fnc.params.map(p => {
        val actualRange = ranges(p.id) +/- errors(p.id)
        s"fRand(${actualRange.xlo}, ${actualRange.xhi})"
      })

      val kind = if (ctx.hasFlag("metalibm")) "metalibm" else "math.h"

      benchmark.write(
        s"""        double res = 0.0;
           |        long i = 0;
           |        while(i < bound) {
           |            res += ${fnc.id}(${params.mkString(",")});
           |            i++;
           |        }
           |
           |        res = 0.0;
           |        i = 0;
           |
           |        while (i < bound) {
           |            do {
           |                READ_TIME_COUNTER(&before);
           |                res += ${fnc.id}(${params.mkString(",")});
           |                READ_TIME_COUNTER(&after);
           |            } while (before >= after);
           |
           |            time = after - before;
           |            output << time << ",";
           |            i++;
           |        }
           |
           |        output.close();
           |
           |        printf("$kind, ${fnc.id}  finished benchmarking\\n");
           |    }
           |""".stripMargin)
    }
  }

  def writeMainFunction(benchmark: PrintWriter,ctx: Context,prg: Program,bound: Long): Unit = {
    benchmark.write(
      s"""
         |int main(){
         |    srand (time(NULL));
         |    long bound = ${bound};
         |""".stripMargin
    )

    for (fnc <- functionsToConsider(ctx, prg)) {
      benchmark.write(
        s"""    {
           |        high_resolution_clock::time_point _t1 = high_resolution_clock::now();
           |        double res = 0.0;
           |        long i = 0;
           |        while(i < bound) {
           |""".stripMargin
      )

      val ranges = ctx.specInputRanges(fnc.id)
      val errors = ctx.specInputErrors(fnc.id)

      val params = fnc.params.map(p => {
        val actualRange = ranges(p.id) +/- errors(p.id)
        s"fRand(${actualRange.xlo}, ${actualRange.xhi})"
      })
      val kind = if (ctx.hasFlag("metalibm")) "metalibm" else "math.h"

      benchmark.write(
        s"""
           |            res += ${fnc.id}(${params.mkString(",")});
           |            i++;
           |        }
           |        high_resolution_clock::time_point _t2 = high_resolution_clock::now();
           |        duration<double> time_span = duration_cast<duration<double> >(_t2 - _t1);
           |        printf("$kind , ${fnc.id }");
           |        printf(", %f\\n", time_span.count());
           |    }
           |""".stripMargin
      )
    }
  }

}
