


package daisy
package experiment

import lang.Trees.Program
import scala.collection.immutable.Seq
import java.io.FileWriter
import java.io.BufferedWriter
import lang.Identifiers._
import lang.Trees._
import tools.Interval


object CompilerOptimizationsExperimentGenerationPhase extends DaisyPhase {

  override val name = "compiler-optimizations experiment gen"
  override val shortName = "comp-opt-exp-gen"
  override val description = "Generates compiler-optimizations benchmark "
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    FlagOption("comp-opts-exp-gen", "generate C benchcode")
  )

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val fnc = prg.defs.head
    CompilerOptimizationsExperimentGenerationPhase.generateCbenchCode(
      prg,  ctx.specInputRanges(fnc.id))
    (ctx, prg)
  }

  def generateCbenchCode(prg: Program, inputRanges: Map[Identifier, Interval]): Unit = {

    val outTest = new BufferedWriter(new FileWriter("benchmarking/c/compilerOptimizations/" + prg.id + "_benchmark.cpp"))

    outTest.write(
      "#include <math.h> \n" +
      "#include <cstdlib> \n" +
      "#include <chrono> \n" +
      "#include <iostream> \n\n" +
      "namespace A{\n    #include \"functionsNoOpts.cpp\"\n}\n" +
      "namespace B{\n    #include \"functionsCseHorner.cpp\"\n}\n" +
      "namespace C{\n    #include \"functionsCseHornerNoFma.cpp\"\n}\n" +
      "namespace D{\n    #include \"functionsHornerCse.cpp\"\n}\n" +
      "namespace E{\n    #include \"functionsHornerCseNoFma.cpp\"\n}\n\n" +
      "using namespace std;\n" +
      "using namespace std::chrono;;\n\n\n")

    val inputParams: Seq[ValDef] = prg.defs.head.params

    // Generate randomized inputs in range
    inputParams.zipWithIndex.foreach {
      case (currParam, index) =>

        val currParamInterval = inputRanges(currParam.id)

        outTest.write("double input" + (index + 1) + "Min = " +
          currParamInterval.xlo.floatValue + ";\n")

        outTest.write("double input" + (index + 1) + "Max = " +
          currParamInterval.xhi.floatValue +  ";\n\n")

        outTest.write(
            "double random" + (index + 1) + "(){\n" +
            "     double x = (double)rand() / RAND_MAX; \n" +
            "     return input"+ (index + 1)+"Min + x*(input"+ (index + 1)+"Max - input"+ (index + 1) +"Min) ; \n}\n\n")
    }
    outTest.write(
      "int main(){ \n\n "+
      "  int bound = 1000000; \n " +
      "  double res = 0.0; \n" +
      "  srand (time(NULL));\n\n")


    prg.defs.foreach { currDef: FunDef =>
      val currFncName = currDef.id.toString
      outTest.write(

        "{\n    high_resolution_clock::time_point t1 = high_resolution_clock::now(); \n" +
          "    int i = 0; \n" +
          s"    while(i < bound) { \n     res += A::$currFncName(" )

      outTest.write(inputParams.zipWithIndex.map({
        case (valDef, index) =>
          "random" + (index + 1) + "()"
      }).mkString(", "))

      outTest.write(
        ");\n" +
          "      i++; \n" +
          "    }\n" +
          "    high_resolution_clock::time_point t2 = high_resolution_clock::now();\n" +
          "    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);\n" +
          "    cout << \"time "+currFncName+" : \" << time_span.count() << \"\\n\";\n" +
          "}\n")

    outTest.write("\n\n")
    // cse then horner
      outTest.write(

        "{\n    high_resolution_clock::time_point t1 = high_resolution_clock::now(); \n" +
          "    int i = 0; \n" +
          s"    while(i < bound) { \n     res += B::$currFncName(" )

      outTest.write(inputParams.zipWithIndex.map({
        case (valDef, index) =>
          "random" + (index + 1) + "()"
      }).mkString(", "))

      outTest.write(
        ");\n" +
          "      i++; \n" +
          "    }\n" +
          "    high_resolution_clock::time_point t2 = high_resolution_clock::now();\n" +
          "    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);\n" +
          "    cout << \"time "+currFncName+"CseHorner : \" << time_span.count() << \"\\n\";\n" +
          "}\n")

      outTest.write("\n\n")

    // cse then horner No fma
      outTest.write(

        "{\n    high_resolution_clock::time_point t1 = high_resolution_clock::now(); \n" +
          "    int i = 0; \n" +
          s"    while(i < bound) { \n     res += C::$currFncName(" )

      outTest.write(inputParams.zipWithIndex.map({
        case (valDef, index) =>
          "random" + (index + 1) + "()"
      }).mkString(", "))

      outTest.write(
        ");\n" +
          "      i++; \n" +
          "    }\n" +
          "    high_resolution_clock::time_point t2 = high_resolution_clock::now();\n" +
          "    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);\n" +
          "    cout << \"time "+currFncName+"CseHornerNoFma : \" << time_span.count() << \"\\n\";\n" +
          "}\n")

      outTest.write("\n\n")

    // horner the cse
      outTest.write(

        "{\n    high_resolution_clock::time_point t1 = high_resolution_clock::now(); \n" +
          "    int i = 0; \n" +
          s"    while(i < bound) { \n     res += D::$currFncName(" )

      outTest.write(inputParams.zipWithIndex.map({
        case (valDef, index) =>
          "random" + (index + 1) + "()"
      }).mkString(", "))

      outTest.write(
        ");\n" +
          "      i++; \n" +
          "    }\n" +
          "    high_resolution_clock::time_point t2 = high_resolution_clock::now();\n" +
          "    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);\n" +
          "    cout << \"time "+currFncName+"HornerCse : \" << time_span.count() << \"\\n\";\n" +
          "}\n")

      outTest.write("\n\n")

    // horner then cse No fma
      outTest.write(

        "{\n    high_resolution_clock::time_point t1 = high_resolution_clock::now(); \n" +
          "    int i = 0; \n" +
          s"    while(i < bound) { \n     res += E::$currFncName(" )

      outTest.write(inputParams.zipWithIndex.map({
        case (valDef, index) =>
          "random" + (index + 1) + "()"
      }).mkString(", "))

      outTest.write(
        ");\n" +
          "      i++; \n" +
          "    }\n" +
          "    high_resolution_clock::time_point t2 = high_resolution_clock::now();\n" +
          "    duration<double> time_span = duration_cast<duration<double>>(t2 - t1);\n" +
          "    cout << \"time "+currFncName+"HornerCseNoFma : \" << time_span.count() << \"\\n\";\n" +
          "}\n")

      outTest.write("\n\n")
    }
    outTest.write("printf( \" res : %f \\n\" , res);\n\n\n}")

    outTest.close

  }

}