

package daisy
package experiment

import lang.Trees.Program
import scala.collection.immutable.Seq
import java.io.FileWriter
import java.io.BufferedWriter


import tools.FinitePrecision._
import lang.Trees._
import tools.{Rational, Interval}
import lang.Identifiers._
import lang.Types.RealType

/**
  Generates benchmarks for mixed-precision optimization by creating copies
  of a given benchmark with different postconditions.


  Prerequisites:
    - SpecsProcessingPhase
 */
object MixedPrecisionExperimentGenerationPhase extends DaisyPhase with tools.RoundoffEvaluators {

  override val name = "mixed-precision experiment gen"
  override val shortName = "mixed-exp-gen"
  override val description = "Generates mixed-precision versions of a given benchmark " +
    "as well as a performance measurement harness"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption("mixed-exp-gen-high-precision", Set("Float64", "DoubleDouble"), "Float64",
      "Which precision to use as the highest precision")
  )

  implicit val debugSection = DebugSectionExperiment

  var reporter: Reporter = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    val lowPrecision = Float32
    var highPrecision: Precision = Float64

    // if this is 2, then it will just postconditions, which should make the
    // mixed-precision optimization generate the high-precision uniform and
    // low-precision uniform versions.
    val numPostconditions = 10

    ctx.option[String]("mixed-high-precision") match {
      case "Float64" =>
        highPrecision = Float64
      case "DoubleDouble" =>
        highPrecision = DoubleDouble
    }

    assert(prg.defs.size == 1)
    // "original" benchmark
    val fnc = prg.defs.head

    val newDefs: Seq[FunDef] =
      if (!fnc.precondition.isEmpty && !fnc.body.isEmpty) {

        reporter.info("function: " + fnc.id)

        val inputValMap = ctx.specInputRanges(fnc.id)
        val inputErrorMapLowPrec = {
          // we only take into account roundoff errors, and no input errors
          val allIDs = fnc.params.map(_.id)
          allIDs.map( id => (id -> lowPrecision.absRoundoff(inputValMap(id)))).toMap
        }
        val inputErrorMapHighPrec = {
          // we only take into account roundoff errors, and no input errors
          val allIDs = fnc.params.map(_.id)
          allIDs.map( id => (id -> highPrecision.absRoundoff(inputValMap(id)))).toMap
        }

        val lowPrecError = uniformRoundoff_IA_AA(fnc.body.get,
          inputValMap, inputErrorMapLowPrec, lowPrecision)._1
        val highPrecError = uniformRoundoff_IA_AA(fnc.body.get,
          inputValMap, inputErrorMapHighPrec, highPrecision)._1

        reporter.info("low  prec error: " + lowPrecError)
        reporter.info("high prec error: " + highPrecError)

        // logarithmic
        val minVal = lowPrecError.toDouble
        val maxVal = highPrecError.toDouble
        val postconditionErrors: Seq[Rational] = {

          val minValLog = math.log(minVal)
          val maxValLog = math.log(maxVal)

          val increment = (maxValLog - minValLog)/(numPostconditions - 1)

          for(i <- 0 until numPostconditions) yield {
            val dbl = math.exp(minValLog + (increment * i))
            Rational.fromString(roundDouble(dbl))
          }
        }

        reporter.info("generated postcondition errors: " + postconditionErrors)

        postconditionErrors.zipWithIndex.map({
          case (err, index) =>
            // copy fnc and attach new postcondition
            val resVar = FreshIdentifier("res", RealType)
            val errLiteral = RealLiteral(err, err.toString)
            fnc.copy(id = FreshIdentifier(fnc.id.toString + "_" + index), postcondition =
              Some(Lambda(Seq(ValDef(resVar)), AbsError(Variable(resVar), errLiteral))))

        })

      } else {
        throw new Exception("Precondition and body cannot be empty")
      }

    val newProgram = Program(prg.id, newDefs)
    println(utils.BenchmarkPrinter(newProgram))

    // val suffixes = Seq("_Float", "_Double", "_DblDouble", "_32", "_32_05", "_32_01", "_32_001", "_64", "_64_05", "_64_01", "_64_001", "_dbldbl")
    // val newDefs: Seq[FunDef] =
    //   if (!fnc.precondition.isEmpty && !fnc.body.isEmpty) {
    //     suffixes.map(suffix => {
    //       fnc.copy(id = FreshIdentifier(fnc.id.toString + suffix))
    //       })
    //   } else Seq()
    // val newProgram = Program(prg.id, newDefs)
    // println(lang.BenchmarkPrinter(newProgram))

    val fileName = s"src/test/resources/mixed-precision/${prg.id}MultPost$highPrecision.scala"
    val fstream = new FileWriter(fileName)
    val outProgram = new BufferedWriter(fstream)
    outProgram.write(utils.BenchmarkPrinter(newProgram))
    outProgram.close


    MixedPrecisionExperimentGenerationPhase.generateScalabenchCode(
      newProgram, s"MultPost$highPrecision", ctx.specInputRanges(fnc.id), "daisy.bench")
      //newProgram, s"MixedBench", ctx.inputRanges(fnc.id), "daisy.bench.mixed")

    (ctx, newProgram)
  }

  def generateScalabenchCode(prg: Program, suffix: String, inputRanges: Map[Identifier, Interval], packageName: String): Unit = {

    val outTest = new BufferedWriter(new FileWriter("src/bench/scala/mixed-precision/generated/" + prg.id + s"Benchmark$suffix.scala"))

    outTest.write(
      s"package $packageName\n\n" +
      //s"package bench\n\n" +
      "import org.scalameter.api._\n" +
      "import org.scalameter.picklers.Implicits._\n" +
      "import scala.annotation.strictfp\n" +
      "import scala.util.Random\n" +
      s"import ${prg.id}._\n\n")

    outTest.write("@strictfp\n")
    outTest.write(s"object ${prg.id}Benchmark$suffix extends Bench[Double] {\n")

    outTest.write(
      "  lazy val executor = SeparateJvmsExecutor(\n" +
      "    new Executor.Warmer.Default,\n" +
      "    Aggregator.average,\n" +
      "    measurer)\n  " +
      "lazy val measurer = new Measurer.Default\n  " +
      "def persistor = Persistor.None\n  " +
      "def reporter = new LoggingReporter[Double]//new DsvReporter(',')\n\n" +
      "  val rand = new Random(System.currentTimeMillis)\n" +
      "  val numRuns = 100000\n" +
      "  val nix = Gen.unit(\"unit\")\n")

    // We only generate the harness for one benchmark
    val inputParams: Seq[ValDef] = prg.defs.head.params

    // Generate randomized inputs in range
    inputParams.zipWithIndex.foreach {
      case (currParam, index) =>

        val currParamInterval = inputRanges(currParam.id)

        outTest.write("  val input" + (index + 1) + "Min = " +
          currParamInterval.xlo.floatValue + "f\n")

        outTest.write("  val input" + (index + 1) + "Max = " +
          currParamInterval.xhi.floatValue +  "f\n")

        outTest.write(
          "  val inputsFloat" + (index + 1) + " = Array.fill(numRuns) {\n" +
            "    input"+ (index + 1) +"Min + (input"+ (index + 1) +
              "Max - input"+ (index + 1) +"Min) * rand.nextFloat()\n" +
            "  }\n")
    }

    outTest.write("  var counter = 0.0\n\n")

    prg.defs.foreach { currDef: FunDef =>
      val currFncName = currDef.id.toString
      outTest.write(
        "  counter = 0.0\n" +
        "  performance of \"" + currFncName + "\" in {\n" +
        "    measure method (\"" + "\t" + currFncName + "\") in {\n" +
        "      using(nix) in {\n" +
        "        r => {\n" +
        "          var i = 0\n" +
       s"          var res = $currFncName(" +
          inputParams.zipWithIndex.map({
            case (valDef, index) => "inputsFloat" + (index + 1) + "(0)"
          }).mkString(", ") + ")\n" +
        "          while(i < numRuns) {\n" +
       s"            res += $currFncName(")

      outTest.write(inputParams.zipWithIndex.map({
        case (valDef, index) =>
          "inputsFloat" + (index + 1) + "(i)"
      }).mkString(", "))

      outTest.write(
        ")\n" +
          "            i = i + 1\n" +
          "          }\n" +
          "          counter += res.toDouble\n" +  //"          counter += res + i\n" +
          "        }\n" +
          "      }\n" +
          "    }\n" +
          "  }\n\n")
    }
    outTest.write("\n\n}")
    outTest.close

  }

  private def roundDouble(dbl: Double): String = {
    val dblString = dbl.toString
    if (dblString.contains('E')) {

      val split = dblString.split('E')
      val mantissa = split(0)
      val exponent = split(1)
      val shortenedMantissa = (mantissa.take(5)).toDouble + 0.001
      shortenedMantissa + "E" + exponent

    } else {

      val indexOfFirstNonZero = dblString.indexWhere(char => char != '0' && char != '.')
      val split = dblString.splitAt(indexOfFirstNonZero)
      val zeroPrefix = split._1
      val mantissa = split._2

      val shortenedMantissa = if(mantissa.contains('.')) {
        (mantissa.take(5)).toDouble + 0.001
      } else {
        (mantissa.take(5)).toInt + 1
      }
      zeroPrefix + shortenedMantissa

    }
  }

}