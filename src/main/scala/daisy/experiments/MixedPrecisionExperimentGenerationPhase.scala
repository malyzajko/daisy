

package daisy
package experiment

import java.io.{BufferedWriter, FileWriter}

import daisy.analysis.{DataflowPhase, DataflowSubdivisionPhase}
import daisy.lang.Identifiers._
import daisy.lang.TreeOps
import daisy.lang.Trees.{Program, _}
import daisy.lang.Types.RealType
import daisy.tools.FinitePrecision._
import daisy.tools.Interval
import daisy.utils.ScalaPrinter
import scala.collection.parallel.CollectionConverters._

import scala.collection.immutable.Seq

/**
 * Generates benchmarks for mixed-precision optimization by creating copies
 * of a given benchmark with different postconditions.
 * *
 *
 * Prerequisites:
 *- SpecsProcessingPhase
 */
object MixedPrecisionExperimentGenerationPhase extends DaisyPhase with tools.RoundoffEvaluators with tools. Subdivision {

  override val name = "Mixed-precision experiment gen"
  override val description = "Generates mixed-precision versions of a given benchmark " +
    "as well as a performance measurement harness"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringOption("outputFolder", "output folder name"),
    MultiStringOption(
      "bench-precisions",
      List("Float16", "Float32", "Float64", "Float128"),
      "Which precisions to consider as basis")
  )

  override implicit val debugSection = DebugSectionExperiment

  var reporter: Reporter = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    //val totalOpt = ctx.option[Long]("totalOpt")

    val precisions = ctx.option[Seq[String]]("bench-precisions").map({
      case "Float16" => Float16
      case "Float32" => Float32
      case "Float64" => Float64
      case "Float128" => Float128
      case str if str.startsWith("Fixed") => FixedPrecision(str.replace("Fixed", "").toInt)
    })
    //println(precisions)
    val scaleFactor = tools.Rational.fromString("10.0")      // which factor to apply to the computed error

    val definedFncs = prg.defs.filter(fnc => fnc.precondition.isDefined && fnc.body.isDefined)
    val newDefs: Seq[FunDef] = definedFncs.flatMap({ fnc =>
      if (benchmarkSupported(fnc.body.get)) {
        reporter.info(s"Skipping ${fnc.id} because it contains an if-expression")
        Seq()
      } else {
        val inputValMap = ctx.specInputRanges(fnc.id)

        // generate one function/postcondition for each precision
        precisions.flatMap(prec => {
          // we only take into account roundoff errors, and no input errors
          val allIDs = fnc.params.map(_.id)
          val inputErrorMap =  allIDs.map(id => (id -> prec.absRoundoff(inputValMap(id)))).toMap

          val precisionMap = Map[Identifier, Precision]().withDefaultValue(prec)

          // this is a very hacky way of running subdivisions with the right options
          val tempContext = ctx.copy(
            specInputErrors = Map(fnc.id -> inputErrorMap),
            specInputPrecisions = Map(fnc.id -> precisionMap))

          try {
            val (context, _) = DataflowSubdivisionPhase.runPhase(tempContext, prg.copy(defs = Seq(fnc)))

            val postconditionError = context.resultAbsoluteErrors(fnc.id) / scaleFactor

            val suffix = "order"//prec.toString.replace("Float", "")
            val newFncName = s"${fnc.id.toString}_${suffix}" //_${scaleFactor.toString.replace(".", "_")}"

            val resVar = FreshIdentifier("res", RealType)
            val errLiteral = RealLiteral(postconditionError, postconditionError.toString)

            // for some reason, having the result assigned to a variable changes the result
            // find out why
            val newBody = TreeOps.updateLastExpression({
              case v @ Variable(_) => v
                case e =>
                  val resultIdentifier = FreshIdentifier("_ret", RealType, alwaysShowUniqueID = true)
                  Let(resultIdentifier, e, Variable(resultIdentifier))
              })(fnc.body.get)

            Some(fnc.copy(id = FreshIdentifier(newFncName),
              body = Some(newBody),
              postcondition = Some(Lambda(Seq(ValDef(resVar)), AbsError(Variable(resVar), errLiteral)))))

          } catch {
            case e: Throwable =>
              reporter.info(s"Error for prec ${fnc.id}: $e")
              None
          }
        })
      }
    })

    val folderName = ctx.option[Option[String]]("outputFolder") match {
      case Some(name) => name
      case None => "output"
    }
    // need to print here and not in CodeGenerationPhase as we need the benchmarks separately
    for(fnc <- newDefs) {
      val tmpProgram = Program(FreshIdentifier(fnc.id.toString), Seq(fnc))
      backend.CodeGenerationPhase.writeFile(tmpProgram, "DaisyInput", ctx, folderName)
    }

    val newProgram = Program(prg.id, newDefs)

    (ctx, newProgram)
  }

  def benchmarkSupported(body: Expr) = {
    TreeOps.exists({
      case IfExpr(_, _, _) => true
      case _ => false
    })(body)
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