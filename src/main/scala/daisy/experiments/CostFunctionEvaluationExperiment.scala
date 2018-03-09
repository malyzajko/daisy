


package daisy
package experiment

import scala.collection.immutable.Seq
import scala.collection.mutable.{Set => MSet}
import util.Random
import java.io.FileWriter
import java.io.BufferedWriter

import lang.Trees.Program
import tools.FinitePrecision._
import lang.TreeOps.allVariablesOf
import lang.Trees._
import lang.Types.FinitePrecisionType
import lang.Identifiers._

/**
  Generates random mixed-precision type configurations for a given example,
  evalautes our cost functions on them and generates Scalameter benchmarks.

  Prerequisites:
    - SpecsProcessingPhase
    - SSATransformerPhase (probably)
    - ConstantTransformerPhase (probably, maybe)
    -
 */
object CostFunctionEvaluationExperiment extends DaisyPhase with opt.CostFunctions with tools.RoundoffEvaluators {

  override val name = "cost function eval experiment"
  override val shortName = "cost-eval"
  override val description = "Generates random mixed-precision versions of a given benchmark " +
    "as well as a performance measurement harness"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption("mixed-high-precision", Set("Float64", "DoubleDouble"), "Float64",
      "Which precision to use as the highest precision")
  )
    //ParamOptionDef("mixed-exp-num-post", "number of functions to generate", "10"))

  implicit val debugSection = DebugSectionExperiment

  var reporter: Reporter = null

  type TypeConfig = Map[Identifier, Precision]

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    var availablePrecisions: List[Precision] = List(Float32, Float64)

    val numVersions = 42

    ctx.option[String]("mixed-high-precision") match {
      case "Float64" =>
        availablePrecisions = List(Float32, Float64)
      case "DoubleDouble" =>
        availablePrecisions = List(Float32, Float64, DoubleDouble)
    }

    assert(prg.defs.size == 1)
    // "original" benchmark
    val fnc = prg.defs.head

    val seed = System.currentTimeMillis
    val rand = new Random(seed)

    val outCosts = new BufferedWriter(new FileWriter("cost_functions_data.txt", true))
    outCosts.write(s"\nfnc-name abs-error naive-cost benchmarked-cost (maxDblVarsCost, maxDblOpsCost, maxDblOpsAndVarsCost), seed: $seed\n")


    val newDefs: Seq[FunDef] =
      if (!fnc.precondition.isEmpty && !fnc.body.isEmpty) {

        reporter.info("function: " + fnc.id)

        // val inputValMap = ctx.specInputRanges(fnc.id)
        // val (_, intermediateRanges) = evalRange[SMTRange](fnc.body.get,
        //   inputValMap.map({ case (id, int) => (id -> SMTRange(Variable(id), int)) }),
        //   SMTRange.apply)
        // val rangeMap = intermediateRanges.map(x => (x._1 -> x._2.toInterval))

        // generate numVersions different type configs
        // generate and save at most numVersions random, unique type configurations
        val candidateTypeConfigs = MSet[TypeConfig]()

        val numPrecisions = availablePrecisions.length
        //val highestPrecision = availablePrecisions.last
        val ids = allVariablesOf(fnc.body.get)

        val maxUniqueTypeConfigs: Long = math.pow(numPrecisions,ids.size).toLong
        val maxCandTypeConfigs: Long = math.min(numVersions, maxUniqueTypeConfigs)
        // some wiggle room if the same configs are selected repeatedly
        val maxIterCount = 15 * maxCandTypeConfigs

        var iterCount = 0l
        while (candidateTypeConfigs.size < maxCandTypeConfigs && iterCount < maxIterCount) {
          val tmp = ids.map( id => (id -> availablePrecisions(rand.nextInt(numPrecisions)))).toMap
          candidateTypeConfigs += tmp
          iterCount = iterCount + 1
        }
        if (iterCount >= maxIterCount) reporter.warning("maxIterCount reached in random type assignment")
        if (candidateTypeConfigs.size == maxUniqueTypeConfigs) reporter.info("exhaustive search")

        reporter.info(s"Generated ${candidateTypeConfigs.size} unique type configs")



        // assign types
        candidateTypeConfigs.toList.zipWithIndex.map({
          case (typeConfig, index) =>
            val updatedBody = opt.MixedPrecisionOptimizationPhase.applyFinitePrecision(fnc.body.get, typeConfig)

            val updatedParams = fnc.params.map(valDef =>
              ValDef(valDef.id.changeType(FinitePrecisionType(typeConfig(valDef.id)))))

            val newId = FreshIdentifier(fnc.id.toString + "_" + index)

            val naiveCost = simpleMixedPrecisionCost(fnc.body.get, typeConfig)
            val benchmarkedCost = benchmarkedMixedPrecisionCost(fnc.body.get, typeConfig)

            // val maxDblVarsCost = maximizeDoubleVars(fnc.body.get, typeConfig, goodPrecision=Float32)
            // val maxDblOpsCost = maximizeDoubleOps(fnc.body.get, typeConfig, goodPrecision=Float32)
            // val maxDblOpsAndVarsCost = maximizeDoubleOpsAndVars(fnc.body.get, typeConfig, goodPrecision=Float32)

            val opCount = countNumOps(fnc.body.get, typeConfig)
            val opCountString =  s"(${opCount(Float32)},${opCount(Float64)},${opCount(DoubleDouble)})"

            // for info, also the absolute error of this
            val (absError, returnType) = opt.MixedPrecisionOptimizationPhase.computeAbsError(
              fnc.body.get, typeConfig, availablePrecisions.last, ctx.intermediateRanges(fnc.id))

            //val infoString = s"${fnc.id}_$index ${absError._1} $naiveCost $benchmarkedCost $maxDblVarsCost $maxDblOpsCost $maxDblOpsAndVarsCost"
            val infoString = s"${fnc.id}_$index ${absError} $naiveCost $benchmarkedCost $opCountString"

            outCosts.write(infoString + "\n")
            reporter.info(infoString)

            fnc.copy(id = newId,
              returnType = FinitePrecisionType(returnType),
              params = updatedParams,
              body = Some(updatedBody))

        })

      } else {
        throw new Exception("Precondition and body cannot be empty")
      }
    outCosts.close
    println(newDefs)

    val newProgram = Program(prg.id, newDefs)
    println(utils.PrettyPrinter(newProgram))

    val fileName = s"src/test/resources/mixed-precision/${prg.id}CostEval${availablePrecisions.last}.scala"
    val fstream = new FileWriter(fileName)
    val out = new BufferedWriter(fstream)
    utils.CodePrinter(newProgram, ctx, "Scala", out)

    MixedPrecisionExperimentGenerationPhase.generateScalabenchCode(
      newProgram, s"CostEval${availablePrecisions.last}", ctx.specInputRanges(fnc.id), "daisy.bench.costeval")

    (ctx, newProgram)
  }

}