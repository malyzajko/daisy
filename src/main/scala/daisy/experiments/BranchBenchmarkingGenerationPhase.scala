package daisy
package experiment

import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.Trees._
import daisy.lang.Types.FinitePrecisionType
import daisy.opt.MixedPrecisionOptimizationPhase
import daisy.tools.FinitePrecision.Float32
import daisy.tools.{Interval, Rational}

import scala.util.Random

object BranchBenchmarkingGenerationPhase extends DaisyPhase {

  override val name = "Branch Benchmarking Generation"
  override val description = "generates the branch benchmarking program"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("max-num-branches", 64, "maximum number of branches to generate"),
    StringChoiceOption("branch-generation", Set("cascaded", "sequential"), "cascaded")
  )

  implicit val debugSection: DebugSection = DebugSectionExperiment

  var reporter: Reporter = _

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val maximumNumberOfBranches = ctx.option[Long]("max-num-branches").toInt

    val defaultPrecisionType = FinitePrecisionType(Float32)
    val referenceRange = Interval(Rational(-100), Rational(100))
    val defaultError = Rational.zero
    val maximumOffsetInteger = 100

    val benchmarkingFuns = analyzeConsideredFunctions(ctx, prg) { f =>
      val variableToSplitBy = f.params.head.id
      val inputRange = ctx.specInputRanges(f.id)(variableToSplitBy)

      val generationFunction: (List[Interval], () => Expr, Identifier, Int) => (Expr, Int) =
        ctx.option[String]("branch-generation") match {
          case "cascaded" => bodyInNBranchesCascaded
          case "sequential" => bodyInNBranchesLinear
        }

      val bodyWithPrecisions = MixedPrecisionOptimizationPhase.applyFinitePrecision(f.body.get,
        Map().withDefaultValue(defaultPrecisionType.prec))(defaultPrecisionType.prec)

      val retValId = FreshIdentifier("_retVal")
      val bodyGenerator = () => {
        val randomOffset = Rational(Random.nextInt(maximumOffsetInteger))
        Let(retValId, Plus(bodyWithPrecisions,
          RealLiteral(randomOffset, randomOffset.toString)), Variable(retValId))
      }

      val updatedParams = f.params.map(valDef => ValDef(valDef.id.changeType(defaultPrecisionType)))

      val newFuns = (1 to maximumNumberOfBranches).map(numBranches => {
        val divided = inputRange.divide(numBranches)

        val (branchedBody, maxDepth) = generationFunction(divided, bodyGenerator, variableToSplitBy, 0)

        f.copy(id = FreshIdentifier(s"${f.id.name}_${maxDepth}_$numBranches"),
          body = Some(branchedBody),
          params = updatedParams,
          returnType = defaultPrecisionType)
      })


      f.copy(id = FreshIdentifier(s"${f.id.name}_0_0"),
        body = Some(Let(retValId, bodyWithPrecisions, Variable(retValId))),
        params = updatedParams,
        returnType = defaultPrecisionType) +: newFuns
    }

    val newInputRanges = benchmarkingFuns.flatMap { case (id, benchmarkingFuns) =>
      val inputRange = ctx.specInputRanges(id).withDefaultValue(referenceRange)
      benchmarkingFuns.map(f => (f.id, inputRange))
    }

    val newInputErrors = benchmarkingFuns.flatMap { case (id, benchmarkingFuns) =>
      val inputError = ctx.specInputErrors(id).withDefaultValue(defaultError)
      benchmarkingFuns.map(f => (f.id, inputError))
    }

    val newDefs = benchmarkingFuns.values.flatten

    val newUniformPrecisions = benchmarkingFuns.flatMap { case (id, benchmarkingFuns) =>
      benchmarkingFuns.map(f => (f.id, defaultPrecisionType.prec))
    }

    (ctx.copy(specInputRanges = newInputRanges, specInputErrors = newInputErrors,
      uniformPrecisions = newUniformPrecisions), prg.copy(defs = newDefs.toSeq))
  }

  private def bodyInNBranchesCascaded(inputs: List[Interval], bodyGenerator: () => Expr, variable: Identifier,
    currentDepth: Int = 0): (Expr, Int) = {

    inputs match {
      case List(_) =>
        (bodyGenerator(), currentDepth)
      case _ =>

        val (inputPartsLow, inputPartsHigh) = inputs.sortBy(_.xlo).splitAt(inputs.size / 2)
        assert(inputPartsLow.last.xhi == inputPartsHigh.head.xlo)

        val condition = LessEquals(Variable(variable),
          RealLiteral(inputPartsLow.last.xhi, inputPartsLow.last.xhi.toString))

        val (lowerExpression, lowerMaxDepth) = bodyInNBranchesCascaded(inputPartsLow, bodyGenerator,
          variable, currentDepth + 1)
        val (upperExpression, upperMaxDepth) = bodyInNBranchesCascaded(inputPartsHigh, bodyGenerator,
          variable, currentDepth + 1)

        (IfExpr(cond = condition, thenn = lowerExpression, elze = upperExpression),
          Math.max(lowerMaxDepth, upperMaxDepth))
    }
  }

  private def bodyInNBranchesLinear(inputs: List[Interval], bodyGenerator: () => Expr, variable: Identifier,
    currentDepth: Int = 0): (Expr, Int) = {

    inputs match {
      case List(_) =>
        (bodyGenerator(), currentDepth)
      case head :: tail =>

        val condition = And(LessEquals(Variable(variable), RealLiteral(head.xhi, head.xhi.toString)),
          GreaterEquals(Variable(variable), RealLiteral(head.xlo, head.xlo.toString)))

        val (elseBranch, elseBranchDepth) = bodyInNBranchesLinear(tail, bodyGenerator, variable, currentDepth + 1)
        (IfExpr(cond = condition, thenn = bodyGenerator(), elze = elseBranch), elseBranchDepth)
    }
  }

}
