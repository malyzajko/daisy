// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package opt

import scala.concurrent._
import java.io._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._

import lang.Trees._
import tools.{Interval, Rational}
import tools.FinitePrecision._
import lang.Identifiers._
import lang.Types._

object FPTunerPhase extends DaisyPhase {
  override val name = "fptuner"
  override val description = "Calls FPTuner to do mixed-precision tuning"

  implicit val debugSection = DebugSectionOptimization
  var reporter: Reporter = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    if (ctx.hasFlag("regime-inf")) {

      val regimesAndContexts = analyzeConsideredFunctions(ctx, prg) { fnc =>
      // TODO: add a check to make sure that the function returns a single variable (not a complex expression)

        // apply FPTuner to individual regimes
        val regime = ctx.regimes(fnc.id)

        val fpTunerRegimes = regime.map(r => {
          val (typeConfig, newDef) = optimizeWithFPTuner(r.intervalMap, fnc, ctx.specResultErrorBounds(fnc.id))

          val returnPrecision = newDef.returnType match {
            case FinitePrecisionType(prec) => prec
          }

          r.copy(evaluationBody=fnc.body.get,
            rewrittenBody=newDef.body.get,
            inputPrecisions=typeConfig,
            resultPrecision=returnPrecision)

        })

        RegimeInferencePhase.introduceCastsForInputAndReturnValues(fnc.params.map(_.id), fpTunerRegimes,
          ctx.specInputPrecisions(fnc.id))
      }

      val regimes = regimesAndContexts.mapValues(_._1.toSeq).toMap

      val specInputPrecisions = regimesAndContexts.map {
        case (id, (_, inputPrecisions, _)) => (id, inputPrecisions)
      }

      val specResultPrecisions = regimesAndContexts.map {
        case (id, (_, _, returnPrecision)) => (id, returnPrecision)
      }

      val combinedContext = ctx.copy(
        regimes = regimes,
        specInputPrecisions = specInputPrecisions,
        specResultPrecisions = specResultPrecisions
      )

      (combinedContext, prg)


    } else {

      var newDefs: Seq[FunDef] = Seq()

      analyzeConsideredFunctions(ctx, prg) { fnc =>

        val (typeConfig, newDef) = optimizeWithFPTuner(ctx.specInputRanges(fnc.id),
          fnc, ctx.specResultErrorBounds(fnc.id))


        newDefs = newDefs ++ Seq(newDef)


      }
      (ctx, Program(prg.id, newDefs))
    }

  }

  def optimizeWithFPTuner(inputRanges: Map[Identifier, Interval], fnc: FunDef,
    errorBound: Rational): (Map[Identifier, Precision], FunDef) = {

    reporter.info(s"calling FPTuner on ${inputRanges}")
    val body = fnc.body.get

    // step 1: generate and print FPTuner file
    var varMapping: Map[Int, Identifier] = Map()

    val problemDef = new PrintWriter(new File("fptuner_prob.py"))
    problemDef.write("import tft_ir_api as IR\n\n")

    // define variables
    inputRanges.zipWithIndex.map({
      case ((id, Interval(lo, hi)), index) =>
        varMapping = varMapping + (index -> id)
        problemDef.write(s"""$id = IR.RealVE("$id", $index, $lo, $hi)\n""")
    })
    val initCount = inputRanges.size

    val resId = FreshIdentifier("_rel") // dummy, TODO: remove
    val (expr, map) = printFPTunerExpr(body, initCount, resId)
    problemDef.write(expr + "\n")

    varMapping = varMapping ++ map
    //println(s"size varMapping: ${varMapping.size}")

    lang.TreeOps.getLastExpression(body) match {
      case Variable(retId) =>
        problemDef.write(s"IR.TuneExpr(${retId})\n")
    }
    problemDef.close()

    // step 2: call FPTuner
    //val output = callFPTuner("fptuner_prob.py", ctx.specResultErrorBounds(fnc.id))
    //callFPTunerDummy("fptuner_prob.py", ctx.specResultErrorBounds(fnc.id)) match {

    val output = callFPTuner("fptuner_prob.py", errorBound)

    //println(s"output : $output")
    // step 3: parse result back and put into Daisy format
    val indexBitsMap: Map[Int, Int] = parseFPTunerOutput(output)
    //println(s"indexBitsMap: $indexBitsMap")

    var typeConfig: Map[Identifier, Precision] = indexBitsMap.map({
      case (index, bits) =>
        if (bits == 32) {
          (varMapping(index) -> Float32)
        } else if (bits == 64) {
          (varMapping(index) -> Float64)
        } else {
          assert(bits == 128)
          (varMapping(index) -> Float128)
        }
    })
    if (typeConfig.isEmpty && output.contains("TFT: no available allocation for the main expr...")) {
      reporter.info("No allocation found, assigning uniform 128")
      typeConfig = varMapping.map({
        case (i, id) => (id -> Float128)
      })
    }
    if (typeConfig.isEmpty) {
      reporter.warning(output)
    }

    val resPrecision = typeConfig(lang.TreeOps.getLastExpression(body).asInstanceOf[Variable].id)

    val updatedBody = opt.MixedPrecisionOptimizationPhase.applyFinitePrecision(
      body, typeConfig)(resPrecision)

    val updatedParams = fnc.params.map(valDef =>
      ValDef(valDef.id.changeType(FinitePrecisionType(typeConfig(valDef.id)))))

    val newDef = fnc.copy(returnType = FinitePrecisionType(resPrecision), params = updatedParams,
      body = Some(updatedBody))

    (typeConfig, newDef)

  }

  // returns a map from index to number of bits of precision
  def parseFPTunerOutput(output: String): Map[Int, Int] = {
    val list = output.split("\n")
    list.filter(line => line.startsWith("Group")).map(line => {
      //Group 0 : 32-bit
      val assignment = line.replace("Group ", "").replace("-bit", "").split(" : ")
      assert(assignment.size == 2)
      (assignment(0).toInt -> assignment(1).toInt)
    }).toMap
  }

  def callFPTunerDummy(problemDef: String, errorBound: Rational, timeOut: Int = 120): Option[String] = {
    Some("""==== error bound : 0.001 ====
      |Total # of operators: 5
      |# of 32-bit operators: 2
      |# of 64-bit operators: 3
      |
      |---- alloc. ----
      |Group 0 : 32-bit
      |Group 1 : 32-bit
      |Group 2 : 64-bit
      |Group 3 : 64-bit
      |Group 4 : 64-bit
      |----------------
      |
      |# L2H castings: 2
      |# H2L castings: 0
      |# Castings: 2
      |
      |Expression:
      |(* (+ (A) (B)) (C))
      |""".stripMargin)
  }

  def callFPTuner(problemDef: String, errorBound: Rational, timeOut: Int = 600): String = {
    var output: String = ""

    val f: Future[Unit] = Future {

      //println(s"""python3 ../FPTuner/bin/fptuner.py -b '64 128' -e ${errorBound} ${problemDef}""")
      // TODO: make FPTuner path an option

      val commands = Array("python3", "../FPTuner/bin/fptuner.py", "-b", "64 128",
        "-e", s"${errorBound}", s"${problemDef}")
      //println(commands.mkString(" "))

      val problemDefRes = Runtime.getRuntime().exec(commands)

      /* Read the problemDefRes */
      val stdInput =  new BufferedReader(new InputStreamReader(problemDefRes.getInputStream))
      var readResLine: String = ""
      readResLine = stdInput.readLine()

      /* Recover the last line where all the information we need is */
      while(readResLine != null){
        output = output + readResLine + "\n"
        //println(readResLine)
        readResLine = stdInput.readLine()
      }

      problemDefRes.getInputStream().close()
    }

    try {
       Await.result(f, timeOut.second)

    }

    /*catch {
      case e: java.util.concurrent.TimeoutException =>
        reporter.warning(s"FPTuner has timed out after $timeOut seconds.");
        return None
      case e: java.io.IOException =>
        reporter.warning(e.getMessage())
        return None
      case e: Exception =>
         reporter.warning("Something went wrong. Here's the stack trace:");
         e.printStackTrace
         return None
    }*/
    output
  }

  private def printFPTunerExpr(e: Expr, count: Int, resId: Identifier): (String, Map[Int, Identifier]) = e match {
    case Let(b, Plus(Variable(lhs), Variable(rhs)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.BE("+", $count, $lhs, $rhs)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Minus(Variable(lhs), Variable(rhs)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.BE("-", $count, $lhs, $rhs)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Times(Variable(lhs), Variable(rhs)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.BE("*", $count, $lhs, $rhs)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Division(Variable(lhs), Variable(rhs)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.BE("/", $count, $lhs, $rhs)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, UMinus(Variable(t)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.UE("-", $count, $t)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Exp(Variable(t)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.UE("exp", $count, $t)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Log(Variable(t)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.UE("log", $count, $t)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Sqrt(Variable(t)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.UE("sqrt", $count, $t)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Sin(Variable(t)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.UE("sin", $count, $t)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Cos(Variable(t)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.UE("cos", $count, $t)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Tan(Variable(t)), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.UE("tan", $count, $t)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Atan(Variable(t)), body) =>
      throw new tools.AtanNotSupportedException("FPTuner does not support atan.")
      null

    case Let(b, RealLiteral(v), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = IR.RealVE("$b", $count, $v, $v)\n$bodyExpr""", bodyMap + (count -> b))

    case Let(b, Variable(v), body) =>
      val (bodyExpr, bodyMap) = printFPTunerExpr(body, count + 1, resId)

        (s"""$b = $v\n$bodyExpr""", bodyMap + (count -> b))

    case Plus(Variable(lhs), Variable(rhs)) =>
      (s"""IR.BE("+", $count, $lhs, $rhs)""", Map())

    case Minus(Variable(lhs), Variable(rhs)) =>
      (s"""IR.BE("-", $count, $lhs, $rhs)""", Map())

    case Times(Variable(lhs), Variable(rhs)) =>
      (s"""IR.BE("*", $count, $lhs, $rhs)""", Map())

    case Division(Variable(lhs), Variable(rhs)) =>
      (s"""IR.BE("/", $count, $lhs, $rhs)""", Map())

    case UMinus(Variable(t)) =>
      (s"""IR.UE("-",  $count, $t)""", Map())

    // case RealLiteral(v) =>
    //   (s"IR.FConst($v)", Map())

    case Variable(v) => // this will necessarily be a return variable
      ("", Map())
  }


}
