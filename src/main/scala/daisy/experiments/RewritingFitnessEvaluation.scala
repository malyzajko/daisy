
package daisy
package experiment

import scala.collection.immutable.Set
import scala.collection.parallel.immutable.ParMap
import java.io.FileWriter
import java.io.BufferedWriter

import lang.Trees.{Program, Expr}
import lang.Identifiers._
import tools.{Interval, Rational}
import tools.FinitePrecision._
import analysis.ErrorMeasurerMPFR

object RewritingFitnessEvaluation extends DaisyPhase with tools.RoundoffEvaluators
  with tools.DynamicEvaluators with opt.RewritingOps {

  override val name = "rewriting-fitness-eval"
  override val shortName = "fitness-eval"
  override val description = "fitness function evaluation for rewriting"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption("rewrite-baseline-type", Set("absolute", "relative"),
      "absolute", "Which baseline metric to use: 'absolute' or 'relative' error")
  )

  implicit val debugSection = DebugSectionExperiment

  var reporter: Reporter = null

  val numRewritings = 200
  val numBaselineSamples = 100000
  var seed = System.currentTimeMillis
  var rand = new util.Random(seed)

  val uniformPrecision = Float64

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    val logFile = new BufferedWriter(new FileWriter("rawdata/rewrite-fiteval-log.txt"))
    var baselineType: String = "absolute"

    baselineType = ctx.option[String]("rewrite-baseline-type")

    val infoString = s"seed: $seed, # rewritings: $numRewritings, baseline type: $baselineType, # baseline samples: $numBaselineSamples"
    reporter.info(infoString)
    if (logFile != null) {
      logFile.write("# " + infoString +"\n")
      logFile.write(s"# fraction of correctly distinguished expressions for\n" +
          "# benchmark int-affine, aff-affine, smt-affine, int-int, aff-int, smt-int, dyn-256-abs, dyn-1000-abs, dyn-5000-abs, "+
          "dyn-256-rel, dyn-1000-rel, dyn-5000-rel, dyn-256-avrg-abs, dyn-1000-avrg-abs, dyn-5000-avrg-abs, "+
          "dyn-256-avrg-rel, dyn-1000-avrg-rel, dyn-5000-avrg-rel\n")

    }

    val fncsToConsider = functionsToConsider(ctx, prg)
    for (fnc <- prg.defs) if (!fnc.precondition.isEmpty && !fnc.body.isEmpty &&
      fncsToConsider.contains(fnc.id.toString)) {

      // ----- generate X random rewritings -----
      val exprsGenerated: Set[Expr] = generateMutants(Set(fnc.body.get))


      reporter.info(s"generated ${exprsGenerated.size} rewritings for ${fnc.id}")
      val exprs = if (exprsGenerated.size < numRewritings) {
        reporter.warning("Not enough rewritings generated.")
        exprsGenerated
      } else {
        // pick a random subset
        rand.shuffle(exprsGenerated).take(numRewritings)
      }
      reporter.info(s"considering ${exprs.size} of those")
      //println(exprs.mkString("\n"))

      // ----- evaluate them on baseline -----
      val inputValMap: Map[Identifier, Interval] = ctx.specInputRanges(fnc.id)
      val inputErrors = fnc.params.map(_.id).map {
        id => (id -> uniformPrecision.absRoundoff(inputValMap(id)))
      }.toMap

      // this is expensive, so should run in parallel
      val start = System.currentTimeMillis
      val baseline = if (baselineType == "absolute") {
          exprs.par.map(e => {
            val measurer = generalErrorDynamicWithInputRoundoff(e, inputValMap, numBaselineSamples)
            (e -> Rational.fromString(measurer.maxAbsError.toString()))
          }).toMap
        } else {
          exprs.par.map(e => {
            val measurer = generalErrorDynamicWithInputRoundoff(e, inputValMap, numBaselineSamples)
            (e -> Rational.fromString(measurer.maxRelError.toString()))
          }).toMap
        }
      val baselineTime = (System.currentTimeMillis - start)
      reporter.info(s"time taken for baseline: $baselineTime ms")

      val equalCountBaseline: Int = (for { x <- exprs.toList; y <- exprs.toList if (baseline(x) == baseline(y))} yield {
        1
      }).size
      reporter.info("baseline equal count: " + equalCountBaseline)

      // TODO: perhaps check that these are sufficiently different?
      //println("baseline results: \n" + baseline.mkString("\n"))


      // ----- apply fitness functions and compare the results -----

      //val numExprGenerated = exprs.size
      //val total = numExprGenerated * numExprGenerated

      // Affine for errors
      val (resIntAffine, timeIntAffine) = generateValidStatic(exprs, uniformRoundoff_IA_AA(_, inputValMap, inputErrors, uniformPrecision)._1)
      val (fracIntAffine, equalCountIntAffine) = compareAgainstBaseline(resIntAffine, baseline, "int-affine")

      val (resAffAffine, timeAffAffine) = generateValidStatic(exprs, uniformRoundoff_AA_AA(_, inputValMap, inputErrors, uniformPrecision)._1)
      val (fracAffAffine, equalCountAffAffine) = compareAgainstBaseline(resAffAffine, baseline, "aff-affine")

      val (resSMTAffine, timeSMTAffine) = generateValidStatic(exprs, uniformRoundoff_SMT_AA(_, inputValMap, inputErrors, uniformPrecision)._1)
      val (fracSMTAffine, equalCountSMTAffine) = compareAgainstBaseline(resSMTAffine, baseline, "smt-affine")

      // Intervals for errors
      val (resIntInterval, timeIntInterval): (Map[Expr, Rational], Long) = ??? //generateValidStatic(exprs, errorIntervalInterval(_, inputValMap, inputErrors, uniformPrecision))
      val (fracIntInterval, _) = compareAgainstBaseline(resIntInterval, baseline, "int-int")

      val (resAffInterval, timeAffInterval): (Map[Expr, Rational], Long) = ??? //generateValidStatic(exprs, errorAffineInterval(_, inputValMap, inputErrors, uniformPrecision))
      val (fracAffInterval, _) = compareAgainstBaseline(resAffInterval, baseline, "aff-int")

      val (resSMTInterval, timeSMTInterval): (Map[Expr, Rational], Long) = ??? //generateValidStatic(exprs, errorSMTInterval(_, inputValMap, inputErrors, uniformPrecision))
      val (fracSMTInterval, _) = compareAgainstBaseline(resSMTInterval, baseline, "smt-int")

      // ----- Dynamic 256 samples -------
      val (mesDynamic256, timeDyn256) = generateValidDynamic(exprs, generalErrorDynamicWithInputRoundoff(_, inputValMap, 256))
      // max abs
      val resDyn256_abs = mesDynamic256.map(x => (x._1 -> Rational.fromString(x._2.maxAbsError.toString())))
      val (fracDyn256Abs, equalCountDyn256Abs) = compareAgainstBaseline(resDyn256_abs, baseline, "dyn256Abs")
      // max-rel
      val resDyn256_rel = mesDynamic256.map(x => (x._1 -> Rational.fromString(x._2.maxRelError.toString())))
      val (fracDyn256Rel, equalCountDyn256Rel) = compareAgainstBaseline(resDyn256_rel, baseline, "dyn256Rel")
      // avrg abs
      val resDyn256_avrgAbs = mesDynamic256.map(x => (x._1 -> Rational.fromString(x._2.avrgAbsError.toString())))
      val (fracDyn256AvrgAbs, equalCountDyn256AvrgAbs) = compareAgainstBaseline(resDyn256_avrgAbs, baseline, "dyn256AvrgAbs")

      // avrg rel
      val resDyn256_avrgRel = mesDynamic256.map(x => (x._1 -> Rational.fromString(x._2.avrgRelError.toString())))
      val (fracDyn256AvrgRel, equalCountDyn256AvrgRel) = compareAgainstBaseline(resDyn256_avrgRel, baseline, "dyn256AvrgRel")

      // ----- Dynamic 1000 samples -------
      val (mesDynamic1000, timeDyn1000) = generateValidDynamic(exprs, generalErrorDynamicWithInputRoundoff(_, inputValMap, 1000))
      // max abs
      val resDyn1000_abs = mesDynamic1000.map(x => (x._1 -> Rational.fromString(x._2.maxAbsError.toString())))
      val (fracDyn1000Abs, equalCountDyn1000Abs) = compareAgainstBaseline(resDyn1000_abs, baseline, "dyn1000Abs")

      // max-rel
      val resDyn1000_rel = mesDynamic1000.map(x => (x._1 -> Rational.fromString(x._2.maxRelError.toString())))
      val (fracDyn1000Rel, equalCountDyn1000Rel) = compareAgainstBaseline(resDyn1000_rel, baseline, "dyn1000Rel")

      // avrg abs
      val resDyn1000_avrgAbs = mesDynamic1000.map(x => (x._1 -> Rational.fromString(x._2.avrgAbsError.toString())))
      val (fracDyn1000AvrgAbs, equalCountDyn1000AvrgAbs) = compareAgainstBaseline(resDyn1000_avrgAbs, baseline, "dyn1000AvrgAbs")


      // avrg rel
      val resDyn1000_avrgRel = mesDynamic1000.map(x => (x._1 -> Rational.fromString(x._2.avrgRelError.toString())))
      val (fracDyn1000AvrgRel, equalCountDyn1000AvrgRel) = compareAgainstBaseline(resDyn1000_avrgRel, baseline, "dyn1000AvrgRel")



      // ----- Dynamic 5000 samples -------
      val (mesDynamic5000, timeDyn5000) = generateValidDynamic(exprs, generalErrorDynamicWithInputRoundoff(_, inputValMap, 5000))
      // max abs
      val resDyn5000_abs = mesDynamic5000.map(x => (x._1 -> Rational.fromString(x._2.maxAbsError.toString())))
      val (fracDyn5000Abs, equalCountDyn5000Abs) = compareAgainstBaseline(resDyn5000_abs, baseline, "dyn5000Abs")
      // max-rel
      val resDyn5000_rel = mesDynamic5000.map(x => (x._1 -> Rational.fromString(x._2.maxRelError.toString())))
      val (fracDyn5000Rel, equalCountDyn5000Rel) = compareAgainstBaseline(resDyn5000_rel, baseline, "dyn5000Rel")

      // avrg abs
      val resDyn5000_avrgAbs = mesDynamic5000.map(x => (x._1 -> Rational.fromString(x._2.avrgAbsError.toString())))
      val (fracDyn5000AvrgAbs, equalCountDyn5000AvrgAbs) = compareAgainstBaseline(resDyn5000_avrgAbs, baseline, "dyn5000AvrgAbs")


      // avrg rel
      val resDyn5000_avrgRel = mesDynamic5000.map(x => (x._1 -> Rational.fromString(x._2.avrgRelError.toString())))
      val (fracDyn5000AvrgRel, equalCountDyn5000AvrgRel) = compareAgainstBaseline(resDyn5000_avrgRel, baseline, "dyn5000AvrgRel")


      // val (fracDyn256, timeDyn256, equalCountDyn256) = compareAgainstBaseline(exprs, baseline,
      //   errorDynamicWithInputRoundoff(_, inputValMap, 256), total, "dyn-256   ")

      // val (fracDyn1000, timeDyn1000, equalCountDyn1000) = compareAgainstBaseline(exprs, baseline,
      //   errorDynamicWithInputRoundoff(_, inputValMap, 1000), total, "dyn-1000  ")

      // val (fracDyn5000, timeDyn5000, equalCountDyn5000) = compareAgainstBaseline(exprs, baseline,
      //   errorDynamicWithInputRoundoff(_, inputValMap, 5000), total, "dyn-5000  ")

      // reporter.info(s"equal-counts: $equalCountIntAffine $equalCountAffAffine " +
      //     s"$equalCountSMTAffine $equalCountDyn256 $equalCountDyn1000 $equalCountDyn5000")

      if (logFile != null) {
        logFile.write(f"fraction ${fnc.id} $fracIntAffine%1.4f $fracAffAffine%1.4f $fracSMTAffine%1.4f " +
          f"$fracIntInterval%1.4f $fracAffInterval%1.4f $fracSMTInterval%1.4f " +
          f"$fracDyn256Abs%1.4f $fracDyn1000Abs%1.4f $fracDyn5000Abs%1.4f " +
          f"$fracDyn256Rel%1.4f $fracDyn1000Rel%1.4f $fracDyn5000Rel%1.4f " +
          f"$fracDyn256AvrgAbs%1.4f $fracDyn1000AvrgAbs%1.4f $fracDyn5000AvrgAbs%1.4f " +
          f"$fracDyn256AvrgRel%1.4f $fracDyn1000AvrgRel%1.4f $fracDyn5000AvrgRel%1.4f\n")
        logFile.write(s"time ${fnc.id} $timeIntAffine $timeAffAffine $timeSMTAffine " +
          s"$timeIntInterval $timeAffInterval $timeSMTInterval " +
          s"$timeDyn256 $timeDyn1000 $timeDyn5000\n")
        logFile.write(s"equal-count-baseline ${fnc.id} $equalCountBaseline\n")
        logFile.write(s"equal-count ${fnc.id} $equalCountIntAffine $equalCountAffAffine " +
          s"$equalCountSMTAffine $equalCountDyn256Abs $equalCountDyn1000Abs $equalCountDyn5000Abs " +
          s"$equalCountDyn256Rel $equalCountDyn1000Rel $equalCountDyn5000Rel " +
          s"$equalCountDyn256AvrgAbs $equalCountDyn1000AvrgAbs $equalCountDyn5000AvrgAbs " +
          s"$equalCountDyn256AvrgRel $equalCountDyn1000AvrgRel $equalCountDyn5000AvrgRel\n\n")



      }
    }

    ctx.reporter.info(s"Finished $name")
    if (logFile != null) {
      logFile.close
    }
    (ctx, prg)
  }

  val activeRules = COMMUTATIVITY ++ ASSOCIATIVITY ++ DISTRIBUTIVITY ++ List(IDENTITIES) ++
    FRACTIONS_TRANSFORM ++ FRACTIONS_DISTRIBUTE

  // this is guaranteed to terminate, although it may take a long time (if we are unlucky)
  private def generateMutants(exprs: Set[Expr]): Set[Expr] = {
    if (exprs.size < numRewritings) {
      // for each expression, apply a rule to each node
      val newExprs: Set[Expr] = for (e <- exprs; i <- 0 until sizeWithoutTerminals(e)) yield {
        _mutate(e, i, activeRules)
      }
      val allExprs = exprs ++ newExprs
      // if no new expressions were generated, stop (this may not be an actual fixpoint)
      if (allExprs.size == exprs.size) {
        allExprs
      } else {
        generateMutants(allExprs)
      }

    } else {
      exprs
    }
  }

  // @return (expr -> max-abs-error, time taken)
  private def generateValidStatic(exprs: Set[Expr], fncToTest: Expr => Rational): (Map[Expr, Rational], Long) = {
    val start = System.currentTimeMillis

    // this way, results will only have expressions which have passed the test
    // this means that the total will always be those expressions that can be analyzed
    // and not necessarily those that can be analyzed by all
    val results = exprs.flatMap(e => {
      try {
        Some((e -> fncToTest(e)))
      } catch {
        case e: daisy.tools.DivisionByZeroException =>
          reporter.warning("div-by-zero happily ignored")
          None
      }}).toMap
    val time = (System.currentTimeMillis - start)
    (results, time)
  }

  private def generateValidDynamic(exprs: Set[Expr], fncToTest: Expr => ErrorMeasurerMPFR): (Map[Expr, ErrorMeasurerMPFR], Long) = {
    val start = System.currentTimeMillis

    // this way, results will only have expressions which have passed the test
    // this means that the total will always be those expressions that can be analyzed
    // and not necessarily those that can be analyzed by all
    val results = exprs.flatMap(e => {
      try {
        Some((e -> fncToTest(e)))
      } catch {
        case e: daisy.tools.DivisionByZeroException =>
          reporter.warning("div-by-zero happily ignored")
          None
      }}).toMap
    val time = (System.currentTimeMillis - start)
    (results, time)
  }

  private def compareAgainstBaseline(results: Map[Expr, Rational], baseline: ParMap[Expr, Rational], name: String): (Double, Int) = {

    val exprs = results.keys.toSeq
    var fitnessFncEqualCount = 0

    val correctCount = for { x <- exprs; y <- exprs } yield {
      val bx = baseline(x)
      val by = baseline(y)
      val rx = results(x)
      val ry = results(y)

      // this is just for statistics:
      if (rx == ry) {
        fitnessFncEqualCount += 1
      }

      // not sure how often this is actually the case, but in that case
      // we don't care which expression is picked
      if (bx == by) {
        1
      } else if (bx < by) {
        if (rx < ry) 1 else 0

      } else { // by < bx
        if (ry < rx) 1 else 0
      }
    }
    val correct = correctCount.foldLeft(0)(_ + _)

    val total = exprs.size
    val fraction = correct / (total * total).toDouble
    reporter.info(f"correct $name: $fraction%1.4f (analyzed # exprs: $total)")
    (fraction, fitnessFncEqualCount)
  }

  // private def compareAgainstBaseline(exprs: Set[Expr], baseline: ParMap[Expr, Rational],
  //   fncToTest: Expr => Rational, _total: Int, name: String): (Double, Long, Int) = {

  //   val start = System.currentTimeMillis

  //   // this way, results will only have expressions which have passed the test
  //   // this means that the total will always be those expressions that can be analyzed
  //   // and not necessarily those that can be analyzed by all
  //   val results = exprs.flatMap(e => {
  //     try {
  //       Some((e -> fncToTest(e)))
  //     } catch {
  //       case e: daisy.utils.DivisionByZeroException =>
  //         reporter.warning("div-by-zero happily ignored")
  //         None
  //     }}).toMap

  //   val validExprs = results.keys.toSeq
  //   var fitnessFncEqualCount = 0

  //   val correctCount = for { x <- validExprs; y <- validExprs } yield {
  //     val bx = baseline(x)
  //     val by = baseline(y)
  //     val rx = results(x)
  //     val ry = results(y)

  //     // this is just for statistics:
  //     if (rx == ry) {
  //       fitnessFncEqualCount += 1
  //     }

  //     // not sure how often this is actually the case, but in that case
  //     // we don't care which expression is picked
  //     if (bx == by) {
  //       1
  //     } else if (bx < by) {
  //       if (rx < ry) 1 else 0

  //     } else { // by < bx
  //       if (ry < rx) 1 else 0
  //     }
  //   }
  //   val correct = correctCount.foldLeft(0)(_ + _)

  //   val total = validExprs.size
  //   val fraction = correct / (total * total).toDouble
  //   val time = (System.currentTimeMillis - start)
  //   reporter.info(f"correct $name: $fraction%1.4f in $time ms (analyzed # exprs: $total)")
  //   (fraction, time, fitnessFncEqualCount)
  // }
}