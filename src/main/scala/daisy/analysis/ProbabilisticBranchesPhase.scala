// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.collection.immutable.Seq

import lang.Identifiers._
import tools._
import Rational.{zero => rzero, one => rone, _}
import lang.Trees._
import solvers.Solver
import FinitePrecision._


// Phase for Probabilistic Analysis
object ProbabilisticBranchesPhase extends DaisyPhase with DynamicEvaluators with Subdivision with tools.RoundoffEvaluators
  with tools.Taylor with tools.RangeEvaluators {
  override val name = "Probabilistic"
  override val shortName = "probabilistic"
  override val description = "probabilistic analysis"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringOption(
      "thresholdFile",
      """File with thresholds.
        The format is the following:
        function_name_1=threshold1
        function_name_2=threshold2
        .....
        function_name_i=threshold_i"""),
    // Number of DS subdivision
    NumOption("dsSubDiv", 4, "Number of DS subdivision"),
    NumOption("outerDiv", 8000, "Number of outer interval subdivision"),
    FlagOption( "gaussian", "Gaussian input distribution, otherwise the inputs are considered to be uniform"),
    FlagOption( "dependent", "The inputs are dependent"),
    FlagOption( "derivativeGuided", "Derivative guided outer subdivision for uniform inputs, compute propagation errors with derivatives")
  )
  implicit val debugSection = DebugSectionAnalysis

  def rat(d: Double) = Rational.fromReal(d)

  val noiseLowerLimit = rat(-1.0)
  val noiseUpperLimit = rat(1.0)
  val timeFormat = new java.text.SimpleDateFormat("HH:mm-dd-MM")
  val timestamp: String = timeFormat.format(new java.util.Date())

  override def runPhase(ctx: Context, prg: Program): (Context, Program) =  {
    val reporter = ctx.reporter
    if (ctx.option[Option[String]]("thresholdFile").isEmpty) {
      reporter.fatalError("No input file is provided")
    }
    else {
      val Some(file) = ctx.option[Option[String]]("thresholdFile")
      val threshold = parseThresholdFile(file)
      val dependency = ctx.hasFlag("dependent")
      val uniformInput = ctx.hasFlag("gaussian") // Flag for Gaussian Distribution
      val derivative = ctx.hasFlag("derivativeGuided") // Flag for derivative guided outer subdivision
      val numDSSubDiv = ctx.option[Long]("dsSubDiv")
      val outerSubDiv = ctx.option[Long]("outerDiv")
      val prec = ctx.option[Precision]("precision")


      for (fnc <- functionsToConsider(ctx, prg)) {
        val fncId = fnc.id
        val body = fnc.body.get
        ctx.reporter.info("evaluating " + fncId + "...")
        val inputRanges = ctx.specInputRanges(fncId)
        val thres = Rational.fromString(threshold(fncId.toString))
        val inputErrors = ctx.specInputErrors(fnc.id)
        val (rndoffError, _) = uniformRoundoff_SMT_AA(body, inputRanges, inputErrors, fnc.precondition.get, prec,
            trackRoundoffErrors = true, approxRoundoff = false)
        val criticalInterval = Interval((thres - rndoffError), (thres + rndoffError))
        if (derivative) {
          val inlinedBody = inline(body) //inline the function body
          // compute Lipschitz constants
          val lipschitzConsts: Map[Identifier, Rational] = fnc.params.map( param => {
            val id = param.id
            val derivative = easySimplify(getPartialDerivative(inlinedBody, id))
            val lipschitzConst = Interval.maxAbs(evalRange[Interval](derivative, inputRanges, Interval.apply)._1)
            (id -> lipschitzConst)
          }).toMap

          //  sort the Lipschitz constants from smallest to largest ...
          val idSortedByLipschitz = lipschitzConsts.toList.sortBy(_._2).map(_._1)
          val divLimitBounds: Seq[Int] = fnc.params.length match {
            case 1 => Seq(8000)
            case 2 => Seq(60, 133)
            case 3 => Seq(30, 20, 13)
            case 4 => Seq(18, 12, 7, 5)
            case 5 => Seq(10, 8, 6, 4, 4)
            case 7 => Seq(2, 3, 3, 4, 4, 5, 5)
            case 9 => Seq(2, 2, 2, 2, 3, 3, 3, 4, 4)
          }
          // ... so that the smaller ones get less subdivisions
          val divLimits: Map[Identifier, Int] = idSortedByLipschitz.zip(divLimitBounds).toMap
          val subIntervals = getIntervalSubdivisionCustom(inputRanges, divLimits).toList
          val wrongPathProb = probWithSubdiv(body, subIntervals, criticalInterval, numDSSubDiv.toInt, dependency)
          ctx.reporter.info(s"Function:" + fncId + " Wrong Path Probability:" + wrongPathProb)
        }
        else {
          val divLimit: Int = math.floor(math.pow(outerSubDiv.toInt, 1.0/(fnc.params.length)) + 0.01).toInt // 0.01 for roundoff
          val supportNormalized = Interval(- rone, rone).divide(numDSSubDiv.toInt)

          if (uniformInput) { // inputs are uniformly distributed
            val subIntervals: Seq[Map[Identifier, Interval]] = getIntervalSubdivision(inputRanges, divLimit).toList
            val wrongPathProb = probWithSubdiv(body, subIntervals, criticalInterval, numDSSubDiv.toInt, dependency)
            ctx.reporter.info(s"Function:" + fncId + " Wrong Path Probability:" + wrongPathProb)

          }
          else { // inputs have gaussian distribution
            val subIntervals: Seq[Map[Identifier, (Interval, Rational, DSInterval)]] =
            genericCartesianProduct[(Interval, Rational, DSInterval)](srcCartesian(inputRanges, numDSSubDiv.toInt,divLimit))
            val wrongPathProb = probWithSubdivNormal(body, subIntervals, criticalInterval, dependency)
            ctx.reporter.info(s"Function:" + fncId + " Wrong Path Probability:" + wrongPathProb)
          }
        }
      }
    }
    (ctx, prg)
  }

  // creates the threshold map
  private def parseThresholdFile(f: String): Map[String, String] = {
    val pairs = for (line <- io.Source.fromFile(f).getLines)
    yield {
      val split = line.split("=").toList
      val fncName = split.head
      val thres = split.tail.head
      (fncName -> thres)}
    pairs.toMap
  }

  // returns the upper and lower bound on the probability of intersection, and total prob
  def computeIntersection(ds: DSInterval, criticalInterval: Interval): (Rational, Rational, Rational) = {
    val criticalLow = criticalInterval.xlo
    val criticalHigh = criticalInterval.xhi
    var totalProb = Rational.zero
    var wrongPathProbHigh = Rational.zero
    var wrongPathProbLow = Rational.zero
    // Compute intersection with critical interval
    for ((intrvl, weight) <- ds.dsi) {
      totalProb += weight
      // intrvl intersects with the critical interval
      if (intrvl.xlo <= criticalHigh && criticalLow <= intrvl.xhi) {
        wrongPathProbHigh += weight
        if (criticalInterval.includes(intrvl)) {
          wrongPathProbLow += weight
        }
      } else {
      }
    }
    (wrongPathProbHigh, wrongPathProbLow, totalProb)
  }

  def genericCartesianProduct[T](a: Map[Identifier, Seq[T]]): Seq[Map[Identifier, T]] = {
    def product(a: List[(Identifier, Seq[T])]): Seq[Map[Identifier, T]] =
      a match {
        case (name, values) :: tail =>
          for {
            result <- product(tail)
            value <- values
          } yield Map(name -> value).++(result)
        case Nil => Seq(Map.empty)
      }
    product(a.toList)
  }

    // Computes the total probability for normal innitial probability
  def probWithSubdivNormal(body: Expr, subIntervals: Seq[Map[Identifier, (Interval, Rational, DSInterval)]],
    criticalInterval: Interval, dependent: Boolean): Rational = {
    // compute wrongPathProb and weight for each subinterval
    val res = subIntervals.map({ subInput: Map[Identifier, (Interval, Rational, DSInterval)] =>
      // makeshift timeout without interrupting GLPK and corrupting memory
      //getting the weights for all variables
      val allWeights = subInput.map({ case (id, (_, weight, _)) => weight })
      // weight of the carthesian product
      val subInputWeight = allWeights.fold(rone)({ case (product, next) => product * next })

      val inputConstr: Seq[Expr] = subInput.flatMap({
        case (id, (range, _, _)) =>
          Seq(LessEquals(RealLiteral(range.xlo), Variable(id)),
            LessEquals(Variable(id), RealLiteral(range.xhi)))
        }).toList

      val outputConstr = Seq(
        LessEquals(RealLiteral(criticalInterval.xlo), body),
        LessEquals(body, RealLiteral(criticalInterval.xhi)))
      val solverQuery = And(inputConstr ++ outputConstr)

      val resSolver = Solver.checkSat(solverQuery)

      resSolver match {
        case Some(false) =>  // UNSAT
            // this subdomain cannot reach the criticalPath, so wrong-path probability is zero
            (Rational.zero, subInputWeight)
          case _ =>
            // Run probabilistic analysis
            var dsAffineInput = collection.immutable.Map[Identifier, DSAffineForm]()
            if (!dependent) {
              dsAffineInput = subInput.mapValues({ case (range, _, noise) => DSAffineForm(range, noise) })
            } else {
              dsAffineInput = DSAffineForm.applyInputDependencyOnGaussian(subInput)
            }
            val dsAffineOutput = evalDSAffine(body, dsAffineInput)

            val outputProb = dsAffineOutput.toDSStruct
            val (wrongPathProbHigh, _, totalProb) = computeIntersection(outputProb, criticalInterval)

            (wrongPathProbHigh * subInputWeight, totalProb * subInputWeight)
        }
      // end subIntervals.map
    }).unzip

    val totalWrongProbHigh = res._1.foldLeft(Rational.zero)({case (sum, next) => sum + next})
    val totalProb = res._2.foldLeft(Rational.zero)({case (sum, next) => sum + next})
    totalWrongProbHigh
  }

  def probWithSubdiv(body: Expr, subIntervals: Seq[Map[Identifier, Interval]], criticalInterval: Interval,
    numDSSubdiv: Int, dependent: Boolean): Rational = {
    val noise = DistributionGenerators.generateUniform(Rational.fromReal(-1.0), Rational.fromReal(1.0), numDSSubdiv)
    // compute wrongPathProb and totalProb for each subinterval
    val res = subIntervals.map({ subInputRanges =>
      // don't do any computation, if this subdomain cannot reach the critical interval
      val inputConstr: Seq[Expr] = subInputRanges.flatMap({
        case (id, range) =>
          Seq(LessEquals(RealLiteral(range.xlo), Variable(id)),
            LessEquals(Variable(id), RealLiteral(range.xhi)))
        }).toList

      val outputConstr = Seq(
        LessEquals(RealLiteral(criticalInterval.xlo), body),
        LessEquals(body, RealLiteral(criticalInterval.xhi)))
      val solverQuery = And(inputConstr ++ outputConstr)

      val resSolver = Solver.checkSat(solverQuery)
      resSolver match {
        case Some(false) =>  // UNSAT
            // this subdomain cannot reach the criticalPath, so wrong-path probability is zero
            (Rational.zero, Rational.one)

          case _ =>
            // it's satisfiable, so let's do the work
            // Run probabilistic analysis
            var dsAffineInput = collection.immutable.Map[Identifier, DSAffineForm]()
            if (!dependent) {
              dsAffineInput = subInputRanges.mapValues(DSAffineForm(_, noise))
            } else {
              dsAffineInput = DSAffineForm.applyInputDependency(subInputRanges, noise)
            }

            val dsAffineOutput = evalDSAffine(body, dsAffineInput)
            val outputProb = dsAffineOutput.toDSStruct
            val (wrongPathProbHigh: Rational, _, totalProb: Rational) = computeIntersection(
              outputProb, criticalInterval)


            (wrongPathProbHigh, totalProb)
        }
    }).unzip

    // for uniform initial probability, the total probability computation is easy
    val totalWrongProbHigh = res._1.foldLeft(Rational.zero)({case (sum, next) => sum + next}) / (subIntervals.size)
    val totalProb = res._2.foldLeft(Rational.zero)({case (sum, next) => sum + next}) / (subIntervals.size)
    totalWrongProbHigh
  }

  def srcCartesian(inputRanges: Map[Identifier, Interval], numDSSubdiv: Int, divLimit: Int): Map[Identifier, Seq[(Interval, Rational, DSInterval)]] = {
    val supportNormalized = Interval(- rone, rone).divide(numDSSubdiv)
    inputRanges.map({
      case (id, interval) if (interval.xlo.equals(interval.xhi)) =>
        val noise = DistributionGenerators.generateStandardNormalCase1(noiseLowerLimit, noiseUpperLimit, numDSSubdiv)
        (id -> Seq( (interval, rone, noise) ))

      case (id, interval) =>
              // there is something to subdivide
        val numTotalSubdivs = numDSSubdiv * divLimit
        val noiseTotal: DSInterval = DistributionGenerators.generateStandardNormalCase1(noiseLowerLimit, noiseUpperLimit, numTotalSubdivs)

              // get slices of the noise of size inner subdiv
        val outerSubdiv: List[DSInterval] = {
          val outerDiv = new scala.collection.mutable.ListBuffer[DSInterval]
          val innerSlice = noiseTotal.dsi.grouped(numDSSubdiv).toList
          innerSlice.foreach(i => outerDiv += DSInterval(i))
          outerDiv.toList
        }
        assert(outerSubdiv.length == divLimit)  // we got the correct number of outer subdivisions

        val outerSubdivRanges = interval.divide(divLimit)

              // match each noise with the corresponding outer interval
        (id -> outerSubdiv.zip(outerSubdivRanges).map({
      case (noise, outerInterval) =>
                  // normalize the noise to have support [-1, 1] and total weight 1
                  // for the support [-1, 1], it's easy as we know how many pieces it was subdivided to
        val (_, weights: Seq[Rational]) = noise.dsi.unzip
                  // total weight of this slice
        val weightSum: Rational = weights.fold(rzero)({case (sum, weight) => sum + weight })
        val weightsNormalized = weights.map( w => w / weightSum )

        (outerInterval, weightSum, DSInterval(supportNormalized.zip(weightsNormalized)))
      }))
    }) // end cartesian
  }

  private def inline(e: Expr, replacements: Map[Expr, Expr] = Map()): Expr = e match {
    case Let(id, value, body) =>
      val newValue = lang.TreeOps.replace(replacements)(value)
      inline(body, replacements + (Variable(id) -> newValue))
    case _ =>
      lang.TreeOps.replace(replacements)(e)
  }

}
