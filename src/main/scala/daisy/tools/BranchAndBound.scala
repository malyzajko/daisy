// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import daisy.lang.Identifiers.Identifier
import daisy.lang.TreeOps.{easySimplify, getPartialDerivative}
import daisy.lang.Trees.Expr

import scala.collection.immutable.Map
import scala.collection.{immutable, mutable}
import scala.collection.mutable.Queue
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq

trait BranchAndBound extends RangeEvaluators {

  type InputConfig = Map[Identifier, Interval]

  /**
   * Interval branch and bound algorithm to compute
   * the minimum and maximum of given expression
   *
   * @param expr expression whose interval has to be calculated
   * @param inputConfig map for input variable and interval
   * @param minInputWidth minimum input width to stop split step
   * @param minOutputWidth minimum output width to stop split step
   * @param maxSplits maximum splits allowed with default value as 10^5
   * @return minimum and maximum of expression
   */
  def branchAndBound(expr: Expr, inputConfig: InputConfig,
    minInputWidth: Rational, minOutputWidth: Rational, maxSplits: Long = 100000): Interval = {

    // To track the highest bound
    var fbestLow = Rational.fromDouble(Double.MinValue)
    var fbestHigh = Rational.fromDouble(Double.MinValue)

    // To track the lowest bound
    var lowestUpper = Rational.fromDouble(Double.MaxValue)
    var lowestLow = Rational.fromDouble(Double.MaxValue)

    val inputConfigQueue = Queue[InputConfig](inputConfig)

    var i: Long = 0
    while (inputConfigQueue.nonEmpty && i < maxSplits) {
      i += 1
      val input = inputConfigQueue.dequeue()
      val (expressionRange, _) = evalRange[Interval](expr, input, Interval.apply)
      val (lower, upper) = (expressionRange.xlo, expressionRange.xhi)
      fbestLow = Rational.max(lower, fbestLow)
      lowestUpper = Rational.min(upper, lowestUpper)
      if (upper < fbestLow || lower > lowestUpper
        || lowestInputWidth(input) <= minInputWidth || expressionRange.width < minOutputWidth) {
        fbestHigh = Rational.max(fbestHigh, upper)
        lowestLow = Rational.min(lowestLow, lower)
      } else {
        val (left, right) = splitInputConfig(input)
        inputConfigQueue += left
        inputConfigQueue += right
      }
    }
    Interval(lowestLow, fbestHigh)
  }

  /**
   * Interval branch and bound algorithm to compute
   * the minimum and maximum of given expression using
   * priority search based on zero included in the
   * range computed for partially derived expression.
   *
   * @param expr expression whose interval has to be calculated
   * @param inputConfig map for input variable and interval
   * @param minInputWidth minimum input width to stop split step
   * @param minOutputWidth minimum output width to stop split step
   * @param maxSplits maximum splits allowed with default value as 10^5
   * @return minimum and maximum of expression
   */
  def branchAndBoundPD(expr: Expr, inputConfig: InputConfig,
    minInputWidth: Rational, minOutputWidth: Rational, maxSplits: Long = 100000): Interval = {

    // To track the highest bound
    var fbestLow = Rational.fromDouble(Double.MinValue)
    var fbestHigh = Rational.fromDouble(Double.MinValue)

    // To track the lowest bound
    var lowestUpper = Rational.fromDouble(Double.MaxValue)
    var lowestLow = Rational.fromDouble(Double.MaxValue)

    val identifiers = inputConfig.map(_._1)
    val partialDerivations = identifiers.map(id => easySimplify(getPartialDerivative(expr, id)))

    val inputConfigQueue = new mutable.PriorityQueue[InputConfig]()(
      Ordering.by(config => computePriority(partialDerivations, config)))
    inputConfigQueue += inputConfig

    var i: Long = 0
    while (inputConfigQueue.nonEmpty && i < maxSplits) {
      i += 1
      val input: InputConfig = inputConfigQueue.dequeue()
      val (expressionRange, _) = evalRange[Interval](expr, input, Interval.apply)
      val (lower, upper) = (expressionRange.xlo, expressionRange.xhi)
      fbestLow = Rational.max(lower, fbestLow)
      lowestUpper = Rational.min(upper, lowestUpper)
      if (upper < fbestLow || lower > lowestUpper
        || lowestInputWidth(input) <= minInputWidth || expressionRange.width < minOutputWidth) {
        fbestHigh = Rational.max(fbestHigh, upper)
        lowestLow = Rational.min(lowestLow, lower)
      } else {
        val (left, right) = splitInputConfig(input)
        inputConfigQueue += left
        inputConfigQueue += right
      }
    }
    Interval(lowestLow, fbestHigh)
  }

  def distanceToZero(interval: Interval): Rational = {
    if (interval.xlo > Rational.zero){
      interval.mid
    }else{
      Rational.abs(interval.mid)
    }
  }

  def weightForZeroInRange(expr: Expr, inputConfig: InputConfig): Int = {
    val (expressionRange, _) = evalRange[Interval](expr, inputConfig, Interval.apply)
    if(expressionRange.includes(Rational.zero)){
      1
    }else{
      // distance to zero as a penalty
      val dtz = distanceToZero(expressionRange).roundToInt
      -1 * dtz
    }
  }

  def computePriority(partialDerivations: Iterable[Expr], config: InputConfig): Int = {
    val weights = partialDerivations.map(pd => weightForZeroInRange(pd, config))
    weights.sum
  }

  /**
   * Interval branch and bound algorithm to compute
   * the minimum and maximum of given expression in a parallel way using atomic updates
   *
   * @param expr expression whose interval has to be calculated
   * @param inputConfig map for input variable and interval
   * @param minInputWidth minimum input width to stop split step
   * @param minOutputWidth minimum output width to stop split step
   * @param maxSplits maximum splits allowed with default value as 10^5
   * @return minimum and maximum of expression
   */
  def branchAndBoundParAtomic(expr: Expr, inputConfig: InputConfig, minInputWidth: Rational,
    minOutputWidth: Rational, maxSplits: Long = 100000): Interval = {

    // To track the highest bound
    var fbestLow = Rational.fromDouble(Double.MinValue)
    var fbestHigh = Rational.fromDouble(Double.MinValue)

    // To track the lowest bound
    var lowestUpper = Rational.fromDouble(Double.MaxValue)
    var lowestLow = Rational.fromDouble(Double.MaxValue)

    val inputConfigQueue = Queue[InputConfig](inputConfig)

    // Synchronized bound updates
    def atomicUpdateBesLow(lower: Rational): Unit = synchronized { fbestLow = Rational.max(lower, fbestLow)}
    def atomicUpdateBesHigh(upper: Rational): Unit = synchronized { fbestHigh = Rational.max(fbestHigh, upper)}
    def atomicUpdateLowestUpper(upper: Rational): Unit = synchronized { lowestUpper = Rational.min(upper, lowestUpper)}
    def atomicUpdateLowestLow(lower: Rational): Unit = synchronized { lowestLow = Rational.min(lowestLow, lower)}
    def atomicEnqueue(left: InputConfig, right: InputConfig): Unit = synchronized {
      inputConfigQueue.enqueue(left)
      inputConfigQueue.enqueue(right)
    }

    def processConfig(inputConfig: InputConfig): Unit = {
      val (expressionRange, _) = evalRange[Interval](expr, inputConfig, Interval.apply)
      val (lower, upper) = (expressionRange.xlo, expressionRange.xhi)
      atomicUpdateBesLow(lower)
      atomicUpdateLowestUpper(upper)
      if (upper < fbestLow || lower > lowestUpper
        || lowestInputWidth(inputConfig) <= minInputWidth || expressionRange.width < minOutputWidth) {
        atomicUpdateBesHigh(upper)
        atomicUpdateLowestLow(lower)
      } else {
        val (left, right) = splitInputConfig(inputConfig)
        atomicEnqueue(left, right)
      }
    }

    // Number of cores, used to determine how many elements processed in parallel
    val cores = Runtime.getRuntime().availableProcessors()

    var i: Long = 0

    while (inputConfigQueue.nonEmpty && i < maxSplits) {
      val toProcess = dequeAllOrN(inputConfigQueue, cores)
      i += toProcess.size
      toProcess.par.map(config => processConfig(config))
    }
    Interval(lowestLow, fbestHigh)
  }

  // A class to keep and track the bounds
  case class BoundsState(fbestLow: Rational, fbestHigh: Rational, lowestUpper: Rational, lowestLow: Rational)

  /**
   * Interval branch and bound algorithm to compute
   * the minimum and maximum of given expression in a parallel and functional way
   *
   * @param expr expression whose interval has to be calculated
   * @param inputConfig map for input variable and interval
   * @param minInputWidth minimum input width to stop split step
   * @param minOutputWidth minimum output width to stop split step
   * @param maxSplits maximum splits allowed with default value as 10^5
   * @return minimum and maximum of expression
   */
  def branchAndBoundParFunctional(expr: Expr, inputConfig: InputConfig, minInputWidth: Rational,
    minOutputWidth: Rational, maxSplits: Long = 100000): Interval = {

    // To track the bounds in initial bounds
    val maxRational = Rational.fromDouble(Double.MaxValue)
    val minRational = Rational.fromDouble(Double.MinValue)
    var bounds = BoundsState(minRational, minRational, maxRational, maxRational)

    val inputConfigQueue = Queue[InputConfig](inputConfig)

    // Number of cores, used to determine how many elements processed in parallel
    val cores = Runtime.getRuntime().availableProcessors()

    var i: Long = 0

    while (inputConfigQueue.nonEmpty && i < maxSplits) {
      val toProcess = dequeAllOrN(inputConfigQueue, cores)
      i += toProcess.size
      val results = toProcess.par.map(config => processConfig(config, expr, bounds, minInputWidth, minOutputWidth))
      bounds = mergeBoundsAndUpdateQueue(bounds, inputConfigQueue, results)
    }
    Interval(bounds.lowestLow, bounds.fbestHigh)
  }

  def processConfig(inputConfig: InputConfig, expr: Expr, bounds: BoundsState, minInputWidth: Rational,
    minOutputWidth: Rational): (BoundsState, Option[(InputConfig, InputConfig)]) = {

    val (expressionRange, _) = evalRange[Interval](expr, inputConfig, Interval.apply)
    val (lower, upper) = (expressionRange.xlo, expressionRange.xhi)
    val fbestLow = Rational.max(lower, bounds.fbestLow)
    val lowestUpper = Rational.min(upper, bounds.lowestUpper)
    if (upper < fbestLow || lower > lowestUpper
      || lowestInputWidth(inputConfig) <= minInputWidth || expressionRange.width < minOutputWidth) {
      val fbestHigh = Rational.max(bounds.fbestHigh, upper)
      val lowestLow = Rational.min(bounds.lowestLow, lower)
      val newBounds = BoundsState(fbestLow, fbestHigh, lowestUpper, lowestLow)
      (newBounds, None)
    } else {
      val (left, right) = splitInputConfig(inputConfig)
      val newBounds = BoundsState(fbestLow, bounds.fbestHigh, lowestUpper, bounds.lowestLow)
      (newBounds, Some((left, right)))
    }
  }

  def mergeBoundsAndUpdateQueue(initialBounds: BoundsState, inputConfigQueue: Queue[InputConfig],
    results: ParSeq[(BoundsState, Option[(InputConfig, InputConfig)])]): BoundsState = {

    results.foldLeft(initialBounds) { (aggBounds, res) =>
      val currBounds = res._1
      val updatedBounds = BoundsState(
        Rational.max(aggBounds.fbestLow, currBounds.fbestLow),
        Rational.max(aggBounds.fbestHigh, currBounds.fbestHigh),
        Rational.min(aggBounds.lowestUpper, currBounds.lowestUpper),
        Rational.min(aggBounds.lowestLow, currBounds.lowestLow)
      )

      // Insert the splits to the queue
      res._2 match {
        case Some((leftSplit, rightSplit)) => {
          inputConfigQueue.enqueue(leftSplit)
          inputConfigQueue.enqueue(rightSplit)
          updatedBounds
        }
        case None => updatedBounds
      }
    }
  }

  def lowestInputWidth(inputConfig: InputConfig): Rational = {
    val (_, interval) = inputConfig.minBy(i => i._2.width)
    interval.width
  }

  def splitInputConfig(inputConfig: InputConfig): (InputConfig, InputConfig) = {
    // Split a rectangle along the longest dimension.
    val (id, interval) = inputConfig.maxBy(i => i._2.width)

    val left = inputConfig + (id -> Interval(interval.xlo, interval.mid))
    val right = inputConfig + (id -> Interval(interval.mid, interval.xhi))
    (left, right)
  }

  // Dequeue the N elements or all, whatever is smaller
  def dequeAllOrN(queue: mutable.Queue[InputConfig], n: Int): immutable.Seq[InputConfig] = {
    val size = queue.size
    if(size < n){
      queue.dequeueAll(_ => true)
    }else{
      val firstNConfigs = queue.take(n).toSeq
      queue.dropInPlace(n)
      firstNConfigs
    }
  }
}
