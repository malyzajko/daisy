// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.tools

import daisy.lang.Identifiers.Identifier
import daisy.lang.TreeOps.{easySimplify, getPartialDerivative}
import daisy.lang.Trees.Expr

import scala.collection.mutable.Queue
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq
import scala.collection.{immutable, mutable}

trait IntervalBranchAndBound extends RangeEvaluators {

  type InputConfig = Map[Identifier, MPFRInterval]

  /**
   * Interval branch and bound algorithm to compute
   * the minimum and maximum of given expression
   *
   * @param expr           expression whose interval has to be calculated
   * @param inputConfig    map for input variable and interval
   * @param minInputWidth  minimum input width to stop split step
   * @param minOutputWidth minimum output width to stop split step
   * @param maxSplits      maximum splits allowed with default value as 10^5
   * @return minimum and maximum of expression
   */
  def branchAndBound(expr: Expr, inputConfig: InputConfig,
                     minInputWidth: MPFRFloat, minOutputWidth: MPFRFloat, maxSplits: Long = 10000): MPFRInterval = {

    // to track interval arithmatic bounds
    var lb = MPFRFloat.fromDouble(Double.MinValue)
    var hb = MPFRFloat.fromDouble(Double.MaxValue)

    // To track the highest bound
    var fbestLow = MPFRFloat.fromDouble(Double.MinValue)
    var fbestHigh = MPFRFloat.fromDouble(Double.MinValue)

    // To track the lowest bound
    var lowestUpper = MPFRFloat.fromDouble(Double.MaxValue)
    var lowestLow = MPFRFloat.fromDouble(Double.MaxValue)

    val inputConfigQueue = Queue[InputConfig](inputConfig)

    var i: Long = 0
    while (inputConfigQueue.nonEmpty && i < maxSplits) {
      i += 1
      val input = inputConfigQueue.dequeue()
      val (expressionRange, _) = evalRange[MPFRInterval](expr, input, MPFRInterval.apply)

      fbestLow = MPFRFloat.max(expressionRange.xlo, fbestLow)
      lowestUpper = MPFRFloat.min(expressionRange.xhi, lowestUpper)

      if (i == 1) {
        hb = expressionRange.xhi
        lb = expressionRange.xlo
      }

      if (expressionRange.xhi < fbestLow || expressionRange.xlo > lowestUpper ||
        lowestInputWidth(input) <= minInputWidth
        || expressionRange.width < minOutputWidth) {
        fbestHigh = MPFRFloat.max(fbestHigh, expressionRange.xhi)
        lowestLow = MPFRFloat.min(lowestLow, expressionRange.xlo)
      }

      val (left, right) = splitInputConfig(input)
      inputConfigQueue += left
      inputConfigQueue += right

    }

    // if lower(higher) bound is still Double.MaxValue(Double.MinValue), replace it with IA's lower(higher) bound
    if(lowestLow == MPFRFloat.fromDouble(Double.MaxValue)) {
      lowestLow = lb
    }
    if(fbestHigh == MPFRFloat.fromDouble(Double.MinValue)) {
      fbestHigh = hb
    }
    MPFRInterval(lowestLow, fbestHigh)
  }

  /**
   * Interval branch and bound algorithm to compute
   * the minimum and maximum of given expression in a parallel way using atomic updates
   *
   * @param expr           expression whose interval has to be calculated
   * @param inputConfig    map for input variable and interval
   * @param minInputWidth  minimum input width to stop split step
   * @param minOutputWidth minimum output width to stop split step
   * @param maxSplits      maximum splits allowed with default value as 10^5
   * @return minimum and maximum of expression
   */
  def branchAndBoundInParallel(expr: Expr, inputConfig: InputConfig,
                     minInputWidth: MPFRFloat, minOutputWidth: MPFRFloat, maxSplits: Long = 10000): MPFRInterval = {

      // To track the highest bound
      var fbestLow = MPFRFloat.fromDouble(Double.MinValue)
      var fbestHigh = MPFRFloat.fromDouble(Double.MinValue)

      // To track the lowest bound
      var lowestUpper = MPFRFloat.fromDouble(Double.MaxValue)
      var lowestLow = MPFRFloat.fromDouble(Double.MaxValue)

      val inputConfigQueue = Queue[InputConfig](inputConfig)

      // Synchronized bound updates
      def atomicUpdateBesLow(lower: MPFRFloat): Unit = synchronized {
        fbestLow = MPFRFloat.max(lower, fbestLow)
      }

      def atomicUpdateBesHigh(upper: MPFRFloat): Unit = synchronized {
        fbestHigh = MPFRFloat.max(fbestHigh, upper)
      }

      def atomicUpdateLowestUpper(upper: MPFRFloat): Unit = synchronized {
        lowestUpper = MPFRFloat.min(upper, lowestUpper)
      }

      def atomicUpdateLowestLow(lower: MPFRFloat): Unit = synchronized {
        lowestLow = MPFRFloat.min(lowestLow, lower)
      }

      def atomicEnqueue(left: InputConfig, right: InputConfig): Unit = synchronized {
        inputConfigQueue.enqueue(left)
        inputConfigQueue.enqueue(right)
      }

      def processConfig(inputConfig: InputConfig): Unit = {
        val (expressionRange, _) = evalRange[MPFRInterval](expr, inputConfig, MPFRInterval.apply)
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

      // to track interval arithmatic bounds
      val (range, _) = evalRange[MPFRInterval](expr, inputConfig, MPFRInterval.apply)
      val lb = range.xlo
      val hb = range.xhi

      while (inputConfigQueue.nonEmpty && i < maxSplits) {
        val toProcess = dequeAllOrN(inputConfigQueue, cores)
        i += toProcess.size
        toProcess.par.map(config => processConfig(config))
      }

    // if lower(higher) bound is still Double.MaxValue(Double.MinValue), replace it with IA's lower(higher) bound
    if (lowestLow == MPFRFloat.fromDouble(Double.MaxValue)) {
      lowestLow = lb
    }
    if (fbestHigh == MPFRFloat.fromDouble(Double.MinValue)) {
      fbestHigh = hb
    }
      MPFRInterval(lowestLow, fbestHigh)
    }

  def lowestInputWidth(inputConfig: InputConfig): MPFRFloat = {
    val (_, interval) = inputConfig.minBy(i => i._2.width)
    interval.width
  }

  def splitInputConfig(inputConfig: InputConfig): (InputConfig, InputConfig) = {
    // Split a rectangle along the longest dimension.
    val (id, interval) = inputConfig.maxBy(i => i._2.width)

    val left = inputConfig + (id -> MPFRInterval(interval.xlo, interval.mid))
    val right = inputConfig + (id -> MPFRInterval(interval.mid, interval.xhi))
    (left, right)
  }

  // Dequeue the N elements or all, whatever is smaller
  def dequeAllOrN(queue: mutable.Queue[InputConfig], n: Int): immutable.Seq[InputConfig] = {
    val size = queue.size
    if (size < n) {
      queue.dequeueAll(_ => true)
    } else {
      val firstNConfigs = queue.take(n).toSeq
      queue.dropInPlace(n)
      firstNConfigs
    }
  }

}
