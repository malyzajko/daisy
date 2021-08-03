// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import lang.Identifiers.Identifier
import tools.Rational

import scala.collection.immutable.Map

/**
 * Trait with collections of methods to perform
 * interval subdivision
 */
trait Subdivision extends DeltaAbstractionUtils {

  def totalNumberOfSubdivisions(divLimits: Map[Identifier, Int]): Int = {
    divLimits.values.fold(1)({
      case (product, next) => product * next
    })
  }

  // div limit for optimization
  def getDivLimit(inputValMap: Map[Identifier, Interval]): Map[Identifier, Int] = {

    // do not count point input intervals
    val (nontrivial, pointIntervals) = inputValMap.partition(_._2.width > Rational.zero)
    // sort the list of input vars by width of intervals
    val nontrivialSorted = nontrivial.toSeq.sortWith(_._2.width > _._2.width)

    val nontrivialSplits: Map[Identifier, Int] = nontrivial.size match {
      // single variable: 16 splits
      case 1 => nontrivial.map({case (key, i) => (key -> 16)}).toMap

      // two variables: split each by 5, total 25 splits
      case 2 => nontrivial.map({case (key, i) => (key -> 5)}).toMap

      // three variables: split each by 3, total 27 splits
      case 3 => nontrivial.map({case (key, i) => (key -> 3)}).toMap

      // four variables: split 3, 2, 2, 2, total 24 splits
      case 4 =>
        nontrivialSorted.tail.map({case (key, i) => (key -> 2)}).toMap +
          (nontrivialSorted.head._1 -> 3)

      // five and more variables, first 5 vars are split by 2, total of 32 splits
      case _ =>
        val firstFive = nontrivialSorted.take(5)
        val rest = nontrivialSorted.drop(5)
        firstFive.map({case (key, i) => (key -> 2)}).toMap ++
          rest.map({case (key, i) => (key -> 1)}).toMap
    }

    nontrivialSplits ++ pointIntervals.map({case (key, i) => (key -> 1)})
  }

  // divLimit specifies for each variables how many subdivisions should be performed
  // does *not* remove deltas
  def getCustomSubintervals(inputValMap: Map[Identifier, Interval], divLimits: Map[Identifier, Int]): Seq[Map[Identifier, Interval]] = {

   val splits: Map[Identifier, Seq[Interval]] = inputValMap.map({ case (key, interval) =>
        val numSplits = divLimits(key)
        if (numSplits > 1) {
          (key -> interval.divide(numSplits))
        } else {
          (key -> Seq(interval))
        }
      })

    val result = cartesianProduct(splits)
    result

  }

  // TODO: the divParameter can go I think
  def getEqualSubintervals(inputValMap: Map[Identifier, Interval], divLimit: Int, divParameter: Int = -1,
    totalOpt: Int = 32): Seq[Map[Identifier, Interval]] = {


    if (divLimit == 0 || divParameter == 0){
      Seq(inputValMap)
    }
    else {
      val split = if (divParameter != -1) {
        divParameter
      } else {
        divLimit
      }
      var srcCartesian: Map[Identifier, Seq[Interval]] = Map.empty

      // sort the list of input vars by width of intervals
      val inputSeq = inputValMap.toSeq.sortWith(_._2 > _._2)
      var subdivided = 0
      var left = removeDeltasFromMap(inputValMap).keys.size - 1
      var counter = 0

      // ctx.reporter.debug(s"amount of vars is " + (left + 1))

      // TODO: check what this does
      // set of vectors. each vector contains the maps for one var
      for (inputVal <- inputSeq) {
        counter = Math.pow(split, subdivided + 1).toInt + left
        // ctx.reporter.warning(s"counter $counter ;var $inputVal")
        if (inputVal._1.isDeltaId || inputVal._1.isEpsilonId || counter > totalOpt) {
          srcCartesian += (inputVal._1 -> Seq(inputVal._2))
          if (!(inputVal._1.isDeltaId || inputVal._1.isEpsilonId)) left = left - 1
        }
        else {
          // ctx.reporter.info(s"took the subdiv branch $inputVal")
          val (id, interval) = inputVal
          val oneVar = if (interval.xlo.equals(interval.xhi)) {
            List(interval)
          } else {
            interval.divide(split)
          }
          srcCartesian += (id -> oneVar)
          subdivided = subdivided + 1
          left = left - 1
        }
      }
      val result = cartesianProduct(srcCartesian)
      //reporter.warning(s"size cartesian " + result.size)
      result
    }
  }

  def cartesianProduct(a: Map[Identifier, Seq[Interval]]): Seq[Map[Identifier, Interval]] = {
    def product(a: List[(Identifier, Seq[Interval])]): Seq[Map[Identifier, Interval]] =
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

  // TODO: this function is probably not needed
  /* def getSubintervals(inputValMap: Map[Identifier, Interval], bodyReal: Expr,
    subdiv: String, divLimit: Int): Seq[Map[Identifier, Interval]] = {
    subdiv match {
      case "simple" => getEqualSubintervals(inputValMap, divLimit)
      case "model" => val result = getModelSubintervals(bodyReal, inputValMap)
        result match {
          case Some(x) => x
          case None => getEqualSubintervals(inputValMap, divLimit)
        }
    }
  } */

  /* def getModelSubintervals(bodyReal: Expr,
    inputValMap: Map[Identifier, Interval]): Option[Seq[Map[Identifier, Interval]]] = {

    val constrs = removeDeltasFromMap(inputValMap).flatMap(x => {
      val (id, interval) = x
      SMTRange.toConstraints(Variable(id), interval)
    })
    val condition = and(constrs.toSeq :_*)
    val solverQuery = And(condition, Equals(zero, bodyReal))
    val model = Solver.getModel(solverQuery)
    ctx.reporter.debug(model)
    model match {
      case Some(tmp) =>
        ctx.reporter.debug(s"true zeros detected at $model")
        val exprs: Map[Identifier, Expr] = variablesOf(bodyReal).map(id => {(id -> model.get(id))}).toMap
        ctx.reporter.debug(s"Model expressions are $exprs")
        val values: Map[Identifier, Interval] = exprs.map(expr =>
        {expr._1 ->
          Evaluators.evalInterval(expr._2, Map.empty).extendBy(Rational.fromReal(0.1))})
        ctx.reporter.warning(s"Model values are $values")
        val newInts = excludeInterval(values, inputValMap)
        Some(newInts)
      case None => ctx.reporter.debug(s"No true zeros in function")
        None
    }

  } */

  /* def excludeInterval(toExclude: Map[Identifier, Interval],
    original: Map[Identifier, Interval]): Seq[Map[Identifier, Interval]] = {
    var result: Seq[Map[Identifier, Interval]] = Seq.empty
    for ((id, interval) <- toExclude) {
      var copy = original
      val origInterval = original(id)
      if (interval.xlo.<(origInterval.xlo))
        if (interval.xhi.>(origInterval.xhi))
        // interval to exclude is greater than original
        // fixme find a better way
          throw new IllegalArgumentException("The resulting interval is empty")
        else
        // lower bound of interval toExclude is smaller than lower bound of originalInterval
        // e.g. exclude [-0.1 ; 0.1] from [0; 10]
        {
          val tmp = Interval(interval.xhi, origInterval.xhi)
          copy = copy + (id -> tmp)
          result = result ++ Set(copy)
        }
      else if (interval.xhi.>(origInterval.xhi))
      // upper bound of interval toExclude is greater than upper bound of originalInterval
      // e.g. exclude [-0.1 ; 0.1] from [-10; 0]
      {
        val tmp = Interval(origInterval.xlo, interval.xlo)
        copy = copy + (id -> tmp)
        result = result ++ Set(copy)
      }
      else
      // general case
      {
        val tmplo = Interval(origInterval.xlo, interval.xlo)
        copy = copy + (id -> tmplo)
        result = result ++ Set(copy)
        val tmphi = Interval(interval.xhi, origInterval.xhi)
        copy = copy + (id -> tmphi)
        result = result ++ Set(copy)
      }
    }
    result
  } */
}


