// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.collection.immutable.Seq


import lang.Trees._
import lang.Identifiers._

trait RangeEvaluators {

  // TODO: this function probably subsumes other evaluators, probably remove them?
  //       assert stmt that precision map cannot mix floats and fps
  // regression tests for mixed-precision and rewriting (separately)
  // TODO: fix fixed-point arithmetic code generation

  /**
   * Evaluates the range of this expression using the given RangeArithmetic
   * and saves the intermediate ranges in a map.
   *
   * TODO: documentation
   *
   * @param  {[type]} Rational [description]
   * @return {[type]}          [description]
   */
  def evalRange[T <: RangeArithmetic[T]](
    expr: Expr,
    initValMap: Map[Identifier, T],
    rangeFromReal: Rational => T): (T, Map[Expr, T]) = {

    // TODO: we could even do easy caching now, since we can just check whether the
    // stuff is already computed...
    var intermediateRanges: Map[Expr, T] = Map.empty

    def eval(e: Expr, valMap: Map[Identifier, T]): T = (e: @unchecked) match {

      case x @ RealLiteral(r) =>
        val range = rangeFromReal(r)
        intermediateRanges += (x -> range)
        range

      case x @ Variable(id) =>
        // we update the map each time we encounter a variable, in principle
        // we only need to do this once though. Perhaps optimise?
        val range = valMap(id)
        intermediateRanges += (x -> range)
        range

      case x @ Plus(lhs, rhs) =>
        val range = eval(lhs, valMap) + eval(rhs, valMap)
        intermediateRanges += (x -> range)
        range

      case x @ Minus(lhs, rhs) =>
        val range = eval(lhs, valMap) - eval(rhs, valMap)
        intermediateRanges += (x -> range)
        range

      case x @ Times(lhs, rhs) =>
        val range = eval(lhs, valMap) * eval(rhs, valMap)
        intermediateRanges += (x -> range)
        range


      case x @ Division(lhs, rhs) =>
        val range = eval(lhs, valMap) / eval(rhs, valMap)
        intermediateRanges += (x -> range)
        range

      case x @ UMinus(t) =>
        val range = - eval(t, valMap)
        intermediateRanges += (x -> range)
        range

      case x @ Sqrt(t) =>
        val range = eval(t, valMap).squareRoot
        intermediateRanges += (x -> range)
        range


      case x @ Let(id, value, body) =>
        val valueRange = eval(value, valMap)
        // TODO: do we need the ranges also of the Let's?
        eval(body, valMap + (id -> valueRange))

    }
    val res = eval(expr, initValMap)
    (res, intermediateRanges)
  }



}