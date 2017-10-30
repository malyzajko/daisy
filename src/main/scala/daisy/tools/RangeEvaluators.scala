// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import daisy.utils.CachingMap
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

    var intermediateRanges: CachingMap[Expr, T] = new CachingMap[Expr, T]

    for ((id, range) <- initValMap){
      intermediateRanges.put(Variable(id), range)
    }

    def eval(e: Expr): T = intermediateRanges.getOrAdd(e, {

      case x @ RealLiteral(r) => rangeFromReal(r)

      case x @ Plus(lhs, rhs) => eval(lhs) + eval(rhs)

      case x @ Minus(lhs, rhs) => eval(lhs) - eval(rhs)

      case x @ Times(lhs, rhs) => eval(lhs) * eval(rhs)

      case x @ FMA(fac1, fac2, sum) => eval(fac1) * eval(fac2) + eval(sum)

      case x @ Division(lhs, rhs) => eval(lhs) / eval(rhs)

      case x @ Pow(t, n) => eval(t) ^ eval(n)

      case x @ UMinus(t) => - eval(t)

      case x @ Sqrt(t) => eval(t).squareRoot

      case x @ Sin(t) => eval(t).sine

      case x @ Cos(t) => eval(t).cosine

      case x @ Tan(t) => eval(t).tangent

      case x @ Exp(t) => eval(t).exp

      case x @ Log(t) => eval(t).log

      case x @ Let(id, value, body) =>
        intermediateRanges.put(Variable(id), eval(value))
        eval(body)

      case Variable(id) => throw new Exception("Unknown variable: " + id)

      case _ =>
        throw new Exception("Not supported")
    })
    val res = eval(expr)
    (res, intermediateRanges.toMap)
  }



}
