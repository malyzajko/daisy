// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import daisy.utils.CachingMap
import lang.Trees._
import lang.Identifiers._

trait RangeEvaluators {

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

    val intermediateRanges: CachingMap[Expr, T] = new CachingMap[Expr, T]

    for ((id, range) <- initValMap){
      intermediateRanges.put(Variable(id), range)
    }

    def eval(e: Expr): T = intermediateRanges.getOrAdd(e, {

      case RealLiteral(r) => rangeFromReal(r)

      case Plus(lhs, rhs) => eval(lhs) + eval(rhs)

      case Minus(lhs, rhs) => eval(lhs) - eval(rhs)

      case Times(lhs, rhs) => eval(lhs) * eval(rhs)

      case FMA(fac1, fac2, sum) =>
        // TODO: this is ugly.
        //  The multiplication could be tightened using SMT queries, which we need for the certificate generation.
        val mult = eval(Times(fac1, fac2))
        mult + eval(sum)

      case Division(lhs, rhs) => eval(lhs) / eval(rhs)

//      case Pow(t, n) => eval(t) ^ eval(n)

      case IntPow(b, n) => eval(b) ^ n

      case UMinus(t) => - eval(t)

      case Sqrt(t) => eval(t).squareRoot

      case Sin(t) => eval(t).sine

      case Cos(t) => eval(t).cosine

      case Tan(t) => eval(t).tangent

      case Exp(t) => eval(t).exp

      case Log(t) => eval(t).log

      case Let(id, value, body) =>
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
