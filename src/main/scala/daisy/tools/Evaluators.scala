// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools


import collection.immutable.Map
import lang.Trees._
import lang.Identifiers._

// used by a regression test
object Evaluators {

  def evalAffine(expr: Expr, _valMap: Map[Identifier, AffineForm] = Map.empty): AffineForm = {

    var valMap: Map[Identifier, AffineForm] = _valMap

    def eval(e: Expr): AffineForm = (e: @unchecked) match {

      case Delta(id) => valMap(id)
      case Epsilon(id) => valMap(id)

      case Variable(id) => valMap(id)
      case RealLiteral(r) => AffineForm(r)
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      case Pow(x, n) => eval(x) ^ eval(n)
      case UMinus(x) => - eval(x)
      case Let(id, v, b) =>
        val aform = eval(v)
        valMap += (id -> aform)
        eval(b)

    }
    eval(expr)

  }

  def evalInterval(expr: Expr, _valMap: Map[Identifier, Interval] = Map.empty): Interval = {

    var valMap: Map[Identifier, Interval] = _valMap

    def eval(e: Expr): Interval = (e: @unchecked) match {

      case Delta(id) => valMap(id)
      case Epsilon(id) => valMap(id)

      case Variable(id) => valMap(id)
      case RealLiteral(r) => Interval(r)
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      case Pow(x, n) => eval(x) ^ eval(n)
      case UMinus(x) => - eval(x)
      case Let(id, v, b) =>
        val temp = eval(v)
        valMap += (id -> temp)
        eval(b)

    }
    eval(expr)

  }


  def evalSMT(expr: Expr, _valMap: Map[Identifier, SMTRange] = Map.empty): SMTRange = {

    var valMap: Map[Identifier, SMTRange] = _valMap

    def eval(e: Expr): SMTRange = (e: @unchecked) match {

      case Delta(id) => valMap(id)
      case Epsilon(id) => valMap(id)

      case Variable(id) => valMap(id)
      case RealLiteral(r) => SMTRange(r)
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      case Pow(x, n) => eval(x) ^ eval(n)
      case UMinus(x) => - eval(x)
      case Let(id, v, b) =>
        val temp = eval(v)
        valMap += (id -> temp)
        eval(b)
    }
    eval(expr)

  }
}