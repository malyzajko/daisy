// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import daisy.lang.TreeOps
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
    rangeFromInterval: Interval => T): (T, Map[(Expr, PathCond), T]) = {

    val intermediateRanges: CachingMap[(Expr, PathCond), T] = new CachingMap[(Expr, PathCond), T]

    for ((id, range) <- initValMap){
      intermediateRanges.put((Variable(id), emptyPath), range)
    }

    def eval(e: Expr, p: PathCond): T = intermediateRanges.getOrAdd((e, p), {

      case (RealLiteral(r), path) => rangeFromInterval(Interval(r))

      case (Plus(lhs, rhs), path) => eval(lhs, path) + eval(rhs, path)

      case (Minus(lhs, rhs), path) => eval(lhs, path) - eval(rhs, path)

      case (Times(lhs, rhs), path) => eval(lhs, path) * eval(rhs, path)

      case (FMA(fac1, fac2, sum), path) => eval(fac1, path) * eval(fac2, path) + eval(sum, path)

      case (Division(lhs, rhs), path) => eval(lhs, path) / eval(rhs, path)

      //      case Pow(t, n) => eval(t) ^ eval(n)

      case (IntPow(b, n), path) => eval(b, path) ^ n

      case (UMinus(t), path) => - eval(t, path)

      case (Sqrt(t), path) => eval(t, path).squareRoot

      case (Sin(t), path) => eval(t, path).sine

      case (Cos(t), path) => eval(t, path).cosine

      case (Tan(t), path) => eval(t, path).tangent

      case (Exp(t), path) => eval(t, path).exp

      case (Log(t), path) => eval(t, path).log

      case x @ (Let(id, value, body), path) =>
        // TODO: shouldn't the variables be added with empty path?
        // id's should be enough to distinguish them
        intermediateRanges.put((Variable(id), emptyPath), eval(value, path))
        eval(body, path)

      case (Variable(id), path) =>
        val range = intermediateRanges.getOrElse((Variable(id), emptyPath),
          throw new Exception("Unknown variable: " + id)) // needed when constants are pulled out by ConstantTransformerPhase

        // if we are using SMTRange, we can constrain the variable with the path condition
        // currently without effect for IA and AA

        // we need constraints on all the free variables in the path
        val pathVars = lang.TreeOps.allVariablesOf(And.make(path))
        val varsRanges = pathVars.map(id =>
          (id -> intermediateRanges((Variable(id), emptyPath))))
        val varsConstrs = varsRanges.flatMap(x =>
          SMTRange.toConstraints(Variable(x._1), x._2.toInterval))

        range.addConstraint(path.toSet ++ varsConstrs)

      case (IfExpr(cond, thenn, elze), path) =>
        //TODO handle condition (e.g. for x-c <= a case)
        // do the IF branch
        //extractUpdRanges(formatCondition(cond), path, intermediateRanges, rangeFromInterval, true)
        val tmpThen = eval(thenn, path :+ cond)

        // do the ELSE branch
        val negCond = TreeOps.negate(cond)
        //extractUpdRanges(opposite, path,  intermediateRanges, rangeFromInterval, false)
        val tmpElse = eval(elze, path :+ negCond)
        // take a joint interval
        rangeFromInterval(
          Interval(Rational.min(tmpThen.toInterval.xlo, tmpElse.toInterval.xlo),
            Rational.max(tmpThen.toInterval.xhi, tmpElse.toInterval.xhi)))

      case _ =>
        throw new Exception("Not supported")
    })
    val res = eval(expr, emptyPath)
    (res, intermediateRanges.toMap)
  }

  // private def formatCondition(e: Expr): Expr = e match {
  //   case GreaterThan(x, y) => LessEquals(y, x)
  //   case GreaterEquals(x, y) => LessEquals(y, x)
  //   case LessThan(x, y) => LessEquals(x, y)
  //   case LessEquals(x, y) => LessEquals(x, y)
  //   case And(ls) => Or(ls.map(formatCondition))
  //   case Or(ls) => And(ls.map(formatCondition))
  //   case x => x
  // }

  // private def extractUpdRanges[T <: RangeArithmetic[T]](e: Expr, path: PathCond, intRanges: CachingMap[(Expr, PathCond), T], rangeFromInterval: Interval => T, pathPostfix: Boolean): Unit = e match {
  //   case Lambda(args, body) => extractUpdRanges(body, path, intRanges, rangeFromInterval, pathPostfix)
  //   case And(es) => es.foreach(extractUpdRanges(_, path, intRanges, rangeFromInterval, pathPostfix)) // TODO what if conditions are for the same var?

  //   // UPD lower bound
  //   case LessEquals(RealLiteral(r), Variable(id)) if intRanges.contains((Variable(id), path)) =>
  //     val oldRange = intRanges((Variable(id), path))
  //     if (oldRange.toInterval.xlo < r) {
  //       val updRange = Interval(r, oldRange.toInterval.xhi)
  //       intRanges.put((Variable(id), path :+ pathPostfix), rangeFromInterval(updRange))
  //     }

  //   // UPD upper bound
  //   case LessEquals(Variable(id), RealLiteral(r)) if intRanges.contains((Variable(id), path)) =>
  //     val oldRange = intRanges((Variable(id), path))
  //     if (r < oldRange.toInterval.xhi) {
  //       val updRange = Interval(oldRange.toInterval.xlo, r)
  //       intRanges.put((Variable(id), path :+ pathPostfix), rangeFromInterval(updRange))
  //     }

  //   // UPD bounds when const < var or var < const
  //   case LessEquals(Variable(l), Variable(r)) if intRanges.contains(Variable(l), emptyPath) && intRanges.contains(Variable(r), emptyPath)=>
  //     val leftRange = intRanges.getOrElse((Variable(l), path), intRanges(Variable(l), emptyPath))
  //     val rightRange = intRanges.getOrElse((Variable(r), path), intRanges(Variable(r), emptyPath))

  //     if (rightRange.toInterval.isPointRange) {
  //       // x < 0
  //       if (rightRange.toInterval.xlo < leftRange.toInterval.xhi) {
  //         val updRange = Interval(leftRange.toInterval.xlo, rightRange.toInterval.xhi)
  //         intRanges.put((Variable(l), path :+ pathPostfix), rangeFromInterval(updRange))
  //       }
  //     } else if (leftRange.toInterval.isPointRange) {
  //       // 0 < x
  //       if (leftRange.toInterval.xlo > rightRange.toInterval.xlo) {
  //         val updRange = Interval(leftRange.toInterval.xlo, rightRange.toInterval.xhi)
  //         intRanges.put((Variable(r), path :+ pathPostfix), rangeFromInterval(updRange))
  //       }
  //     }

  //   case x => ; // if condition is Or(...) do nothing TODO: probably split into two conditions and analyse for them separately
  // }

}
