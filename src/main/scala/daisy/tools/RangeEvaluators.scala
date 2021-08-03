// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import daisy.lang.TreeOps
import daisy.utils.CachingMap
import lang.Trees._
import lang.Identifiers._
import solvers.Solver

trait RangeEvaluators {

  // TODO: make reporter and require clients to implement?
  var _reporter = new DefaultReporter(Set())

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

      case (Asin(t), path) => eval(t, path).arcsine

      case (Acos(t), path) => eval(t, path).arccosine

      case (Atan(t), path) => eval(t, path).arctangent

      case (Exp(t), path) => eval(t, path).exp

      case (Log(t), path) => eval(t, path).log

      case (Approx(original, t, _, _, _, _), path) => eval(original, path)

      case x @ (Let(id, value, body), path) =>
        // variables are added with empty path, their id's are enough to distinguish them
        intermediateRanges.put((Variable(id), emptyPath), eval(value, path))
        eval(body, path)

      case (x @ Variable(id), path) =>
        // get initial range
        val initRange = intermediateRanges.getOrElse((Variable(id), emptyPath),
          throw new Exception("Unknown variable: " + id)) // needed when constants are pulled out by ConstantTransformerPhase

        val pathVars = lang.TreeOps.allVariablesOf(And.make(path))

        // we may be able to reduce the range if we are inside a branch
        if (path != emptyPath && pathVars.contains(id)) {
          // we can assume the path is feasible
          val xRange = initRange.toInterval
        
          val varsRanges = pathVars.map(id =>
            (id -> intermediateRanges((Variable(id), emptyPath))))
          val varsConstrs = varsRanges.flatMap(x =>
            SMTRange.toConstraints(Variable(x._1), x._2.toInterval))
          
          val newRange = normalizeCond(And.make(path)) match {

            case LessEquals(Variable(i), y: Terminal) if (i == id) =>
              val yRange = y match {
                case Variable(_) => intermediateRanges((y, emptyPath)).toInterval
                case RealLiteral(c) => Interval(c)
              }
              // if we can tighten range, do so
              if (xRange.xhi > yRange.xhi) {
                rangeFromInterval(Interval(xRange.xlo, yRange.xhi))
              } else {
                initRange
              }

            case LessEquals(y: Terminal, Variable(i)) if (i == id)=>
              val yRange = y match {
                case Variable(_) => intermediateRanges((y, emptyPath)).toInterval
                case RealLiteral(c) => Interval(c)
              }
              // if we can tighten range, do so
              if (xRange.xlo < yRange.xlo) {
                rangeFromInterval(Interval(yRange.xlo, xRange.xhi))
              } else {
                initRange
              }

            // complex condition, use an SMT solver to reduce
            case _ => 
              // TODO: disregards precondition
              val newInterval = SMTRange.tightenBounds(xRange, x, 
                BooleanLiteral(true), path.toSet ++ varsConstrs, 
                SMTRange.lowPrecision, SMTRange.lowLoopThreshold, checkVars = true)
              rangeFromInterval(newInterval)

          }

          // the path condition should be added regardless of how/whether we have reduced the range,
          // we may be able to use it later
          newRange.addConstraint(path.toSet ++ varsConstrs)
          
        } else {
          // empty path
          initRange
        }

      case (IfExpr(cond, thenn, elze), path) =>
      
        // check whether if branch is feasible
        val pathVars = lang.TreeOps.allVariablesOf(And.make(path :+ cond))
        // we need constraints on all the free variables in the path
        val varsRanges = pathVars.map(id =>
          (id -> intermediateRanges((Variable(id), emptyPath))))
        val varsConstrs = varsRanges.flatMap(x =>
          SMTRange.toConstraints(Variable(x._1), x._2.toInterval))
        val solverQuery = And((path :+ cond) ++ varsConstrs)
        
        val tmpThen = Solver.checkSat(solverQuery) match {
          case Some(true) =>
            Some(eval(thenn, path :+ cond))
          case Some(false) =>
            _reporter.warning(s"if branch (${cond}) is infeasible")
            None
          case _ => 
            _reporter.warning(s"cannot tell if if-branch (${cond}) is feasible, continuing anyway")
            Some(eval(thenn, path :+ cond))  
        }

        val negCond = TreeOps.negate(cond)
        val solverQueryNeg = And((path :+ negCond) ++ varsConstrs)
        val tmpElse = Solver.checkSat(solverQueryNeg) match {
          case Some(true) =>
            Some(eval(elze, path :+ negCond))
          case Some(false) =>
            _reporter.warning(s"else branch (${cond}) is infeasible")
            None
          case _ => 
            _reporter.warning(s"cannot tell if else-branch (${cond}) is feasible, continuing anyway")
            Some(eval(elze, path :+ negCond))  
        }

        (tmpThen, tmpElse) match {
          case (Some(thenRes), Some(elseRes)) => 
            // take a joint interval
            rangeFromInterval(
              Interval(Rational.min(thenRes.toInterval.xlo, elseRes.toInterval.xlo),
                Rational.max(thenRes.toInterval.xhi, elseRes.toInterval.xhi)))
          
          case (Some(thenRes), None) => thenRes
          case (None, Some(elseRes)) => elseRes
          case (None, None) => 
            _reporter.error("None of the paths is feasible. Aborting.")
            throw new Exception("Not supported")
        }
        
      case _ =>
        throw new Exception("Not supported")
    })
    val res = eval(expr, emptyPath)
    (res, intermediateRanges.toMap)
  }

  private def normalizeCond(e: Expr): Expr = e match {
    case GreaterThan(x, y) => LessEquals(y, x)
    case GreaterEquals(x, y) => LessEquals(y, x)
    case LessThan(x, y) => LessEquals(x, y)
    case LessEquals(x, y) => LessEquals(x, y)
    case And(ls) => And(ls.map(normalizeCond))
    case Or(ls) => Or(ls.map(normalizeCond))
    case x => x
  }

}
