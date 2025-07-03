// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import daisy.lang.Identifiers.Identifier
import daisy.lang.TreeOps._
import daisy.lang.Trees._
import daisy.tools.FinitePrecision.Float64
import daisy.tools.Interval._
// TODO: can we do without the breaks?
import scala.collection.immutable.Map
import scala.collection.parallel.CollectionConverters._

/**
 * Trait with collections of methods to perform Taylor simplifications
 */
trait Taylor extends DeltaAbstractionUtils with RangeEvaluators {

  var listFailed: List[Map[Identifier, Interval]] = List.empty

  // TODO: this can be replaced by reduceOption(Rational.max)
  implicit val optionAbsOrdering = new Ordering[Option[Rational]]{
    override def compare(x: Option[Rational], y: Option[Rational]): Int = {
      if (x.isDefined) {
        if (y.isDefined) {
          Rational.abs(x.get).compare(Rational.abs(y.get))
        } else {
          1  // y undefined Some(x) > None
        }
      } else {
        if (y.isDefined) {
          -1 // x undef None < Some(y)
        } else {
          0  // both undefined None = None
        }
      }
    }
  }


  /**
   * Computes partial derivative w.r.t. passed parameter
   * @param e expression for which derivative is computed
   * @param wrt Delta id w.r.t. which derivative is computed
   * @return expression
   */

  /**
   * Computes partial derivatives w.r.t. each delta in the relative error expression
   * @param e relative error expression
   * @return set of tuples :
   *         (partial derivative,
   *         id of Delta w.r.t. which this derivative has been computed)
   */
  def getDerivative(e: Expr): Seq[(Expr, Identifier)]  = {
    // we only compute partial derivatives with respect to deltas
    var taylorSeries: Seq[(Expr, Identifier)] = Seq.empty
    //ctx.reporter.debug(s"rel error expression $e")
    // val simple = simplify(e)
    epsilonsOf(e).foreach(wrt => {
      val tmp = getPartialDerivative(e, wrt.id) // recurseDerivative(e, wrt.id)
      taylorSeries = taylorSeries :+ (easySimplify(tmp), wrt.id)
    })
    taylorSeries
  }

  /**
   * Computes second derivative and evaluates the range with intervals
   * @param e relative error expression
   * @param intervals map with with intervals for input vars
   * @return max estimated value for the
   */
  def getTaylorRemainder(e: Expr, intervals: Seq[Map[Identifier, Interval]]): Option[Rational] = {

    val simple = easySimplify(e)
    //ctx.reporter.debug("WHAT is INITIALLY in the listFailed " +
    //  listFailed.map(removeDeltasFromMap).map(_.keySet.map(_.globalId)))

    // both deltas and epsilons
    val deltaEps: Set[Variable] = deltasOf(simple) ++ epsilonsOf(simple)
    // todo fix fold or map
    // ctx.reporter.warning("Amount of runs for the first remainder term " + Math.pow(deltaEps.size, 2) * intervals.size)
    val firstTerm = deltaEps.map(wrt => {
      // val wrtSt = System.currentTimeMillis()
      // first derivative
      val tmp = easySimplify(
        getPartialDerivative(simple, wrt.id))
      val tmpValOut = deltaEps.par.map(wrtIn => {
        // val start = System.currentTimeMillis()
        // second derivative
        if (containsVariables(tmp, wrtIn.id)) {
          val tmpIn = easySimplify(Times(wrt, Times(wrtIn, moreSimplify(
            getPartialDerivative(tmp, wrtIn.id)))))

          // ctx.reporter.warning(s"=============WRT $wrt * $wrtIn===============")
          // ctx.reporter.warning(s"Second derivative $tmpIn")
          val tmpVal = tmpIn match {
            case x @ Epsilon(id) => Seq(Some(Float64.machineEpsilon)) // Seq(Some(maxAbs(x.interval)))
            case x @ Delta(id) => Seq(Some(Float64.denormalsError)) // Seq(Some(maxAbs(x.interval)))
            case _ =>
              intervals.par.map(x => {
                val interval = evaluateInterval (tmpIn, x)
                if (interval.isDefined) {
                  Some(maxAbs (interval.get))
                } else {
                  if (! listFailed.contains (x)) {
                    listFailed = listFailed :+ x
                  }
                  None
                }
              })
            }
          // ctx.reporter.warning(s"finished iteration for $wrt, $wrtIn; time:" + (System.currentTimeMillis() - start))
          tmpVal.max(optionAbsOrdering)
        }
        else {
          None
        }
      })
      // remove a dummy entry from the list
      //ctx.reporter.debug(s"before " + listFailed.map(removeDeltasFromMap))
      listFailed = listFailed.filter(x => removeDeltasFromMap(x).keySet == removeDeltasFromMap(intervals.head).keySet)
      //ctx.reporter.debug(s"after " + listFailed.map(removeDeltasFromMap))
      // ctx.reporter.warning(s"done for $wrt in " + (System.currentTimeMillis()-wrtSt))
      tmpValOut.fold(None)(sumOption)
    })

    val deltas: Set[Delta] = deltasOf(simple)
    val secondTerm = deltas.par.map(wrt => {
      val tmp = Times(wrt, Times(wrt,
        getPartialDerivative(simple, wrt.id)))
      val tmpVal = intervals.par.map(x => {
        val interval = evaluateInterval(replaceDeltasWithZeros(tmp), x)
        if (interval.isDefined) {
          Some(maxAbs(interval.get))
        }
        else {
          None
        }
      })
      tmpVal.max(optionAbsOrdering)
    })

    val first = timesOption(Some(Rational(1, 2)), firstTerm.fold(None)(sumOption))
    val second = secondTerm.fold(None)(sumOption)
    sumOption(first, second)
  }

  /**
   * This function checks whether the expression contains a Delta variable,
   * w.r.t. which we compute partial derivative
   *
   * @param e - expression
   * @param wrt - Delta or Epsilon variable
   * @return boolean
   */
  private def containsVariables(e: Expr, wrt: Identifier): Boolean = exists{
    // if we compute w.r.t. this delta or epsilon, it's a var
    case Delta(`wrt`) | Epsilon(`wrt`) => true
  }(e)

  private def evaluateInterval(expr: Expr, intMap: collection.immutable.Map[Identifier, Interval]): Option[Interval] = {
    try {
      // TODO remove it! only for Doppler evaluation!
      // Some(Evaluators.evalSMT(expr,intMap.map({
      // case (id, int) => (id -> SMTRange(Variable(id), int)) })).toInterval)
      //Some(Evaluators.evalInterval(expr,intMap))
      Some(evalRange[Interval](expr, intMap, Interval.apply)._1)
    }
    catch {
      case DivisionByZeroException(_) => None
    }

  }

  def sumOption(a: Option[Rational], b: Option[Rational]): Option[Rational] = {
    //  TODO fix to make more generic            ,op: (Rational, Rational) => Rational
    // fixme is this better?   a.getOrElse(Rational.zero) + b.getOrElse(Rational.zero)
    if (a.isDefined)
      if (b.isDefined) {
        Some(a.get + b.get)
      } else {
        a
      }
    else {
      if (b.isDefined) {
        b
      } else {
        None
      }
    }
  }

  def timesOption(a: Option[Rational], b: Option[Rational]): Option[Rational] = {
    if (a.isDefined && b.isDefined) {
      Some(a.get * b.get)
    } else {
      None
    }
  }
}
