// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package lang

import scala.collection.immutable.Seq

import Trees._
import Constructors._

object Extractors {

  object ArithOperator {

    def unapply(expr: Expr): Option[(Seq[Expr], (Seq[Expr]) => Expr)] = expr match {
      case UMinus(t) =>
        Some((Seq(t), (es: Seq[Expr]) => UMinus(es.head)))
      case Plus(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Plus(es(0), es(1)))
      case Minus(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Minus(es(0), es(1)))
      case Times(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Times(es(0), es(1)))
      case FMA(t1, t2, t3) =>
        Some(Seq(t1,t2,t3), (es: Seq[Expr]) => FMA(es(0), es(1), es(2)))
      case Division(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Division(es(0), es(1)))
//      case Pow(t1, t2) =>
//        Some(Seq(t1, t2), (es: Seq[Expr]) => Pow(es(0), es(1)))
      case IntPow(t1, n) =>
        Some(Seq(t1), (es: Seq[Expr]) => IntPow(es(0), n))
      case Sqrt(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Sqrt(es.head)))
      case Sin(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Sin(es.head)))
      case Cos(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Cos(es.head)))
      case Tan(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Tan(es.head)))
      case Exp(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Exp(es.head)))
      case Log(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Log(es.head)))

      case _ =>
        None
    }
  }

  object Operator {

    // IMPROVEMENT: in Leon there are simplifications included here
    // they are not currently taken over, because we would be changing
    // the tree behind the scenes and it may break other stuff
    def unapply(expr: Expr): Option[(Seq[Expr], (Seq[Expr]) => Expr)] = expr match {
      /* Unary operators */
      case Not(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Not(es.head)))
      case UMinus(t) =>
        Some((Seq(t), (es: Seq[Expr]) => UMinus(es.head)))
      case Plus(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Plus(es(0), es(1)))
      case Minus(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Minus(es(0), es(1)))
      case Times(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Times(es(0), es(1)))
      case FMA(t1, t2, t3) =>
        Some(Seq(t1,t2,t3), (es: Seq[Expr]) => FMA(es(0), es(1), es(2)))
      case Division(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Division(es(0), es(1)))
//      case Pow(t1, t2) =>
//        Some(Seq(t1, t2), (es: Seq[Expr]) => Pow(es(0), es(1)))
      case IntPow(t1, n) =>
        Some(Seq(t1), (es: Seq[Expr]) => IntPow(es(0), n))
      case Sqrt(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Sqrt(es.head)))
      case Sin(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Sin(es.head)))
      case Cos(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Cos(es.head)))
      case Tan(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Tan(es.head)))
      case Exp(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Exp(es.head)))
      case Log(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Log(es.head)))
      case RightShift(t, by) =>
        Some((Seq(t), (es: Seq[Expr]) => RightShift(es.head, by)))
      case LeftShift(t, by) =>
        Some((Seq(t), (es: Seq[Expr]) => LeftShift(es.head, by)))
      case Downcast(e, newType) =>
        Some(Seq(e), (es: Seq[Expr]) => Downcast(es.head, newType))

      case Lambda(args, body) =>
        Some((Seq(body), (es: Seq[Expr]) => Lambda(args, es.head)))
      case Equals(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Equals(es(0), es(1)))
      case Implies(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => Implies(es(0), es(1)))
      case AbsError(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => AbsError(es(0), es(1)))
      case LessThan(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => LessThan(es(0), es(1)))
      case GreaterThan(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => GreaterThan(es(0), es(1)))
      case LessEquals(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => LessEquals(es(0), es(1)))
      case GreaterEquals(t1, t2) =>
        Some(Seq(t1, t2), (es: Seq[Expr]) => GreaterEquals(es(0), es(1)))
      case Let(binder, e, body) =>
        Some(Seq(e, body), (es: Seq[Expr]) => Let(binder, es(0), es(1)))
      case Require(pre, body) =>
        Some(Seq(pre, body), (es: Seq[Expr]) => Require(es(0), es(1)))
      case Ensuring(body, post) =>
        Some(Seq(body, post), (es: Seq[Expr]) => Ensuring(es(0), es(1)))
      case Assert(const, oerr, body) =>
        Some(Seq(const, body), (es: Seq[Expr]) => Assert(es(0), oerr, es(1)))

      /* Other operators */
      case fi@FunctionInvocation(fdId, params, args, retTpe) =>
        Some((args, FunctionInvocation(fdId, params, _, retTpe)))

      case And(args) => Some((args, and))
      case Or(args) => Some((args, or))
      case IfExpr(cond, thenn, elze) => Some(Seq(cond, thenn, elze), (es: Seq[Expr]) => IfExpr(es(0), es(1), es(2)))

      /* Terminals */
      case t: Terminal => Some(Seq[Expr](), (_: Seq[Expr]) => t)

      /* Expr's not handled here should implement this trait */
      // case e: Extractable =>
      //  e.extract
    }
  }

}