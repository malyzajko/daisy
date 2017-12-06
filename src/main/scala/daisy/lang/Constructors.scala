// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package lang

import Trees._
import TreeOps._

import scala.collection.immutable.Seq

object Constructors {

  /** $encodingof `&&`-expressions with arbitrary number of operands, and simplified.
   * @see [[lang.Trees.And And]]
   */
  def and(exprs: Expr*): Expr = {
    // mutable
    val flat = exprs.flatMap {
      case And(es) => es
      case o => Seq(o)
    }

    var stop = false
    val simpler = for(e <- flat if !stop && e != BooleanLiteral(true)) yield {
      if (e == BooleanLiteral(false)) {
        stop = true
      }
      e
    }

    simpler match {
      case collection.mutable.Seq()  => BooleanLiteral(true)
      case collection.mutable.Seq(x) => x
      case _      => And(Seq(simpler: _*))  // make immutable Seq
    }
  }

  /** $encodingof `&&`-expressions with arbitrary number of operands as a sequence, and simplified.
   * @see [[purescala.Expressions.And And]]
   */
  // def andJoin(es: Seq[Expr]) = and(es :_*)

  /** $encodingof `||`-expressions with arbitrary number of operands, and simplified.
   * @see [[purescala.Expressions.Or Or]]
   */
  def or(exprs: Expr*): Expr = {
    val flat = exprs.flatMap {
      case Or(es) => es
      case o => Seq(o)
    }

    var stop = false
    val simpler = for(e <- flat if !stop && e != BooleanLiteral(false)) yield {
      if (e == BooleanLiteral(true)) {
        stop = true
      }
      e
    }

    simpler match {
      case collection.mutable.Seq()  => BooleanLiteral(false)
      case collection.mutable.Seq(x) => x
      case _      => Or(Seq(simpler: _*)) // make immutable Seq
    }
  }

  /** $encodingof `&&`-expressions with arbitrary number of operands as a sequence, and simplified.
   * @see [[purescala.Expressions.And And]]
   */
  def andJoin(es: Seq[Expr]): Expr = and(es: _*)

  /** $encodingof `||`-expressions with arbitrary number of operands as a sequence, and simplified.
   * @see [[purescala.Expressions.Or Or]]
   */
  def orJoin(es: Seq[Expr]): Expr = or(es: _*)

  /** $encodingof simplified `!`-expressions .
   * @see [[purescala.Expressions.Not Not]]
   */
  def not(e: Expr): Expr = negate(e)


  /** $encodingof simplified `... + ...` (plus).
   * @see [[purescala.Expressions.Plus Plus]]
   * @see [[purescala.Expressions.BVPlus BVPlus]]
   * @see [[purescala.Expressions.RealPlus RealPlus]]
   */
  def plus(lhs: Expr, rhs: Expr): Expr = (lhs, rhs) match {
    case (IntegerLiteral(bi), _) if bi == 0 => rhs
    case (_, IntegerLiteral(bi)) if bi == 0 => lhs
    case (Int32Literal(0), _) => rhs
    case (_, Int32Literal(0)) => lhs
    case (RealLiteral(r), _) if r == tools.Rational.zero => rhs
    case (_, RealLiteral(r)) if r == tools.Rational.zero => lhs
    // case (IsTyped(_, Int32Type), IsTyped(_, Int32Type)) => BVPlus(lhs, rhs)
    // case (IsTyped(_, RealType), IsTyped(_, RealType)) => RealPlus(lhs, rhs)
    case _ => Plus(lhs, rhs)
  }

  /** $encodingof simplified `... - ...` (minus).
   * @see [[purescala.Expressions.Minus Minus]]
   * @see [[purescala.Expressions.BVMinus BVMinus]]
   * @see [[purescala.Expressions.RealMinus RealMinus]]
   */
  def minus(lhs: Expr, rhs: Expr): Expr = (lhs, rhs) match {
    case (_, IntegerLiteral(bi)) if bi == 0 => lhs
    case (_, Int32Literal(0)) => lhs
    case (IntegerLiteral(bi), _) if bi == 0 => UMinus(rhs)
    // case (IntLiteral(0), _) => BVUMinus(rhs)
    // case (IsTyped(_, Int32Type), IsTyped(_, Int32Type)) => BVMinus(lhs, rhs)
    // case (IsTyped(_, RealType), IsTyped(_, RealType)) => RealMinus(lhs, rhs)
    case _ => Minus(lhs, rhs)
  }

  /** $encodingof simplified `... * ...` (times).
   * @see [[purescala.Expressions.Times Times]]
   * @see [[purescala.Expressions.BVTimes BVTimes]]
   * @see [[purescala.Expressions.RealTimes RealTimes]]
   */
  def times(lhs: Expr, rhs: Expr): Expr = (lhs, rhs) match {
    case (IntegerLiteral(bi), _) if bi == 1 => rhs
    case (_, IntegerLiteral(bi)) if bi == 1 => lhs
    case (IntegerLiteral(bi), _) if bi == 0 => IntegerLiteral(0)
    case (_, IntegerLiteral(bi)) if bi == 0 => IntegerLiteral(0)
    case (Int32Literal(1), _) => rhs
    case (_, Int32Literal(1)) => lhs
    case (Int32Literal(0), _) => Int32Literal(0)
    case (_, Int32Literal(0)) => Int32Literal(0)
    // case (IsTyped(_, Int32Type), IsTyped(_, Int32Type)) => BVTimes(lhs, rhs)
    // case (IsTyped(_, RealType), IsTyped(_, RealType)) => RealTimes(lhs, rhs)
    case _ => Times(lhs, rhs)
  }
}