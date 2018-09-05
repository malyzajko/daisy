package daisy
package tools

import daisy.lang.Trees._

object Sign extends Enumeration {

  type Sign = Value
  val Positive, Negative, Unknown = Value

  def negate(sign: Sign): Sign = sign match {
    case Positive => Negative
    case Negative => Positive
    case Unknown => Unknown
  }

  def ofRational(x: Rational): Sign =
    if (x < Rational.zero) {
      Negative
    } else {
      Positive
    }

  def ofInterval(iv: Interval): Sign =
    if (iv.xlo < Rational.zero) {
      if (iv.xhi < Rational.zero) {
        Negative
      } else {
        Unknown
      }
    } else {
      Positive
    }

  private def ofAddition(fst: Sign, snd: Sign): Sign = {
    if (fst == Positive) {
      if (snd == Negative) {
        Unknown
      } else {
        snd
      }
    } else if (fst == Negative) {
      if (snd == Negative) {
        Negative
      } else {
        Unknown
      }
    } else {
      Unknown
    }
  }

  private def ofSubtraction(fst: Sign, snd: Sign): Sign = {
    ofAddition(fst, negate(snd))
  }

  private def ofMultiplication(fst: Sign, snd: Sign): Sign = {
    if (fst == Positive) {
      snd
    } else if (fst == Negative) {
      negate(snd)
    } else {
      Unknown
    }
  }

  def ofExpression(expr: Expr, path: PathCond, range: Map[(Expr, PathCond), Interval]): Sign = expr match {
    case RealLiteral(x) => ofRational(x)
    case x @ Variable(_) => ofInterval(range(x, path))
    case UMinus(x) => negate(ofExpression(x, path, range))
    case Plus(x, y) => ofAddition(ofExpression(x, path, range), ofExpression(y, path, range))
    case Minus(x, y) => ofSubtraction(ofExpression(x, path, range), ofExpression(y, path, range))
    case Times(x, y) => ofMultiplication(ofExpression(x, path, range), ofExpression(y, path, range))
    case Division(x, y) => ofMultiplication(ofExpression(x, path, range), ofExpression(y, path, range))
    case IntPow(x, n) =>
      if (n % 2 == 0) {
        Positive
      } else {
        ofExpression(x, path, range)
      }
    case Exp(_) | Sqrt(_)  => Positive
    case _ => Unknown
  }

}
