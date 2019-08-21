// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package lang

import scala.collection.immutable.Seq
import TypeOps._
import Types._
import utils.{Positioned, PrettyPrinter}
import Identifiers._
import tools.FinitePrecision._
import tools.Rational

object Trees {

  abstract class Tree extends Positioned with Serializable {
    override def toString: String = PrettyPrinter(this)
  }

  sealed abstract class Definition extends Tree {

    val id: Identifier

    override def hashCode: Int = id.hashCode
    override def equals(that: Any): Boolean = that match {
      case t: Definition => t.id == this.id
      case _ => false
    }

  }

  /**
   * A ValDef represents a parameter of a [[lang.Trees.FunDef function]].
   */
  case class ValDef(id: Identifier) extends Definition with Typed {
    self: Serializable =>
    val getType = id.getType
  }

  /**
   * Function or field definition.
   */
  case class FunDef(
    id: Identifier,
    returnType: TypeTree,
    params: Seq[ValDef],
    precondition: Option[Expr],
    body: Option[Expr],
    postcondition: Option[Expr],
    isField: Boolean = false) extends Definition

  /**
    A program is currently one (and only one) top-level object with a
    number of function and field definitions.
    We ignore package and import information.
    // TODO: constructor block
   */
  case class Program(id: Identifier, defs: Seq[FunDef]) extends Definition

  /** Represents an expression in Leon. */
  sealed abstract class Expr extends Tree with Typed {
    def Plus(that: Expr): Expr = Trees.Plus(this, that)
    def Minus(that: Expr): Expr = Trees.Minus(this, that)
    def Times(that: Expr): Expr = Trees.Times(this, that)
    def Division(that: Expr): Expr = Trees.Division(this, that)
//    def Pow(that: Expr): Expr = Trees.Pow(this, that)
    def IntPow(n: Int): Expr = Trees.IntPow(this, n)
  }

  /** Trait which gets mixed-in to expressions without subexpressions */
  trait Terminal {
    self: Expr =>
  }

  /** Stands for an undefined Expr, similar to `???` or `null`
   *
   * During code generation, it gets compiled to `null`, or the 0 of the
   * respective type for value types.
   */
  case class NoTree(tpe: TypeTree) extends Expr with Terminal {
    val getType = tpe
  }

  /* Specifications */

  /** Computational errors (unmatched case, taking min of an empty set,
   * division by zero, etc.). Corresponds to the ``error[T](description)``
   * Leon library function.
   * It should always be typed according to the expected type.
   *
   * @param tpe The type of this expression
   * @param description The description of the error
   */
  case class Error(tpe: TypeTree, description: String) extends Expr with Terminal {
    val getType = tpe
  }

  /** Precondition of an [[Expressions.Expr]]. Corresponds to the Leon keyword *require*
   *
   * @param pred The precondition formula inside ``require(...)``
   * @param body The body following the ``require(...)``
   */
  case class Require(pred: Expr, body: Expr) extends Expr {
    val getType = {
      if (pred.getType == BooleanType) {
        body.getType
      } else {
        Untyped
      }
    }
  }

  /** Postcondition of an [[Expressions.Expr]]. Corresponds to the Leon keyword *ensuring*
   *
   * @param body The body of the expression. It can contain at most one [[Expressions.Require]] sub-expression.
   * @param pred The predicate to satisfy. It should be a function whose argument's type can handle the type of the body
   */
  case class Ensuring(body: Expr, pred: Expr) extends Expr {
    require(pred.isInstanceOf[Lambda])

    val getType = pred.getType match {
      case FunctionType(Seq(bodyType), BooleanType) if isSubtypeOf(body.getType, bodyType) =>
        body.getType
      case _ =>
        Untyped
    }
    /** Converts this ensuring clause to the body followed by an assert statement */
    // def toAssert: Expr = {...}
  }

  /** Local assertions with customizable error message
   *
   * @param pred The predicate, first argument of `assert(..., ...)`
   * @param error An optional error string to display if the assert fails. Second argument of `assert(..., ...)`
   * @param body The expression following `assert(..., ...)`
   */
  case class Assert(pred: Expr, error: Option[String], body: Expr) extends Expr {
    val getType = {
      if (pred.getType == BooleanType) {
        body.getType
      } else {
        Untyped
      }
    }
  }

  /** Variable
   * @param id The identifier of this variable
   */
  trait Variable extends Expr with Terminal {
    val id: Identifier
    val getType = id.getType
    override def equals(obj: scala.Any): Boolean = obj match {
      case Variable(tid) => id == tid
      case _ => false
    }
  }

  object Variable {
    def apply(id: Identifier): Variable = ConcreteVariable(id)
    def unapply(arg: Variable): Option[Identifier] = Some(arg.id)
  }

  private case class ConcreteVariable(id: Identifier) extends Variable {
    override def equals(obj: scala.Any): Boolean = super.equals(obj)
  }
  case class Delta(id: Identifier) extends Variable{
    override def equals(obj: scala.Any): Boolean = super.equals(obj)
  }
  case class Epsilon(id: Identifier) extends Variable {
    override def equals(obj: scala.Any): Boolean = super.equals(obj)
  }



  /** $encodingof `val ... = ...; ...`
   *
   * @param binder The identifier used in body, defined just after '''val'''
   * @param value The value assigned to the identifier, after the '''=''' sign
   * @param body The expression following the ``val ... = ... ;`` construct
   * @see [[leon.purescala.Constructors#let purescala's constructor let]]
   */
  case class Let(binder: Identifier, value: Expr, body: Expr) extends Expr {
    val getType = {
      // We can't demand anything sticter here, because some binders are
      // typed context-wise
      if (typesCompatible(value.getType, binder.getType)) {
        body.getType
      } else {
        Untyped
      }
    }
  }

  /* Control flow */

  /** $encodingof  `function(...)` (function invocation) */
  // There is a chicken-n-egg problem here, because at extraction time,
  // we don't have (and can't have?) all the FunDef's ready, if they are to be immutable.
  // Additionally, the fnc body may change, e.g. with SSA transform.
  case class FunctionInvocation(fdId: Identifier, params: Seq[ValDef], args: Seq[Expr],
    returnType: TypeTree) extends Expr {
    // require(fd.params.size == args.size)
    val getType = returnType
  }

  /** $encodingof `if(...) ... else ...` */
  case class IfExpr(cond: Expr, thenn: Expr, elze: Expr) extends Expr {
    val getType = {
      val thenPrec = (thenn.getType: @unchecked) match {
        case FinitePrecisionType(x) => Some(x)
        case _ => None
      }
      val elsePrec = (elze.getType: @unchecked) match {
        case FinitePrecisionType(x) => Some(x)
        case _ => None
      }
      if (thenPrec.isDefined && elsePrec.isDefined)
        FinitePrecisionType(getUpperBound(thenPrec.get, elsePrec.get))
      else
        leastUpperBound(thenn.getType, elze.getType).getOrElse(Untyped).unveilUntyped // todo better solution for other tree types?
    }
  }

  case class Tuple(args: Seq[Expr]) extends Expr {
    val getType = TupleType(args map (_.getType))
  }

  /** $encodingof `(args) => body` */
  case class Lambda(args: Seq[ValDef], body: Expr) extends Expr {
    val getType = FunctionType(args.map(_.getType), body.getType).unveilUntyped
    def paramSubst(realArgs: Seq[Expr]): Map[Identifier, Expr] = {
      require(realArgs.size == args.size)
      (args map { _.id } zip realArgs).toMap
    }
    /* def withParamSubst(realArgs: Seq[Expr], e: Expr) = {
      replaceFromIDs(paramSubst(realArgs), e)
    } */
  }

  /** Literals */
  sealed abstract class Literal[+T] extends Expr with Terminal {
    val value: T
  }

  /** $encodingof a 16-bit integer literal */
  case class Int16Literal(value: Int) extends Literal[Int] {
    val getType = Int16Type
  }

  /** $encodingof a 32-bit integer literal */
  case class Int32Literal(value: Int) extends Literal[Int] {
    val getType = Int32Type
  }

  // Long
  case class Int64Literal(value: Long) extends Literal[Long] {
    val getType = Int64Type
  }

  /** $encodingof an infinite precision integer literal */
  case class IntegerLiteral(value: BigInt) extends Literal[BigInt] {
    val getType = IntegerType
  }

  /** $encodingof a boolean literal '''true''' or '''false''' */
  case class BooleanLiteral(value: Boolean) extends Literal[Boolean] {
    val getType = BooleanType
  }

  /** $encodingof the unit literal `()` */
  case class UnitLiteral() extends Literal[Unit] {
    val getType = UnitType
    val value = ()
  }

  // the string is exactly what the user wrote
  case class RealLiteral(value: Rational) extends Literal[Rational] {
    val getType = RealType
    protected var _stringValue: String = null
    def stringValue = _stringValue
  }

  object RealLiteral {
    def apply(value: Rational, stringValue: String): RealLiteral = {
      val r = RealLiteral(value)
      r._stringValue = stringValue
      r
    }
    val zero = RealLiteral(Rational.zero)
    val one = RealLiteral(Rational.one)
    val two = RealLiteral(Rational.two)
    val neg_one = RealLiteral(-Rational.one)
  }

  case class FinitePrecisionLiteral(value: Rational, prec: Precision, stringValue: String) extends Literal[Rational] {
    val getType = FinitePrecisionType(prec)
  }

  case class Cast(expr: Expr, newType: TypeTree) extends Expr {
    val getType = newType
  }

  /* Propositional logic */

  /** $encodingof `... == ...` */
  case class Equals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = {
      if (typesCompatible(lhs.getType, rhs.getType)) {
        BooleanType
      } else {
        Untyped
      }
    }
  }

  /** $encodingof `... && ...`
   *
   * [[exprs]] must contain at least two elements; if you are not sure about this,
   * you should use [[purescala.Constructors#and purescala's constructor and]]
   * or [[purescala.Constructors#andJoin purescala's constructor andJoin]]
   */
  case class And(exprs: Seq[Expr]) extends Expr {
    require(exprs.size >= 2)
    val getType = {
      if (exprs forall (_.getType == BooleanType)) {
        BooleanType
      } else {
        Untyped
      }
    }
  }

  object And {
    def apply(a: Expr, b: Expr): Expr = And(Seq(a, b))

    def make(exprs: Seq[Expr]): Expr = exprs match {
      case Seq() => BooleanLiteral(true)
      case Seq(e) => e
      case _ => new And(exprs)
    }
  }

  /** $encodingof `... || ...`
   *
   * [[exprs]] must contain at least two elements; if you are not sure about this,
   * you should use [[purescala.Constructors#or purescala's constructor or]] or
   * [[purescala.Constructors#orJoin purescala's constructor orJoin]]
   */
  case class Or(exprs: Seq[Expr]) extends Expr {
    require(exprs.size >= 2)
    val getType = {
      if (exprs forall (_.getType == BooleanType)) {
        BooleanType
      } else {
        Untyped
      }
    }
  }

  object Or {
    def apply(a: Expr, b: Expr): Expr = Or(Seq(a, b))
  }

  /** $encodingof `... ==> ...` (logical implication).
   *
   * This is not a standard Scala operator, but it results from an implicit
   * conversion in the Leon library.
   *
   * @see [[leon.purescala.Constructors.implies]]
   */
  case class Implies(lhs: Expr, rhs: Expr) extends Expr {
    val getType = {
      if (lhs.getType == BooleanType && rhs.getType == BooleanType) {
        BooleanType
      } else {
        Untyped
      }
    }
  }

  /** $encodingof `!...`
   *
   * @see [[leon.purescala.Constructors.not]]
   */
  case class Not(expr: Expr) extends Expr {
    val getType = {
      if (expr.getType == BooleanType) {
        BooleanType
      } else {
        Untyped
      }
    }
  }

  /* Arithmetic */

  /** $encodingof `... +  ...` */
  case class Plus(lhs: Expr, rhs: Expr) extends Expr {
    val getType = lhs.getType
  }

  /** $encodingof `... -  ...` */
  case class Minus(lhs: Expr, rhs: Expr) extends Expr {
    val getType = lhs.getType
  }

  /** $encodingof `- ... for BigInts` */
  case class UMinus(expr: Expr) extends Expr {
    val getType = expr.getType
  }

  /** $encodingof `... * ...` */
  case class Times(lhs: Expr, rhs: Expr) extends Expr {
    val getType = lhs.getType
  }

  /** $encodingof `(... * ...) + ...` */
  case class FMA(fac1: Expr, fac2: Expr, s: Expr) extends Expr {
    val getType = fac1.getType
  }

  /** $encodingof `... / ...` */
  case class Division(lhs: Expr, rhs: Expr) extends Expr {
    val getType = lhs.getType
  }

//  case class Pow(lhs: Expr, rhs: Expr) extends Expr {
//    assert(lhs.getType == rhs.getType)
//    val getType = {
//      if (lhs.getType == RealType) {
//        RealType
//      } else if (lhs.getType == IntegerType) {
//        IntegerType
//      } else {
//        Untyped
//      }
//    }
//  }

  case class IntPow(base: Expr, exp: Int) extends Expr {
    assert(exp > 0)
    override val getType: TypeTree = base.getType
  }

  case class Sqrt(t: Expr) extends Expr {
    // TODO: this operation may not be available for Float32
    val getType = t.getType
  }

  case class Sin(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class Cos(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class Tan(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class Asin(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class Acos(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class Atan(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class Exp(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class Log(t: Expr) extends Expr {
    val getType = t.getType
  }

  case class ApproxPoly(original: Expr, arg: Expr, approxFncId: Identifier, totalError: Rational) extends Expr {
    val getType = arg.getType
  }

  case class Approx(original: Expr, t: Expr, relImplError: Rational,
    errorMultiplier: Rational, implName: String, dblDbl: Boolean = false) extends Expr {
    val getType = t.getType
  }


  /*  Comparisons */

  /** $encodingof `... < ...` */
  case class LessThan(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
  }
  /** $encodingof `... > ...` */
  case class GreaterThan(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
  }
  /** $encodingof `... <= ...` */
  case class LessEquals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
  }
  /** $encodingof `... >= ...` */
  case class GreaterEquals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
  }

  /* Shifts */
  case class RightShift(t: Expr, by: Int) extends Expr {
    val getType = t.getType
  }

  case class LeftShift(t: Expr, by: Int) extends Expr {
    val getType = t.getType
  }

  /* Specs */

  case class AbsError(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
  }

  /*  Program structure  */

  // maybe we don't need this
  /* case class Block(exprs: Seq[Expr], last: Expr) extends Expr {
    def extract: Option[(Seq[Expr], (Seq[Expr])=>Expr)] = {
      Some((exprs :+ last, exprs => Block(exprs.init, exprs.last)))
    }

    override def getPos = {
      Position.between(exprs.head.getPos, last.getPos)
    }

    def printWith(implicit pctx: PrinterContext) {
      p"${nary(exprs :+ last, "\n")}"
    }

    val getType = last.getType

    override def isSimpleExpr = false
  }

  /*
    An assignment to a new variable.

    Daisy supports only a functional language, hence no re-assigment is allowed.
   */
  case class Assignment(varId: Identifier, expr: Expr) extends Expr {
    val getType = UnitType

    def extract: Option[(Seq[Expr], Seq[Expr]=>Expr)] = {
      Some((Seq(expr), (es: Seq[Expr]) => Assignment(varId, es.head)))
    }

    def printWith(implicit pctx: PrinterContext) {
      p"$varId = $expr;"
    }
  } */
}
