
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy
package lang

import scala.collection.immutable.Seq
import TypeOps._
import Types._
import utils.{Interval, Positioned, Rational}
import Identifiers._
import utils.FinitePrecision.{Float64, Precision}

object Trees {

  abstract class Tree extends Positioned with Serializable {
    def copiedFrom(o: Tree): this.type = {
      setPos(o)
      this
    }

    override def toString: String = PrettyPrinter(this)

    // copy keeps Identifiers intact, but looses positions
    def deepCopy: Tree
  }

  sealed abstract class Definition extends Tree {

    val id: Identifier

    override def hashCode : Int = id.hashCode
    override def equals(that : Any) : Boolean = that match {
      case t : Definition => t.id == this.id
      case _ => false
    }

  }

  /**
    * A ValDef represents a parameter of a [[lang.Trees.FunDef function]].
    */
  case class ValDef(id: Identifier) extends Definition with Typed {
    self: Serializable =>

    val getType = id.getType

    def deepCopy = ValDef(id)
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
    isField: Boolean = false) extends Definition {

    def deepCopy = FunDef(id,
      returnType,
      params.map(_.deepCopy),
      precondition.map(_.deepCopy),
      body.map(_.deepCopy),
      postcondition.map(_.deepCopy),
      isField)
  }

  /**
    A program is currently one (and only one) top-level object with a
    number of function and field definitions.
    We ignore package and import information.
    // TODO: constructor block
   */
  case class Program(id: Identifier, defs: Seq[FunDef]) extends Definition {
    def deepCopy = Program(id, defs.map(_.deepCopy))
  }



  /** Represents an expression in Leon. */
  sealed abstract class Expr extends Tree with Typed {

    override def deepCopy: Expr
  }

  /** Trait which gets mixed-in to expressions without subexpressions */
  trait Terminal {
    self: Expr =>
  }

  /** Trait which allows to get and Identifier from the Expr*/
  trait Identifiable {
   def getId: Identifier
  }

  /** Stands for an undefined Expr, similar to `???` or `null`
    *
    * During code generation, it gets compiled to `null`, or the 0 of the
    * respective type for value types.
    */
  case class NoTree(tpe: TypeTree) extends Expr with Terminal {
    val getType = tpe
    def deepCopy = NoTree(tpe)
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
    def deepCopy = Error(tpe, description)
  }

  /** Precondition of an [[Expressions.Expr]]. Corresponds to the Leon keyword *require*
    *
    * @param pred The precondition formula inside ``require(...)``
    * @param body The body following the ``require(...)``
    */
  case class Require(pred: Expr, body: Expr) extends Expr {
    val getType = {
      if (pred.getType == BooleanType)
        body.getType
      else Untyped
    }
    def deepCopy = Require(pred.deepCopy, body.deepCopy)
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
    //def toAssert: Expr = {...}
    def deepCopy = Ensuring(body.deepCopy, pred.deepCopy)
  }

  /** Local assertions with customizable error message
    *
    * @param pred The predicate, first argument of `assert(..., ...)`
    * @param error An optional error string to display if the assert fails. Second argument of `assert(..., ...)`
    * @param body The expression following `assert(..., ...)`
    */
  case class Assert(pred: Expr, error: Option[String], body: Expr) extends Expr {
    val getType = {
      if (pred.getType == BooleanType)
        body.getType
      else Untyped
    }
    def deepCopy = Assert(pred.deepCopy, error, body.deepCopy)
  }


  /** Variable
    * @param id The identifier of this variable
    */
  case class Variable(id: Identifier) extends Expr with Terminal with NumAnnotation {
    val getType = id.getType
    def deepCopy = Variable(id)
  }

  /**
    * Delta
    * Should be a special subclass of the variable. Not inherited to enable the case distinction
    */
  // TODO Anastasiia: try to make it Variable Subclass and overcome case-to-case inheritance problem

  case class Delta(id: Identifier) extends Expr with Terminal with NumAnnotation with Identifiable {
    val getType = id.getType
    def deepCopy = Delta(id)
    def toVariable = Variable(id)
    override def getId: Identifier = id
  }

  /**
    * Epsilon
    * Should be a special subclass of the variable. Not inherited to enable the case distinction
    */
  case class Epsilon(id: Identifier) extends Expr with Terminal with NumAnnotation with Identifiable {
    val getType = id.getType
    def deepCopy = Epsilon(id)
    def toVariable = Variable(id)
    override def getId: Identifier = id
  }



  /** $encodingof `val ... = ...; ...`
    *
    * @param binder The identifier used in body, defined just after '''val'''
    * @param value The value assigned to the identifier, after the '''=''' sign
    * @param body The expression following the ``val ... = ... ;`` construct
    * @see [[leon.purescala.Constructors#let purescala's constructor let]]
    */
  case class Let(binder: Identifier, value: Expr, body: Expr) extends Expr with NumAnnotation {
    val getType = {
      // We can't demand anything sticter here, because some binders are
      // typed context-wise
      if (typesCompatible(value.getType, binder.getType))
        body.getType
      else {
        Untyped
      }
    }
    def deepCopy = Let(binder, value.deepCopy, body.deepCopy)
  }

  /* Control flow */

  /** $encodingof  `function(...)` (function invocation) */
  // There is a chicken-n-egg problem here, because at extraction time,
  // we don't have (and can't have?) all the FunDef's ready, if they are to be immutable.
  // Additionally, the fnc body may change, e.g. with SSA transform.
  case class FunctionInvocation(fdId: Identifier, params: Seq[ValDef], args: Seq[Expr],
    returnType: TypeTree) extends Expr {
    //require(fd.params.size == args.size)
    val getType = returnType
    def deepCopy = FunctionInvocation(fdId, params.map(_.deepCopy), args.map(_.deepCopy),
      returnType)
  }

  /** $encodingof `if(...) ... else ...` */
  case class IfExpr(cond: Expr, thenn: Expr, elze: Expr) extends Expr {
    val getType = leastUpperBound(thenn.getType, elze.getType).getOrElse(Untyped).unveilUntyped
    def deepCopy = IfExpr(cond.deepCopy, thenn.deepCopy, elze.deepCopy)
  }

  /** $encodingof `(args) => body` */
  case class Lambda(args: Seq[ValDef], body: Expr) extends Expr {
    val getType = FunctionType(args.map(_.getType), body.getType).unveilUntyped
    def paramSubst(realArgs: Seq[Expr]) = {
      require(realArgs.size == args.size)
      (args map { _.id } zip realArgs).toMap
    }
    /*def withParamSubst(realArgs: Seq[Expr], e: Expr) = {
      replaceFromIDs(paramSubst(realArgs), e)
    }*/
    def deepCopy = Lambda(args.map(_.deepCopy), body.deepCopy)
  }

  /** Literals */
  sealed abstract class Literal[+T] extends Expr with Terminal {
    val value: T
  }

  /** $encodingof a 32-bit integer literal */
  case class Int32Literal(value: Int) extends Literal[Int] {
    val getType = Int32Type
    def deepCopy = Int32Literal(value)
  }

  // Long
  case class Int64Literal(value: Long) extends Literal[Long] {
    val getType = Int64Type
    def deepCopy = Int64Literal(value)
  }

  /** $encodingof an infinite precision integer literal */
  case class IntegerLiteral(value: BigInt) extends Literal[BigInt] {
    val getType = IntegerType
    def deepCopy = IntegerLiteral(value)
  }

  /** $encodingof a boolean literal '''true''' or '''false''' */
  case class BooleanLiteral(value: Boolean) extends Literal[Boolean] {
    val getType = BooleanType
    def deepCopy = BooleanLiteral(value)
  }

  /** $encodingof the unit literal `()` */
  case class UnitLiteral() extends Literal[Unit] {
    val getType = UnitType
    val value = ()
    def deepCopy = UnitLiteral()
  }

  // the string is exactly what the user wrote
  case class RealLiteral(value: Rational) extends Literal[Rational] with NumAnnotation {
    val getType = RealType

    private var _stringValue: String = null
    def stringValue_=(s: String): Unit = {
      if (_stringValue == null) {
        _stringValue = s
      } else {
        throw new Exception("'stringValue' is a write-only-once field, but you tried twice!")
      }
    }
    def stringValue = _stringValue
    def deepCopy = {
      val tmp = RealLiteral(value)
      tmp.stringValue = this.stringValue
      tmp
    }
  }

  case class FinitePrecisionLiteral(value: Rational, prec: Precision) extends Literal[Rational] with NumAnnotation {
    val getType = FinitePrecisionType(prec)

    private var _stringValue: String = null
    def stringValue_=(s: String): Unit = {
      if (_stringValue == null) {
        _stringValue = s
      } else {
        throw new Exception("'stringValue' is a write-only-once field, but you tried twice!")
      }
    }
    def stringValue = _stringValue
    def deepCopy = {
      val tmp = FinitePrecisionLiteral(value, prec)
      tmp.stringValue = this.stringValue
      tmp
    }
  }



  /* Propositional logic */

  /** $encodingof `... == ...` */
  case class Equals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = {
      if (typesCompatible(lhs.getType, rhs.getType)) BooleanType
      else {
        Untyped
      }
    }
    def deepCopy = Equals(lhs.deepCopy, rhs.deepCopy)
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
      if (exprs forall (_.getType == BooleanType)) BooleanType
      else Untyped
    }
    def deepCopy = And(exprs.map(_.deepCopy))
  }

  object And {
    def apply(a: Expr, b: Expr): Expr = And(Seq(a, b))
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
      if (exprs forall (_.getType == BooleanType)) BooleanType
      else Untyped
    }
    def deepCopy = Or(exprs.map(_.deepCopy))
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
      if(lhs.getType == BooleanType && rhs.getType == BooleanType) BooleanType
      else Untyped
    }
    def deepCopy = Implies(lhs.deepCopy, rhs.deepCopy)
  }

  /** $encodingof `!...`
    *
    * @see [[leon.purescala.Constructors.not]]
    */
  case class Not(expr: Expr) extends Expr {
    val getType = {
      if (expr.getType == BooleanType) BooleanType
      else Untyped
    }
    def deepCopy = Not(expr.deepCopy)
  }

  /* Arithmetic */

  /** $encodingof `... +  ...` */
  case class Plus(lhs: Expr, rhs: Expr) extends Expr with NumAnnotation {
    assert(lhs.getType == rhs.getType, "lhs: " + lhs.getType + ", rhs: " + rhs.getType)
    val getType = lhs.getType
    def deepCopy = Plus(lhs.deepCopy, rhs.deepCopy)
  }

  /** $encodingof `... -  ...` */
  case class Minus(lhs: Expr, rhs: Expr) extends Expr with NumAnnotation {
    assert(lhs.getType == rhs.getType)
    val getType = lhs.getType
    def deepCopy = Minus(lhs.deepCopy, rhs.deepCopy)
  }

  /** $encodingof `- ... for BigInts`*/
  case class UMinus(expr: Expr) extends Expr with NumAnnotation {
    val getType = expr.getType
    def deepCopy = UMinus(expr.deepCopy)
  }

  /** $encodingof `... * ...` */
  case class Times(lhs: Expr, rhs: Expr) extends Expr with NumAnnotation {
    assert(lhs.getType == rhs.getType)
    val getType = lhs.getType
    def deepCopy = Times(lhs.deepCopy, rhs.deepCopy)
  }

  /** $encodingof `... / ...` */
  case class Division(lhs: Expr, rhs: Expr) extends Expr with NumAnnotation {
    assert(lhs.getType == rhs.getType, "lhs: " + lhs.getType + ", " + rhs.getType)
    val getType = lhs.getType
    def deepCopy = Division(lhs.deepCopy, rhs.deepCopy)
  }

  case class Pow(lhs: Expr, rhs: Expr) extends Expr with NumAnnotation {
    assert(lhs.getType == rhs.getType)
    val getType = {
      if (lhs.getType == RealType) RealType
      else if (lhs.getType == IntegerType) IntegerType
      else Untyped
    }
    def deepCopy = Pow(lhs.deepCopy, rhs.deepCopy)
  }

  case class Sqrt(t: Expr) extends Expr with NumAnnotation {
    require(t.getType == RealType)
    assert(t.isInstanceOf[NumAnnotation])
    // TODO: this operation may not be available for Float32
    val getType = t.getType
    def deepCopy = Sqrt(t.deepCopy)
  }

  case class Sin(t: Expr) extends Expr with NumAnnotation {
    require(t.getType == RealType)
    val getType = RealType
    def deepCopy = Sin(t.deepCopy)
  }

  case class Cos(t: Expr) extends Expr with NumAnnotation {
    require(t.getType == RealType)
    val getType = RealType
    def deepCopy = Cos(t.deepCopy)
  }

  case class Tan(t: Expr) extends Expr with NumAnnotation {
    require(t.getType == RealType)
    val getType = RealType
    def deepCopy = Tan(t.deepCopy)
  }

  case class Exp(t: Expr) extends Expr with NumAnnotation {
    require(t.getType == RealType)
    val getType = RealType
    def deepCopy = Exp(t.deepCopy)
  }

  case class Log(t: Expr) extends Expr with NumAnnotation {
    require(t.getType == RealType)
    val getType = RealType
    def deepCopy = Log(t.deepCopy)
  }

  /*  Comparisons */

  /** $encodingof `... < ...`*/
  case class LessThan(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    def deepCopy = LessThan(lhs.deepCopy, rhs.deepCopy)
  }
  /** $encodingof `... > ...`*/
  case class GreaterThan(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    def deepCopy = GreaterThan(lhs.deepCopy, rhs.deepCopy)
  }
  /** $encodingof `... <= ...`*/
  case class LessEquals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    def deepCopy = LessEquals(lhs.deepCopy, rhs.deepCopy)
  }
  /** $encodingof `... >= ...`*/
  case class GreaterEquals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    def deepCopy = GreaterEquals(lhs.deepCopy, rhs.deepCopy)
  }

  /* Shifts */
  case class RightShift(t: Expr, by: Int) extends Expr {
    val getType = t.getType
    def deepCopy = RightShift(t.deepCopy, by)
  }

  case class LeftShift(t: Expr, by: Int) extends Expr {
    val getType = t.getType
    def deepCopy = LeftShift(t.deepCopy, by)
  }

  /* Specs */

  case class AbsError(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    def deepCopy = AbsError(lhs.deepCopy, rhs.deepCopy)
  }

  /*  Program structure  */

  // maybe we don't need this
  /*case class Block(exprs: Seq[Expr], last: Expr) extends Expr {
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
  }*/
}