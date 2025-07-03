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
    private lazy val _hash = (getClass, id).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, binder, value, body).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, fdId, params, args).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, cond, thenn, elze).##
    override def hashCode: Int = _hash
  }

  case class Tuple(args: Seq[Expr]) extends Expr {
    val getType = TupleType(args map (_.getType))
    private lazy val _hash = (getClass, args).##
    override def hashCode: Int = _hash
  }

  case class TupleElement(tpl: Expr, index: Int) extends Expr {
    assert(tpl.getType match { case TupleType(_) => true; case _ => false }, s"Trying to take an tuple element of a non-tuple expression ${tpl}: ${tpl.getType}")
    val getType = tpl.getType match { case TupleType(args) => args(index) }
    private lazy val _hash = (getClass, tpl, index).##
    override def hashCode: Int = _hash
  }

  /** $encodingof `(args) => body` */
  case class Lambda(args: Seq[ValDef], body: Expr) extends Expr {
    val getType = FunctionType(args.map(_.getType), body.getType).unveilUntyped
    def paramSubst(realArgs: Seq[Expr]): Map[Identifier, Expr] = {
      require(realArgs.size == args.size)
      (args map { _.id } zip realArgs).toMap
    }
    val returnType: TypeTree = body.getType
    /* def withParamSubst(realArgs: Seq[Expr], e: Expr) = {
      replaceFromIDs(paramSubst(realArgs), e)
    } */
    private lazy val _hash = (getClass, args, body).##
    override def hashCode: Int = _hash
  }

  /** Literals */
  sealed abstract class Literal[+T] extends Expr with Terminal {
    val value: T
    private lazy val _hash = (getClass, value).##
    override def hashCode: Int = _hash
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

  case class FloatLiteral(value: Float) extends Literal[Float]{
    val getType = FinitePrecisionType(Float32)
    protected var _stringValue: String = null
    def stringValue = _stringValue
  }

  case class DoubleLiteral(value: Double) extends Literal[Double] {
    val getType = FinitePrecisionType(Float64)
    protected var _stringValue: String = null
    def stringValue = _stringValue
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
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, exprs).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, exprs).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, expr).##
    override def hashCode: Int = _hash
  }

  /* Arithmetic */

  /** $encodingof `... +  ...` */
  case class Plus(lhs: Expr, rhs: Expr) extends Expr {
    val getType: TypeTree = {
      if (lhs.getType == RealType && rhs.getType == Int32Type ||
        (lhs.getType == Int32Type && rhs.getType == RealType)) {
        RealType
      } else {
        lhs.getType
      }
    }
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }

  /** $encodingof `... -  ...` */
  case class Minus(lhs: Expr, rhs: Expr) extends Expr {
    val getType: TypeTree = {
      if (lhs.getType == RealType && rhs.getType == Int32Type ||
        (lhs.getType == Int32Type && rhs.getType == RealType)) {
        RealType
      } else {
        lhs.getType
      }
    }
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }

  /** $encodingof `- ... for BigInts` */
  case class UMinus(expr: Expr) extends Expr {
    val getType = expr.getType
    private lazy val _hash = (getClass, expr).##
    override def hashCode: Int = _hash
  }

  /** $encodingof `... * ...` */
  case class Times(lhs: Expr, rhs: Expr) extends Expr {
    val getType: TypeTree = {
      if (lhs.getType == RealType && rhs.getType == Int32Type ||
        (lhs.getType == Int32Type && rhs.getType == RealType)) {
        RealType
      } else {
        lhs.getType
      }
    }
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }

  /** $encodingof `(... * ...) + ...` */
  case class FMA(fac1: Expr, fac2: Expr, s: Expr) extends Expr {
    val getType = fac1.getType
    private lazy val _hash = (getClass, fac1, fac2, s).##
    override def hashCode: Int = _hash
  }

  /** $encodingof `... / ...` */
  case class Division(lhs: Expr, rhs: Expr) extends Expr {
    val getType: TypeTree = {
      if (lhs.getType == RealType && rhs.getType == Int32Type ||
        (lhs.getType == Int32Type && rhs.getType == RealType)) {
        RealType
      } else {
        lhs.getType
      }
    }
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, base, exp).##
    override def hashCode: Int = _hash
  }

  case class Sqrt(t: Expr) extends Expr {
    // TODO: this operation may not be available for Float32
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Sin(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Cos(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Tan(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Asin(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Acos(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Atan(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Exp(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
  }

  case class Log(t: Expr) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t).##
    override def hashCode: Int = _hash
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
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }
  /** $encodingof `... > ...` */
  case class GreaterThan(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }
  /** $encodingof `... <= ...` */
  case class LessEquals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }
  /** $encodingof `... >= ...` */
  case class GreaterEquals(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }

  /* Shifts */
  case class RightShift(t: Expr, by: Int) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t, by).##
    override def hashCode: Int = _hash
  }

  case class LeftShift(t: Expr, by: Int) extends Expr {
    val getType = t.getType
    private lazy val _hash = (getClass, t, by).##
    override def hashCode: Int = _hash
  }

  /* Specs */

  case class AbsError(lhs: Expr, rhs: Expr) extends Expr {
    val getType = BooleanType
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }

  /* Loops over data structures */
  // tensor literals
  case class VectorLiteral(id: Identifier) extends Variable {
    override val getType: TypeTree = id.getType
    override def equals(obj: scala.Any): Boolean = super.equals(obj) // todo compare sizes as well?
    private lazy val _hash = (getClass, id).##
    override def hashCode: Int = _hash
  }

  case class VectorFromList(value: Seq[Expr], size: Int) extends Expr {
    // todo do we need an id?, id: Identifier
    override val getType: TypeTree = VectorType(value.map(_.getType).distinct) // todo do we need all types or just distinct?
    override def equals(obj: scala.Any): Boolean = obj match {
      case VectorFromList(cvd, cvsz) => cvsz == size && cvd == value
      case _ => false
    }
    private lazy val _hash = (getClass, value, size).##
    override def hashCode: Int = _hash
  }

  case class VectorFromExpr(from: Expr) extends Expr {
    assert(from.getType match { case VectorType(_) => true; case _ => false }, s"Trying to create a vector from incompatible expression ${from}: ${from.getType}")
    override val getType: TypeTree = from.getType match {
      case x: VectorType => x // result of map
    }
    override def equals(obj: scala.Any): Boolean = obj match {
      case VectorFromExpr(from2) => from == from2
      case _ => false
    }
    private lazy val _hash = (getClass, from).##
    override def hashCode: Int = _hash
  }

  case class FlatVector(from: Expr) extends Expr {
    assert(from.getType match { case VectorType(Seq(VectorType(_))) => true; case _ => false }, s"Trying to create a flat vector from incompatible expression ${from}: ${from.getType}")
    override val getType: TypeTree = from.getType match {
      case VectorType(Seq(x)) => x // flatten the type
    }
    override def equals(obj: scala.Any): Boolean = obj match {
      case FlatVector(from2) => from == from2
      case _ => false
    }
    private lazy val _hash = (getClass, from).##
    override def hashCode: Int = _hash
  }

  case class MatrixLiteral(id: Identifier) extends Variable {
    override val getType: TypeTree = id.getType
    override def equals(obj: scala.Any): Boolean = super.equals(obj) // todo compare sizes as well?
    private lazy val _hash = (getClass, id).##
    override def hashCode: Int = _hash
  }

  case class MatrixFromLists(value: Seq[Seq[Expr]], numRows: Int, numCols: Int) extends Expr {
    // todo do we need an id? id: Identifier,
    override val getType: TypeTree = {
      val tp: Seq[Seq[TypeTree]] = value.map(x => {
        x.map(y => y.getType)
      })
      MatrixType(tp.flatten.distinct) // todo distinct or all?
    }
    override def equals(obj: scala.Any): Boolean = obj match {
      case MatrixFromLists(cvd, cvr, cvc) => cvr == numRows && cvc == numCols && cvd == value
      case _ => false
    }
    private lazy val _hash = (getClass, value, numRows, numCols).##
    override def hashCode: Int = _hash
  }

  case class MatrixFromExpr(from: Expr) extends Expr {
    assert(from.getType match { case MatrixType(_) => true; case _ => false }, s"Trying to create a matrix from incompatible expression ${from}: ${from.getType}")
    override val getType: TypeTree = from.getType match {
      case x: MatrixType => x // result of map
    }
    override def equals(obj: scala.Any): Boolean = obj match {
      case MatrixFromExpr(from2) => from == from2
      case _ => false
    }
    private lazy val _hash = (getClass, from).##
    override def hashCode: Int = _hash
  }

  case class VectorElement(v: Expr, index: Expr) extends Expr with Terminal { // similar to a variable
    assert(v.getType match { case VectorType(_) => true; case _ => false }, s"Trying to take an vector element of a non-vector expression ${v}: ${v.getType}")
    override val getType: TypeTree = v.getType match {case VectorType(args) => args.head } // todo for it's mixed-precision check the actual spec
    override def equals(obj: scala.Any): Boolean = obj match {
      case VectorElement(ds, ind) => ds == v && ind == index
      case _ => false
    }
    private lazy val _hash = (getClass, v, index).##
    override def hashCode: Int = _hash
  }

  case class MatrixElement(m: Expr, irow: Expr, icol: Expr) extends Expr with Terminal { // similar to a variable
    assert(m.getType match { case MatrixType(_) => true; case _ => false }, s"Trying to take an matrix element of a non-matrix expression ${m}: ${m.getType}")
    override val getType: TypeTree = m.getType match {case MatrixType(args) => args.head } // todo for it's mixed-precision check the actual spec
    override def equals(obj: scala.Any): Boolean = obj match {
      case MatrixElement(ds, indI, indJ) => ds == m && indI == irow && indJ == icol
      case _ => false
    }
    private lazy val _hash = (getClass, m, irow, icol).##
    override def hashCode: Int = _hash
  }

  // for specifying range of a whole vector or subset of vector elements
  case class VectorRange(v: VectorLiteral, fromInd: Int, toInd: Int, lb: RealLiteral, ub: RealLiteral) extends Expr {
    val getType: TypeTree = BooleanType
    private lazy val _hash = (getClass, v, fromInd, toInd, lb, ub).##
    override def hashCode: Int = _hash
  }

  // for specifying range of a whole matrix or subset of matrix elements
  case class MatrixRange(v: MatrixLiteral, indices: Seq[(Int, Int)], lb: RealLiteral, ub: RealLiteral) extends Expr {
    val getType: TypeTree = BooleanType
    private lazy val _hash = (getClass, v, indices, lb, ub).##
    override def hashCode: Int = _hash
  }

  // for specifying vector/matrix size in the ensuring clause
  case class SizeLessEquals(v: Expr, rowNum: Int, colNum: Int) extends Expr {
    val getType: TypeTree = BooleanType
    private lazy val _hash = (getClass, v, rowNum, colNum).##
    override def hashCode: Int = _hash
  }

  // AST node of v.length() | m.length()
  case class SizeLength(v: Expr) extends Expr {
    val getType: TypeTree = Int32Type
    private lazy val _hash = (getClass, v).##
    override def hashCode: Int = _hash
  }

  // AST node of m.numRows()
  case class SizeNumRows(v: Expr) extends Expr {
    val getType: TypeTree = Int32Type
    private lazy val _hash = (getClass, v).##
    override def hashCode: Int = _hash
  }
  // AST node of m.numCols()
  case class SizeNumCols(v: Expr) extends Expr {
    val getType: TypeTree = Int32Type
    private lazy val _hash = (getClass, v).##
    override def hashCode: Int = _hash
  }

  /* Subset of elements */
  // DSL sub(i: Int, j: Int): Vector and everyNth(i: Int): Vector = ???
  case class SubVector(v: Expr, from: Expr, to: Expr) extends Expr {
    val getType: TypeTree = v.getType
    private lazy val _hash = (getClass, v, from, to).##
    override def hashCode: Int = _hash
  }
  // DSL everyNth(i: Int): Vector = ???
  case class EveryNthVector(v: Expr, n: Expr, from: Expr) extends Expr {
    val getType: TypeTree = v.getType
    private lazy val _hash = (getClass, v, n, from).##
    override def hashCode: Int = _hash
  }

  // DSL sub(fromI: Int, fromJ: Int)(toI: Int, toJ: Int): Matrix
  case class SubMatrix(m: Expr, fromTOindices: Seq[(Int, Int)]) extends Expr {
    val getType: TypeTree = m.getType
    private lazy val _hash = (getClass, m, fromTOindices).##
    override def hashCode: Int = _hash
  }

  // DSL everyNth(i: Int): Matrix = ???
  case class EveryNthMatrix(m: Expr, n: Expr, from: Expr) extends Expr {
    val getType: TypeTree = m.getType
    private lazy val _hash = (getClass, m, n, from).##
    override def hashCode: Int = _hash
  }

  // DSL row(i:Int): Vector
  case class RowOfMatrix(m: Expr, i: Expr) extends Expr {
    assert(i.getType == Int32Type, s"Trying to pass a ${i.getType} as a row index on ${m}.")
    assert(m.getType match { case MatrixType(_) => true; case _ => false }, s"Trying to take a row of a non-matrix expression ${m}: ${m.getType}")
    val getType: TypeTree = m.getType match { case MatrixType(t) => VectorType(t) } // return one vector of the same type as the matrix todo for mixed-precision might need adjustment
    private lazy val _hash = (getClass, m, i).##
    override def hashCode: Int = _hash
  }

  /* Operations on elements */
  // DSL Vector.x(Vector) and Matrix.x(Matrix)
  case class CrossProduct(lhs: Expr, rhs: Expr) extends Expr {
    val getType: TypeTree = rhs.getType // e.g Matrix x Vector -> Vector
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }

  // DSL Vector|Matrix.min()
  case class MinOf(v: Expr) extends Expr {
    val getType: TypeTree = v.getType match {
      case VectorType(t) => t.head
      case MatrixType(t) => t.head
    }
    private lazy val _hash = (getClass, v).##
    override def hashCode: Int = _hash
  }

  // DSL Vector|Matrix.max()
  case class MaxOf(v: Expr) extends Expr {
    val getType: TypeTree = v.getType match {
      case VectorType(t) => t.head
      case MatrixType(t) => t.head
    }
    private lazy val _hash = (getClass, v).##
    override def hashCode: Int = _hash
  }

  // DSL: Matrix.determinant(): Matrix
  case class Determinant(m: Expr) extends Expr {
    val getType: TypeTree = m.getType match { case MatrixType(t) => t.head }
    private lazy val _hash = (getClass, m).##
    override def hashCode: Int = _hash
  }

  // DSL: Matrix.inverse(): Matrix
  case class MatrixInverse(m: Expr) extends Expr {
    val getType: TypeTree = m.getType
    private lazy val _hash = (getClass, m).##
    override def hashCode: Int = _hash
  }


  /* rearrange DS */
   // DSL: Vector.++(v: Vector): Vector = ???
   case class Concat(lhs: Expr, rhs: Expr) extends Expr {
     val getType: TypeTree = lhs.getType
     private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
   }

  // DSL: Vector.:+(v: Vector|Matrix)
  case class AppendElement(ds: Expr, el: Expr) extends Expr {
    val getType: TypeTree = ds.getType
    private lazy val _hash = (getClass, ds, el).##
    override def hashCode: Int = _hash
  }

  // DSL: Vector.+:(v: Vector|Matrix)
  case class PrependElement(ds: Expr, el: Expr) extends Expr {
    val getType: TypeTree = ds.getType
    private lazy val _hash = (getClass, ds, el).##
    override def hashCode: Int = _hash
  }

  // DSL zip(Vector, Vector): Matrix
  case class ZipVectors(lhs: Expr, rhs: Expr) extends Expr {
    val getType: TypeTree = {
      val lTpe = lhs.getType match { case VectorType(t) => t }
      val rTpe = rhs.getType match { case VectorType(t) => t }
      val resTpe = (lTpe ++ rTpe).distinct
      MatrixType(resTpe)
    }
    private lazy val _hash = (getClass, lhs, rhs).##
    override def hashCode: Int = _hash
  }

  // DSL Matrix.flatten(): Vector
  case class FlattenMatrix(m: Expr) extends Expr {
    val getType: TypeTree = m.getType match {
      case MatrixType(x) => VectorType(Seq(x.head))
    }
    private lazy val _hash = (getClass, m).##
    override def hashCode: Int = _hash
  }

  // DSL: Matrix.transpose(): Matrix = ???
  case class Transpose(m: Expr) extends Expr {
    val getType: TypeTree = m.getType
    private lazy val _hash = (getClass, m).##
    override def hashCode: Int = _hash
  }

  // DSL: Matrix.flipud(): Matrix
  case class FlipUpsideDown(m: Expr) extends Expr {
    val getType: TypeTree = m.getType
    private lazy val _hash = (getClass, m).##
    override def hashCode: Int = _hash
  }

  // DSL: Matrix.fliplr(): Matrix
  case class FlipLeftToRight(m: Expr) extends Expr {
    val getType: TypeTree = m.getType
    private lazy val _hash = (getClass, m).##
    override def hashCode: Int = _hash
  }

  // DSL: def pad(i: Int): Vector = ??? // add zeros padding around the vector
  case class PadVector(v: Expr, padSize: Expr) extends Expr {
    assert(padSize.getType == Int32Type, s"Trying to pass a ${padSize.getType} as a padding size for ${v}.")
    val getType: TypeTree = v.getType
    private lazy val _hash = (getClass, v, padSize).##
    override def hashCode: Int = _hash
  }

  // DSL: def pad(i: Int, j: Int): Matrix = ??? // add zeros padding around the matrix
  case class PadMatrix(m: Expr, padRows: Expr, padCols: Expr) extends Expr {
    assert(padRows.getType == Int32Type, s"Trying to pass a ${padRows.getType} as a padding row number for ${m}.")
    assert(padCols.getType == Int32Type, s"Trying to pass a ${padCols.getType} as a padding column number for ${m}.")
    val getType: TypeTree = m.getType
    private lazy val _hash = (getClass, m, padRows, padCols).##
    override def hashCode: Int = _hash
  }

  /* Loop-like constructs */
  // DSL: Vector.map(fnc: (Real) => Real)
  case class MapIter(v: Expr, func: Lambda) extends Expr {
    val getType: TypeTree = v.getType match {
      case VectorType(_) => VectorType(Seq(func.returnType))
      case MatrixType(_) =>
        val elType = func.returnType match {
          case VectorType(args) => args.head
        }
        MatrixType(Seq(elType))
    }
    private lazy val _hash = (getClass, v, func).##
    override def hashCode: Int = _hash
  }

  // DSL: Vector.fold(init: Real)(fnc: (Real,Real) => Real): Real
  case class FoldIter(v: Expr, init: Expr, fnc: Lambda) extends Expr {
    val getType: TypeTree = init.getType
    private lazy val _hash = (getClass, v, init, fnc).##
    override def hashCode: Int = _hash
  }
  // DSL: Vector.fold(init: Real)(fnc: (Real,Real) => Real): Real
  case class Sum(v: Expr, init: Expr) extends Expr {
    val getType: TypeTree = init.getType
    private lazy val _hash = (getClass, v, init).##
    override def hashCode: Int = _hash
  }

  // DSL: Matrix.mapElements(fnc: (Real) => Real)
  case class MapElemsIter(m: Expr, func: Lambda) extends Expr {
    val getType: TypeTree = MatrixType(Seq(func.returnType)) // todo allow different types in one matrix? - mixed-precision
    private lazy val _hash = (getClass, m, func).##
    override def hashCode: Int = _hash
  }

  // DSL: Matrix.foldElements(init: Real)(fnc: (Real,Real) => Real): Real
  case class FoldElemsIter(m: Expr, init: Expr, fnc: Lambda) extends Expr {
    val getType: TypeTree = init.getType
    private lazy val _hash = (getClass, m, init, fnc).##
    override def hashCode: Int = _hash
  }

  // DSL: Vector.filter(fnc: (Real) => Boolean): Vector
  // DSL: Matrix.filter(fnc: (Vector) => Boolean): Matrix
  case class FilterIter(v: Expr, func: Lambda) extends Expr {
    val getType: TypeTree = v.getType
    private lazy val _hash = (getClass, v, func).##
    override def hashCode: Int = _hash
  }

  // DSL: def enumMap((Int, Real) => Real): Vector = ???
  case class EnumVectorAndMap(v: Expr, fnc: Lambda) extends Expr {
    assert(v.getType match { case VectorType(_) => true; case _ => false }, s"Trying to enumerate elements of a vector on a non-vector expression ${v}: ${v.getType}")
    val getType: TypeTree = VectorType(Seq(fnc.returnType))
    private lazy val _hash = (getClass, v, fnc).##
    override def hashCode: Int = _hash
  }

  // enumSlideFlatMap(n: Int)(fnc: (Int, Vector) => Vector): Vector - like a slide, but with indexing
  case class EnumSlideFlatMap(v: Expr, n: Expr, fnc: Lambda) extends Expr {
    assert(n.getType == Int32Type, s"$n has wrong type ${n.getType} in $this")
    assert(v.getType match { case VectorType(_) => true; case _ => false }, s"Trying to enumerate vector on a non-vector expression ${v}: ${v.getType}")
    val getType: TypeTree = fnc.returnType
    private lazy val _hash = (getClass, v, n, fnc).##
    override def hashCode: Int = _hash
  }

  // DSL: enumRowsMap(fnc: (Int, Vector) => Real): Matrix = ???
  case class EnumRowsAndMap(m: Expr, fnc: Lambda) extends Expr {
    assert(m.getType match { case MatrixType(_) => true; case _ => false }, s"Trying to enumerate matrix a non-matrix expression ${m}: ${m.getType}")
    val getType: TypeTree = {
      val lambdaType = fnc.returnType match {
        case VectorType(args) => args
        case _ => throw new DaisyFatalError(Some(s"Lambda function returns the wrong type ${fnc.returnType}, expected Vector[T]"))
      }
      MatrixType(lambdaType)
    }
    private lazy val _hash = (getClass, m, fnc).##
    override def hashCode: Int = _hash
  }

  // DSL: def slide(size:Int, step: Int): List[Vector] = ??? // apply sliding window to the vector, list resulting sub-vectors left to right
  case class SlideIter(v: Expr, size: Int, step: Int) extends Expr {
    val getType: TypeTree = v.getType match {
      case VectorType(t) => ??? // TODO how to represent a list of vectors? -> Matrix?
      case MatrixType(t) => ??? // TODO represent a matrix of matrices
    }
    private lazy val _hash = (getClass, v, size, step).##
    override def hashCode: Int = _hash
  }

  // DSL: def slideReduce(size:Int, step: Int)(fnc: (Vector) => Real): Vector = ??? // apply sliding window to the vector, map over sub-vectors, record the reduced value
  case class SlideReduceIter(v: Expr, size: Expr, step: Expr, fnc: Lambda) extends Expr {
    assert(size.getType == Int32Type, s"$size has a wrong type. Wrong arguments to _.slideReduce($size,$step)(...)")
    assert(step.getType == Int32Type, s"$step has a wrong type. Wrong arguments to _.slideReduce($size,$step)(...)")
    val getType: TypeTree = v.getType match {
      case VectorType(_) => VectorType(Seq(fnc.returnType))
      case MatrixType(_) => MatrixType(Seq(fnc.returnType))
    }
    private lazy val _hash = (getClass, v, size, step, fnc).##
    override def hashCode: Int = _hash
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
