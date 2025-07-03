// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package lang

import scala.collection.immutable.Seq
import scala.util.control.Breaks.{break, breakable}

import Extractors._
import Trees._
import Constructors._
import Identifiers._
import daisy.lang.Trees.RealLiteral.{one, two, zero}
import daisy.tools.{DivisionByZeroException, Rational}
import daisy.lang.Types.{FinitePrecisionType, MatrixType, RealType, VectorType}
import daisy.tools.FinitePrecision.{FixedPrecision, FloatPrecision, MatrixFloat64, VectorFloat64}
import daisy.tools.Sign.Sign
import daisy.tools.{DSAbstraction, Index, MatrixIndex, Rational, Sign}

import scala.annotation.tailrec

/**
  Various useful functions for manipulating trees.
 */
object TreeOps {

  /** Does a right tree fold
   *
   * A right tree fold applies the input function to the subnodes first (from left
   * to right), and combine the results along with the current node value.
   *
   * @param f a function that takes the current node and the seq
   *        of results form the subtrees.
   * @param e The Expr on which to apply the fold.
   * @return The expression after applying `f` on all subtrees.
   * @note the computation is lazy, hence you should not rely on side-effects of `f`
   */
  def fold[T](f: (Expr, Seq[T]) => T)(e: Expr): T = {
    val rec = fold(f) _
    val Operator(es, _) = e

    // Usages of views makes the computation lazy. (which is useful for
    // contains-like operations)
    f(e, Seq(es.view.map[T](rec).toSeq: _*))
    // f(e, es.map(rec))
  }

  /** Does a right tree fold including operations on data structures
   *
   * A right tree fold applies the input function to the subnodes first (from left
   * to right), and combine the results along with the current node value.
   *
   * @param f a function that takes the current node and the seq
   *        of results form the subtrees.
   * @param e The (potentially DSOperation) Expr on which to apply the fold.
   * @return The expression after applying `f` on all subtrees.
   * @note the computation is lazy, hence you should not rely on side-effects of `f`
   */
  def foldDS[T](f: (Expr, Seq[T]) => T)(e: Expr): T = {
    val rec = foldDS(f) _

    // Usages of views makes the computation lazy. (which is useful for
    // contains-like operations)
    e match {
      case DSOperations(es,_) =>
        f(e, Seq(es.view.map[T](rec).toSeq: _*))
      case Operator(es,_) =>
        f(e, Seq(es.view.map[T](rec).toSeq: _*))
    }
    // f(e, es.map(rec))
  }

  /** Pre-traversal of the tree.
   *
   * Invokes the input function on every node '''before''' visiting
   * children. Traverse children from left to right subtrees.
   *
   * e.g.
   * {{{
   *   Add(a, Minus(b, c))
   * }}}
   * will yield, in order:
   * {{{
   *   f(Add(a, Minus(b, c))); f(a); f(Minus(b, c)); f(b); f(c)
   * }}}
   *
   * @param f a function to apply on each node of the expression
   * @param e the expression to traverse
   */
  def preTraversal(f: Expr => Unit)(e: Expr): Unit = {
    val rec = preTraversal(f) _
    val Operator(es, _) = e
    f(e)
    es.foreach(rec)
  }


  /** Post-traversal of the tree.
   *
   * Invokes the input function on every node '''after''' visiting
   * children.
   *
   * e.g.
   * {{{
   *   Add(a, Minus(b, c))
   * }}}
   * will yield, in order:
   * {{{
   *   f(a), f(b), f(c), f(Minus(b, c)), f(Add(a, Minus(b, c)))
   * }}}
   *
   * @param f a function to apply on each node of the expression
   * @param e the expression to traverse
   */
  def postTraversal(f: Expr => Unit)(e: Expr): Unit = {
    val rec = postTraversal(f) _
    val Operator(es, _) = e
    es.foreach(rec)
    f(e)
  }

  /** Post-transformation of the tree.
   *
   * Takes a partial function of replacements.
   * Substitutes '''after''' recursing down the trees.
   *
   * Supports two modes :
   *
   *   - If applyRec is false (default), will only substitute once on each level.
   *   e.g.
   *   {{{
   *     Add(a, Minus(b, c)) with replacements: Minus(b,c) -> z, Minus(e,c) -> d, b -> e
   *   }}}
   *   will yield:
   *   {{{
   *     Add(a, Minus(e, c))
   *   }}}
   *
   *   - If applyRec is true, it will substitute multiple times on each level:
   *   e.g.
   *   {{{
   *     Add(a, Minus(b, c)) with replacements: Minus(e,c) -> d, b -> e, d -> f
   *   }}}
   *   will yield:
   *   {{{
   *     Add(a, f)
   *   }}}
   *
   * @note The mode with applyRec true can diverge if f is not well formed (i.e. not convergent)
   */
  def postMap(f: Expr => Option[Expr], applyRec: Boolean = false)(e: Expr): Expr =
    if (applyRec) {
      var prev = e
      var curr = postMap(f,false)(e)
      while (prev != curr) {
        prev = curr
        curr = postMap(f,false)(curr)
      }
      curr
    } else {
      val rec = postMap(f, applyRec) _

      val Operator(es, builder) = e
      val newEs = es.map(rec)
      val newV = {
        if ((newEs zip es).exists { case (bef, aft) => aft ne bef }) {
          builder(newEs)
        } else {
          e
        }
      }
      f(newV) getOrElse newV
    }

  // Returns all sub-expression in an expression in a list
  def getSubExpr(e: Expr): Seq[Expr] = {
    val Operator(es, _) = e
    if(es.size >0){
      es.flatMap(getSubExpr(_)) :+ e
    } else {
      Seq()
    }
  }


  /** Checks if the predicate holds in some sub-expression */
  def exists(matcher: PartialFunction[Expr, Boolean])(e: Expr): Boolean = {
    fold[Boolean]({(e, subs) => PartialFunction.cond(e)(matcher) || subs.contains(true)})(e)
  }

  /** Computes the negation of a boolean formula, with some simplifications. */
  def negate(expr: Expr): Expr = {
    // require(expr.getType == BooleanType)
    (expr match {
      case Let(i,b,e) => Let(i,b,negate(e))
      case Not(e) => e
      case Implies(e1,e2) => and(e1, negate(e2))
      case Or(exs) => and(exs map negate: _*)
      case And(exs) => or(exs map negate: _*)
      case LessThan(e1,e2) => GreaterEquals(e1,e2)
      case LessEquals(e1,e2) => GreaterThan(e1,e2)
      case GreaterThan(e1,e2) => LessEquals(e1,e2)
      case GreaterEquals(e1,e2) => LessThan(e1,e2)
      case IfExpr(c,e1,e2) => IfExpr(c, negate(e1), negate(e2))
      case BooleanLiteral(b) => BooleanLiteral(!b)
      case e => Not(e)
    }).setPos(expr)
  }

  /** Replaces bottom-up sub-expressions by looking up for them in a map */
  def replace(substs: PartialFunction[Expr,Expr], applyRec: Boolean = false)(expr: Expr): Expr = {
    postMap(substs.lift, applyRec)(expr)
  }

  /** Returns the set of free variables in an expression */
  def freeVariablesOf(expr: Expr): Set[Identifier] = {
    fold[Set[Identifier]] {
      case (e, subs) =>
        val subvs = subs.flatten.toSet
        e match {
          case Variable(i) => subvs + i
          case Let(i, _, _) => subvs - i
          case Lambda(args, _) => subvs -- args.map(_.id)
          case _ => subvs
        }
    }(expr)
  }

  /** Returns the set of Delta variables in an expression */
  def deltasOf(expr: Expr): Set[Delta] = {
    fold[Set[Delta]] {
      case (e, subs) =>
        val subvs = subs.flatten.toSet
        e match {
          case x @ Delta(i) => subvs + x
          case _ => subvs
        }
    }(expr)
  }

  /** Returns the set of Epsilon variables in an expression */
  def epsilonsOf(expr: Expr): Set[Epsilon] = {
    fold[Set[Epsilon]] {
      case (e, subs) =>
        val subvs = subs.flatten.toSet
        e match {
          case x @ Epsilon(i) => subvs + x
          case _ => subvs
        }
    }(expr)
  }

  @tailrec
  def getLastExpression(e: Expr): Expr = e match {
    case Let(_, _, body) => getLastExpression(body)
    case _ => e
  }

  /** Returns the set of all variables occuring (i.e. used) in an expression */
  def allVariablesOf(expr: Expr): Set[Identifier] = {
    fold[Set[Identifier]] {
      case (e, subs) =>
        val subvs = subs.flatten.toSet
        e match {
          case Variable(i) => subvs + i
          case _ => subvs
        }
    }(expr)
  }

  /** Returns the sequence of variables occuring (i.e. used) in an expression. Includes duplicates!  */
  def allOccurrencesOfVars(expr: Expr): Seq[Variable] = {
    fold[Seq[Variable]] {
      case (e, subs) =>
        val subvs = subs.flatten
        e match {
          case x@Variable(_) => subvs :+ x
          case _ => subvs
        }
    }(expr)
  }

  /** Returns the sequence of Vector-type variables, on which operations occur in an expression. Includes duplicates!  */
  def allOccurrencesOfOpsOnVectors(expr: Expr): Seq[Variable] = {
    foldDS[Seq[Variable]] {
      case (e, subs) =>
        val subvs = subs.flatten
        e match {
          case x@Variable(_) => subvs :+ x
          case _ => subvs
        }
    }(expr)
  }

  // returns all IDs appearing in the program, both used and unused
  def allIDsOf(expr: Expr): Set[Identifier] = {
    fold[Set[Identifier]] {
      case (e, subs) =>
        val subvs = subs.flatten.toSet
        e match {
          case Variable(i) => subvs + i
          case Let(i, _, _) => subvs + i
          case Lambda(args, _) => subvs ++ args.map({case ValDef(i) => i })
          case _ => subvs
        }
    }(expr)
  }

  /**
    A term is an expression which (for our purposes) does not contain
    propositional logic connectives.
   */
  def isBooleanTerm(e: Expr): Boolean = e match {
    case GreaterEquals(_, _) | GreaterThan(_,_) | LessThan(_, _) | LessEquals(_,_) => true
    case Equals(_, _) => true
    case AbsError(_, _) => true
    case BooleanLiteral(_) => true
    case _ => false
  }

  /**
    * Returns size of the tree, counting the operators and the terminal nodes
    */
  def size(e: Expr): Int = e match {
    case Operator(es, _) =>
      es.map(size).sum + 1
  }

  def containsApproxNode(e: Expr): Boolean = e match {
    case ApproxPoly(_,_,_,_) => true
    case Variable(_) => false
    case RealLiteral(_) => false
    case FinitePrecisionLiteral(_, _, _) => false

    case ArithOperator(es, _) => es.map(containsApproxNode).foldLeft(false)((acc, x) => acc || x)

    case IfExpr(cond, thenn, elze) =>
      // conditionals do not contain approx (todo can this happen in the future?)
      containsApproxNode(thenn) || containsApproxNode(elze)

    case Let(_, value, body) =>
      containsApproxNode(value) || containsApproxNode(body)

    case x @ Cast(castedExpr, _) =>
      containsApproxNode(castedExpr)
  }

  /**
    * Returns an inlined expression from a Let expression
    */
  def inline(e: Expr): Expr = e match {
    case Let(id, v, b) =>
      val tmp = inline(b)
      lang.TreeOps.replace(Map(Variable(id) -> inline(v)))(tmp)
    case UMinus(t) =>
      inline(t)
    case Plus(t1, t2) =>
      Plus(inline(t1), inline(t2))
    case Minus(t1, t2) =>
      Minus(inline(t1), inline(t2))
    case Times(t1, t2) =>
      Times(inline(t1), inline(t2))
    case FMA(t1, t2, t3) =>
      FMA(inline(t1), inline(t2), inline(t3))
    case Division(t1, t2) =>
      Division(inline(t1), inline(t2))
    case IntPow(t1, n) =>
      IntPow(inline(t1), n)
    case Sqrt(t) =>
      Sqrt(inline(t))
    case Sin(t) =>
      Sin(inline(t))
    case Cos(t) =>
      Cos(inline(t))
    case Tan(t) =>
      Tan(inline(t))
    case Asin(t) =>
      Asin(inline(t))
    case Acos(t) =>
      Acos(inline(t))
    case Atan(t) =>
      Atan(inline(t))
    case Exp(t) =>
      Exp(inline(t))
    case Log(t) =>
      Log(inline(t))
    case _ => e
  }

  /**
    * Computes partial derivative w.r.t. passed parameter
    *
    * @param e   expression for which derivative is computed
    * @param wrt Delta id w.r.t. which derivative is computed
    * @return expression
    */
  def getPartialDerivative(e: Expr, wrt: Identifier): Expr = e match {
    case Delta(id) if wrt.equals(id) =>
      one
    case Delta(id) => zero
    // for the remainder term we will also compute partial derivative wrt Epsilons
    case Epsilon(id) if wrt.equals(id) =>
      one
    case Epsilon(id) => zero
    case Variable(id) if wrt.equals(id) => one
    case Variable(id) => zero
    case RealLiteral(r) => zero
    case UMinus(in) => UMinus(getPartialDerivative(in, wrt))

    case Plus(x, y) =>
      Plus(getPartialDerivative(x, wrt), getPartialDerivative(y, wrt))

    case Minus(x, y) =>
      Minus(getPartialDerivative(x, wrt), getPartialDerivative(y, wrt))

    case Times(x, y) if containsVariables(x, wrt) && containsVariables(y, wrt) =>
      Plus(Times(x, getPartialDerivative(y, wrt)), Times(getPartialDerivative(x, wrt), y))

    case Times(x, y) if containsVariables(x, wrt) =>
      // y is constant
      Times(getPartialDerivative(x, wrt), y)

    case Times(x, y) if containsVariables(y, wrt) =>
      // x is constant
      Times(x, getPartialDerivative(y, wrt))

    case Times(x, y) =>
      // x, y are both constants
      zero

    case Division(x, y) if containsVariables(x, wrt) && containsVariables(y, wrt) =>
      Division(Minus(Times(getPartialDerivative(x, wrt), y), Times(getPartialDerivative(y, wrt), x)),
        Times(y, y))

    case Division(x, y) if containsVariables(x, wrt) =>
      // y is constant
      Times(Division(one, y), getPartialDerivative(x, wrt))

    case Division(x, y) if containsVariables(y, wrt) =>
      // x is constant
      // (1/y)' = -y' / y^2
      Times(x, Division(UMinus(getPartialDerivative(y, wrt)), Times(y, y)))

    case Division(x, y) => zero

    // TODO fix the Pow if we allow expressions in power
    // case z @ Pow(x, n) if containsVariables(x, wrt) && containsVariables(n, wrt)=>
    //   ???
    // case z @ Pow(x, n) if containsVariables(n, wrt) =>
    //   ???

    case IntPow(x, n) if containsVariables(x, wrt) =>
      assert(n > 1)
      // assert(n.isValidInt)
      if (n == 2) {
        getPartialDerivative(x, wrt)
      } else {
        Times(RealLiteral(Rational(n)),
          IntPow(getPartialDerivative(x, wrt), n - 1))
      }

    // case z @ IntPow(x, n) => zero

    case Sqrt(x) if containsVariables(x, wrt) =>
      Division(getPartialDerivative(x, wrt), Times(two, Sqrt(x)))
    case Sqrt(x) => zero

    case Sin(x) if containsVariables(x, wrt) =>
      Times(getPartialDerivative(x, wrt), Cos(x))
    case Sin(x) => zero

    case Cos(x) if containsVariables(x, wrt) =>
      Times(getPartialDerivative(x, wrt), UMinus(Sin(x)))
    case Cos(x) => zero

    case Tan(x) if containsVariables(x, wrt) =>
      Times(getPartialDerivative(x, wrt), Plus(one, Times(Tan(x), Tan(x))))
    case Tan(x) => zero

    case Exp(x) if containsVariables(x, wrt) =>
      Times(getPartialDerivative(x, wrt), Exp(x))
    case Exp(x) => zero

    case Log(x) if containsVariables(x, wrt) =>
      Division(getPartialDerivative(x, wrt), x)
    case Log(x) => zero

    case Atan(x) if containsVariables(x, wrt) =>
      Division(one, Plus(one, Times(x, x)))
    case Atan(x) => zero

    case Let(x, value, body) if containsVariables(body, wrt) =>
      getPartialDerivative(body, wrt)

    case Let(x, value, body) => zero

    case z => throw new IllegalArgumentException(s"Unknown expression $z. Computing derivative failed")
  }

  /**
   * Computes partial derivative w.r.t. passed parameter
   *
   * @param e   expression for which derivative is computed
   * @param wrt expression w.r.t. which derivative is computed
   * @param savedDerivs list of already computed derivatives
   * @return expression
   */
  def getPartialDerivativeWrtExp(e: Expr, wrt: Expr, savedDerivs: Map[Identifier, List[(Identifier, Expr)]] = Map()): Expr =
    e match {

      case Delta(id) if wrt.equals(Delta(id)) =>
        one
      case Delta(id) => zero
      // for the remainder term we will also compute partial derivative wrt Epsilons //TODO check where this is used
      case Epsilon(id) if wrt.equals(Epsilon(id)) =>
        one
      case Epsilon(id) => zero
      case Variable(id) if wrt.equals(Variable(id)) => one
      case Variable(id) => zero
      case RealLiteral(r) => zero
      case UMinus(in) => UMinus(getPartialDerivativeWrtExp(in, wrt, savedDerivs))

      case Plus(x, y) =>
        Plus(getPartialDerivativeWrtExp(x, wrt, savedDerivs), getPartialDerivativeWrtExp(y, wrt, savedDerivs))

      case Minus(x, y) =>
        Minus(getPartialDerivativeWrtExp(x, wrt, savedDerivs), getPartialDerivativeWrtExp(y, wrt, savedDerivs))

      case Times(x, y) if containsExprs(x, wrt) && containsExprs(y, wrt) =>
        Plus(Times(x, getPartialDerivativeWrtExp(y, wrt, savedDerivs)), Times(getPartialDerivativeWrtExp(x, wrt, savedDerivs), y))

      case Times(x, y) if containsExprs(x, wrt) =>
        // y is constant
        Times(getPartialDerivativeWrtExp(x, wrt, savedDerivs), y)

      case Times(x, y) if containsExprs(y, wrt) =>
        // x is constant
        Times(x, getPartialDerivativeWrtExp(y, wrt, savedDerivs))

      case Times(x, y) =>
        // x, y are both constants
        zero

      case Division(x, y) if containsExprs(x, wrt) && containsExprs(y, wrt) =>
        Division(Minus(Times(getPartialDerivativeWrtExp(x, wrt, savedDerivs), y), Times(getPartialDerivativeWrtExp(y, wrt, savedDerivs), x)),
          Times(y, y))

      case Division(x, y) if containsExprs(x, wrt) =>
        // y is constant
        Times(Division(one, y), getPartialDerivativeWrtExp(x, wrt, savedDerivs))

      case Division(x, y) if containsExprs(y, wrt) =>
        // x is constant
        // (1/y)' = -y' / y^2
        Times(x, Division(UMinus(getPartialDerivativeWrtExp(y, wrt, savedDerivs)), Times(y, y)))

      case Division(x, y) => zero

      // TODO fix the Pow if we allow expressions in power
      // case z @ Pow(x, n) if containsVariables(x, wrt) && containsVariables(n, wrt)=>
      //   ???
      // case z @ Pow(x, n) if containsVariables(n, wrt) =>
      //   ???

      case IntPow(x, n) if containsExprs(x, wrt) =>
        assert(n > 1)
        // assert(n.isValidInt)
        if (n == 2) {
          getPartialDerivativeWrtExp(x, wrt, savedDerivs)
        } else {
          Times(RealLiteral(Rational(n)),
            IntPow(getPartialDerivativeWrtExp(x, wrt, savedDerivs), n - 1))
        }

      // case z @ IntPow(x, n) => zero

      case Sqrt(x) if containsExprs(x, wrt) =>
        Division(getPartialDerivativeWrtExp(x, wrt, savedDerivs), Times(two, Sqrt(x)))
      case Sqrt(x) => zero

      case Sin(x) if containsExprs(x, wrt) =>
        Times(getPartialDerivativeWrtExp(x, wrt, savedDerivs), Cos(x))
      case Sin(x) => zero

      case Cos(x) if containsExprs(x, wrt) =>
        Times(getPartialDerivativeWrtExp(x, wrt, savedDerivs), UMinus(Sin(x)))
      case Cos(x) => zero

      case Tan(x) if containsExprs(x, wrt) =>
        Times(getPartialDerivativeWrtExp(x, wrt, savedDerivs), Plus(one, Times(Tan(x), Tan(x))))
      case Tan(x) => zero

      case Exp(x) if containsExprs(x, wrt) =>
        Times(getPartialDerivativeWrtExp(x, wrt, savedDerivs), Exp(x))
      case Exp(x) => zero

      case Log(x) if containsExprs(x, wrt) =>
        Division(getPartialDerivativeWrtExp(x, wrt, savedDerivs), x)
      case Log(x) => zero

      case Atan(x) if containsExprs(x, wrt) =>
        Division(one, Plus(one, Times(x, x)))
      case Atan(x) => zero

      case f@FunctionInvocation(_, _, _, _)
        if (wrt.equals(f)) => one

      case f@FunctionInvocation(_, _, args, _) => wrt match {
        case eps@Epsilon(_) =>
          args.map { case arg =>
            if (containsExprs(arg, eps)) {
              Times(getPartialDerivativeWrtExp(arg, eps, savedDerivs), getSavedPartialDerivative(f, arg, savedDerivs))
            }
            else{
              zero
            }
          }.reduceOption((x, y) => Plus(x, y)).getOrElse(zero)
           case _ => zero
      }
      /*case FunctionInvocation(_, _, _, _)  => zero*/

      case Let(x, value, body) if containsExprs(body, wrt) =>
        getPartialDerivativeWrtExp(body, wrt)

      case Let(x, value, body) => zero

      case z => throw new IllegalArgumentException(s"Unknown expression $z. Computing derivative failed")
    }

  private def containsVariables(e: Expr, wrt: Identifier): Boolean = lang.TreeOps.exists {
    case Variable(`wrt`) => true
    case FunctionInvocation(`wrt`, _, _, _) => true
  }(e)

  private def containsExprs(e: Expr, wrt: Expr): Boolean = wrt match {
    case v@Variable(_) => exists {
      case x if (x == v) => true
    }(e)
    case f@FunctionInvocation(_, _, _, _) => exists {
      case x if (x == f) => true
    }(e)
    case ep@Epsilon(_) => exists {
      case x if (x == ep) => true
    }(e)
  }

  // returns the saved derivative for the function while replacing vars with the new arguments
  def getSavedPartialDerivative(f: FunctionInvocation,
                                arg: Expr, savedDerivs: Map[Identifier, List[(Identifier, Expr)]]): Expr = {
    val args2Params = f.args.zip(f.params).toMap
    val params2argsMap: PartialFunction[Expr, Expr] =
      f.params.map({ x: ValDef => Variable(x.id) }).zip(f.args).toMap
    val relatedVar = args2Params(arg).id
    val deriv = savedDerivs(f.fdId).toMap.getOrElse(relatedVar, RealLiteral(Rational.zero))
    replace(params2argsMap)(deriv)
  }

  /**
    * Simplifies expression using simple arithmetic rules
    * +, -, *, / with 0 and 1
    *
    * @param ex expression to be simplified
    * @return simplified expression
    */
  def easySimplify(ex: Expr): Expr = {
    var ready = false
    var resExpr = ex

    def simpleRound(e: Expr): Expr = e match {

      case x@FunctionInvocation(_, _, _, _) =>
        x
      case x @Variable(id) => x

      case x @RealLiteral(r) =>
        if (r.<(Rational.zero)) {
          UMinus(RealLiteral(-r))
        } else {
          x
        }
      // Plus with zeros
      case Plus(lhs, rhs) if (lhs == zero) && (rhs == zero) =>
        zero
      case Plus(lhs, rhs) if lhs == zero =>
        simpleRound(rhs)
      case Plus(lhs, rhs) if rhs == zero =>
        simpleRound(lhs)
      case Plus(UMinus(lhs), UMinus(rhs)) if (lhs == zero) && (rhs == zero) =>
        zero
      case Plus(UMinus(lhs), UMinus(rhs)) if lhs == zero =>
        simpleRound(rhs)
      case Plus(UMinus(lhs), UMinus(rhs)) if rhs == zero =>
        simpleRound(lhs)
      case Plus(lhs, UMinus(rhs)) =>
        Minus(lhs, simpleRound(rhs))

      case Plus(lhs, rhs) if expressionsEqual(lhs, rhs) =>
        Times(RealLiteral(Rational.fromReal(2)), simpleRound(lhs))

      case Plus(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn + (rhsIn))
          case _ =>
            Plus(simpleRound(lhs), simpleRound(rhs))
        }

      // Minus with zeros
      case Minus(lhs, rhs) if (lhs == zero) && (rhs == zero) =>
        zero
      case Minus(lhs, rhs) if lhs == zero =>
        UMinus(simpleRound(rhs))
      case Minus(lhs, rhs) if rhs == zero =>
        simpleRound(lhs)
      case Minus(UMinus(lhs), UMinus(rhs)) if (lhs == zero) && (rhs == zero) =>
        zero
      case Minus(UMinus(lhs), UMinus(rhs)) if lhs == zero =>
        UMinus(simpleRound(rhs))
      case Minus(UMinus(lhs), UMinus(rhs)) if lhs == zero =>
        simpleRound(lhs)
      case Minus(lhs, rhs) if expressionsEqual(lhs, rhs) =>
        zero

      case Minus(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn.-(rhsIn))
          case _ =>
            Minus(simpleRound(lhs), simpleRound(rhs))
        }
      // Times with zeros and ones
      case Times(lhs, rhs) if lhs == zero =>
        zero
      case Times(lhs, rhs) if rhs == zero =>
        zero
      case Times(lhs, rhs) if lhs == one =>
        simpleRound(rhs)
      case Times(lhs, rhs) if rhs == one =>
        simpleRound(lhs)
      case Times(UMinus(lhs), rhs) if lhs == one =>
        UMinus(simpleRound(rhs))
      case Times(lhs, UMinus(rhs)) if rhs == one =>
        UMinus(simpleRound(lhs))

      case Times(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn.*(rhsIn))
          case _ =>
            Times(simpleRound(lhs), simpleRound(rhs))
        }

      // Division with zeros and ones
      case Division(lhs, rhs) if lhs == zero =>
        zero
      case Division(lhs, rhs) if rhs == zero =>
        throw DivisionByZeroException("Inside simplify function this should not happen")
      case Division(lhs, rhs) if rhs == one =>
        simpleRound(lhs)
      case Division(lhs, rhs) if lhs.equals(rhs) =>
        one
      case Division(lhs, rhs) if lhs.equals(UMinus(rhs)) =>
        UMinus(one)
      case Division(lhs, rhs) if UMinus(lhs).equals(rhs) =>
        UMinus(one)

      case Division(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn./(rhsIn))
          case (_, Division(lhsIn, rhsIn)) if lhs == one =>
            Division(simpleRound(rhsIn), simpleRound(lhsIn))
          case _ =>
            Division(simpleRound(lhs), simpleRound(rhs))
        }

      case IntPow(lhs, n) =>
        IntPow(simpleRound(lhs), n)

      case UMinus(lhs) =>
        lhs match {
          case UMinus(lhs2) => simpleRound(lhs2)
          case RealLiteral(Rational.zero) => zero
          case Division(lhsIn, rhsIn) => Division(UMinus(simpleRound(lhsIn)), simpleRound(rhsIn))
          case _ => UMinus(simpleRound(lhs))
        }

      case Sqrt(lhs) =>
        Sqrt(simpleRound(lhs))

      case Sin(lhs) =>
        Sin(simpleRound(lhs))

      case Cos(lhs) =>
        Cos(simpleRound(lhs))

      case Acos(lhs) =>
        Acos(simpleRound(lhs))

      case Tan(lhs) =>
        Tan(simpleRound(lhs))

      case Atan(lhs) =>
        Atan(simpleRound(lhs))

      case Exp(lhs) =>
        Exp(simpleRound(lhs))

      case Log(lhs) =>
        Log(simpleRound(lhs))

      case Let(id, value, body) =>
        simpleRound(body)

      case n =>
        throw new IllegalArgumentException("Unknown expression. Simplifying expression failed. " + n.toString)
    }

    // ctx.reporter.debug(s"COMPLETE EXPR is $ex")
    while (!ready) {
      // simplify expression
      val tmpresExpr = simpleRound(resExpr)
      // check if expression has changed after simplifications
      ready = expressionsEqual(resExpr, tmpresExpr)
      resExpr = tmpresExpr
    }
    // ctx.reporter.debug(s"SIMPLIFIED EXPR is $resExpr")
    resExpr
  }

  /**
    * Simplifies expression using simple arithmetic rules
    *
    * @param ex expression to be simplified
    * @return simplified expression
    */
  def moreSimplify(ex: Expr): Expr = {
    var ready = false
    var resExpr = easySimplify(ex)

    def simpleRound(e: Expr): Expr = e match {
      case x @Variable(id) => x
      case x @RealLiteral(r) => x

      case Plus(lhs, rhs) if expressionsEqual(lhs, rhs) =>
        Times(RealLiteral(Rational.fromReal(2)), simpleRound(lhs))

      case Plus(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn + (rhsIn))
          case (_, RealLiteral(rhsIn)) =>
            Plus(rhs, simpleRound(lhs))
          // // a/b + c/b = (a + c)/b
          // case (Division(nom1, denom1), Division(nom2, denom2))
          //   if expressionsEqual(denom1, denom2)=>
          //   Division(Plus(simpleRound(nom1),simpleRound(nom2)),
          //     simpleRound(denom1))
          // // a/b + c/d = ad + bc / bd
          // case (Division(nom1, denom1), Division(nom2, denom2)) =>
          //   Division(Plus(simpleRound(Times(nom1, denom2)),simpleRound(Times(nom2, denom1))),
          //     simpleRound(Times(denom1, denom2)))
          // (a * b) + (a * c) = a * (b + c)
          case (Times(lhs1, rhs1), Times(lhs2, rhs2))
            if expressionsEqual(lhs1, lhs2) =>
            Times(simpleRound(lhs1),
              Plus(simpleRound(rhs1), simpleRound(rhs2)))

          // (a * b) + (c * a) = a * (b + c)
          case (Times(lhs1, rhs1), Times(lhs2, rhs2))
            if expressionsEqual(lhs1, rhs2) =>
            Times(simpleRound(lhs1),
              Plus(simpleRound(rhs1), simpleRound(lhs2)))
          // (a * b) + (c * b) = b * (a + c)
          case (Times(a, b), Times(c, bb))
            if expressionsEqual(b, bb) =>
            Times(Plus(simpleRound(a), simpleRound(c)),
              simpleRound(b))
          // (a * b) + (b * c) = b * (a + c)
          case (Times(a, b), Times(bb, c))
            if expressionsEqual(b, bb) =>
            Times(Plus(simpleRound(a), simpleRound(c)),
              simpleRound(b))
          // (a * b) + (a) = a * (b + 1)
          case (Times(a, b), _)
            if expressionsEqual(a, rhs) =>
            Times(simpleRound(a),
              Plus(simpleRound(b), one))
          // (a * b) + (b) = b * (a + 1)
          case (Times(a, b), _)
            if expressionsEqual(b, rhs) =>
            Times(simpleRound(b),
              Plus(simpleRound(a), one))

          // (a) + (a * b) = a * (b + 1)
          case (_, Times(a, b))
            if expressionsEqual(lhs, a) =>
            Times(simpleRound(a),
              Plus(simpleRound(b), one))
          // (b) + (a * b) = b * (a + 1)
          case (_, Times(a, b))
            if expressionsEqual(lhs, b) =>
            Times(simpleRound(b),
              Plus(simpleRound(a), one))
          case _ =>
            Plus(simpleRound(lhs), simpleRound(rhs))
        }

      case Minus(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn.-(rhsIn))
          case (_, RealLiteral(rhsIn)) =>
            Plus(UMinus(rhs), simpleRound(lhs))
          // TODO copy fro plus (ab)+a etc
          // // a - (-b) = a + b
          // case (_, UMinus(rhsIn)) =>
          //  Plus(lhs, rhsIn)
          // // x - (x + y) = -y
          // case (_, Plus(lhsIn, rhsIn)) if expressionsEqual(lhs, lhsIn) =>
          //  UMinus(simpleRound(rhsIn))
          // // x - (y + x) = -y
          // case (_, Plus(lhsIn, rhsIn)) if expressionsEqual(lhs, rhsIn) =>
          //  UMinus(simpleRound(lhsIn))
          // // (x + y) - x = y
          // case (Plus(lhsIn, rhsIn), _) if expressionsEqual(rhs, lhsIn) =>
          //  simpleRound(rhsIn)
          // // (y + x) - x = y
          // case (Plus(lhsIn, rhsIn), _) if expressionsEqual(rhs, rhsIn) =>
          //  simpleRound(lhsIn)
          // // a/b - c/b = (a - c)/b
          // case (Division(nom1, denom1), Division(nom2, denom2))
          //  if expressionsEqual(denom1, denom2)=>
          //  Division(Minus(simpleRound(nom1),simpleRound(nom2)),
          //    simpleRound(denom1))
          // // a/b - c/d = ad - bc / bd
          // case (Division(nom1, denom1), Division(nom2, denom2)) =>
          //  Division(Minus(simpleRound(Times(nom1, denom2)),simpleRound(Times(nom2, denom1))),
          //    simpleRound(Times(denom1, denom2)))
          // // (a * b) - (a * c) = a * (b - c)
          // case (Times(lhs1, rhs1), Times(lhs2, rhs2))
          //  if expressionsEqual(lhs1, lhs2)=>
          //  Times(simpleRound(lhs1),
          //    Minus(simpleRound(rhs1), simpleRound(rhs2)))
          // // (a * b) - (c * a) = a * (b - c)
          // case (Times(lhs1, rhs1), Times(lhs2, rhs2))
          //  if expressionsEqual(lhs1, rhs2)=>
          //  Times(simpleRound(lhs1),
          //    Minus(simpleRound(rhs1), simpleRound(lhs2)))
          // // (a * b) - (c * b) = b * (a - c)
          // case (Times(lhs1, rhs1), Times(lhs2, rhs2))
          //  if expressionsEqual(rhs1, rhs2)=>
          //  Times(simpleRound(rhs1),
          //    Minus(simpleRound(lhs1), simpleRound(lhs2)))
          // // (a * b) - (b * c) = b * (a - c)
          // case (Times(lhs1, rhs1), Times(lhs2, rhs2))
          //  if expressionsEqual(rhs1, lhs2)=>
          //  Times(simpleRound(rhs1),
          //    Minus(simpleRound(lhs1), simpleRound(rhs2)))
          case _ =>
            Minus(simpleRound(lhs), simpleRound(rhs))
        }

      case Times(lhs, rhs) if expressionsEqual(lhs, rhs) =>
        val tmp = IntPow(simpleRound(lhs), 2)
        tmp

      case Times(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn.*(rhsIn))

          case (Times(_, _), RealLiteral(rhsIn)) =>
            val list = listElements(lhs)
            multiplyRealLiterals(rhsIn, list)

          case (RealLiteral(lhsIn), Times(_, _)) =>
            val list = listElements(rhs)
            multiplyRealLiterals(lhsIn, list)

          // case (UMinus(lhsIn), UMinus(rhsIn)) =>
          //  Times(simpleRound(lhsIn), simpleRound(rhsIn))
          // case (_, UMinus(rhsIn)) =>
          //  UMinus(Times(simpleRound(lhs), simpleRound(rhsIn)))
          // case (UMinus(lhsIn), _) =>
          //  UMinus(Times(simpleRound(lhsIn), simpleRound(rhs)))
          // // a/b * c/a = c/b
          // case (Division(nom1, denom1), Division(nom2, denom2))
          //  if expressionsEqual(nom1, denom2)=>
          //  Division(simpleRound(nom2),
          //    simpleRound(denom1))
          // // a/b * b/c = a/c
          // case (Division(nom1, denom1), Division(nom2, denom2))
          //  if expressionsEqual(nom2, denom1)=>
          //  Division(simpleRound(nom1),
          //    simpleRound(denom2))
          // a/b * c/d = ac / bd
          case (Division(nom1, denom1), Division(nom2, denom2)) =>
            Division(Times(simpleRound(nom1), simpleRound(nom2)),
              Times(simpleRound(denom1), simpleRound(denom2)))
          // 1/b * c = c/b
          case (Division(nom, denom), _) if nom == one =>
            Division(simpleRound(rhs),
              simpleRound(denom))
          // a/b * c = ac/b
          case (Division(nom, denom), _) =>
            Division(Times(simpleRound(nom), simpleRound(rhs)),
              simpleRound(denom))
          // c * a/b= ca/b
          case (_, Division(nom, denom)) =>
            Division(Times(simpleRound(lhs), simpleRound(nom)),
              simpleRound(denom))

          case (Times(lhsIn, rhsIn), _) if expressionsEqual(rhsIn, rhs) =>
            Times(lhsIn, IntPow(simpleRound(rhs), 2))

          case (Times(lhsIn, IntPow(rhsIn, n)), _) if expressionsEqual(rhsIn, rhs) =>
            Times(lhsIn, IntPow(simpleRound(rhs), (n + 1)))

          case (IntPow(lhsIn, n1), IntPow(rhsIn, n2))
            if expressionsEqual(lhsIn, rhsIn) =>
            IntPow(simpleRound(lhsIn), n1 + n2)

          case (IntPow(lhsIn, n), _)
            if expressionsEqual(lhsIn, rhs) =>
            IntPow(simpleRound(lhsIn), n + 1)

          case (_, IntPow(rhsIn, n))
            if expressionsEqual(lhs, rhsIn) =>
            IntPow(simpleRound(rhsIn), n)

          case _ =>
            Times(simpleRound(lhs), simpleRound(rhs))
        }

      case Division(lhs, rhs) =>
        (lhs, rhs) match {
          case (RealLiteral(lhsIn), RealLiteral(rhsIn)) =>
            RealLiteral(lhsIn./(rhsIn))
          case (_, Division(lhsIn, rhsIn)) if lhs == one =>
            Division(simpleRound(rhsIn), simpleRound(lhsIn))
          // a/b / c/d = ad / bc
          case (Division(a, b), Division(c, d)) =>
            // ctx.reporter.warning("swap divisions")
            Division(Times(simpleRound(a), simpleRound(d)),
              Times(simpleRound(b), simpleRound(c)))
          // case (Division(nom, denom), _) =>
          //  Division(simpleRound(nom),
          //    Times(simpleRound(denom), simpleRound(rhs)))
          // case (_, Division(nom, denom)) =>
          //  Division(Times(simpleRound(lhs), simpleRound(denom)),
          //    simpleRound(nom))

          case (Times(_, _), Times(_, _)) =>
            cancelNominators(simpleRound(lhs), simpleRound(rhs))
          case (Times(_, _), IntPow(_, _)) =>
            cancelNominators(simpleRound(lhs), simpleRound(rhs))
          case (IntPow(_, _), Times(_, _)) =>
            cancelNominators(simpleRound(lhs), simpleRound(rhs))
          case (IntPow(_, _), IntPow(_, _)) =>
            cancelNominators(simpleRound(lhs), simpleRound(rhs))
          case _ =>
            Division(simpleRound(lhs), simpleRound(rhs))
        }

      case IntPow(IntPow(lhs, n1), n) =>
        IntPow(simpleRound(lhs), n * n1)

      case IntPow(lhs, n) =>
        IntPow(simpleRound(lhs), n)

      case UMinus(lhs) =>
        lhs match {
          case UMinus(lhs2) => simpleRound(lhs2)
          case RealLiteral(Rational.zero) => zero
          case Division(lhsIn, rhsIn) => Division(UMinus(simpleRound(lhsIn)), simpleRound(rhsIn))
          case _ => UMinus(simpleRound(lhs))
        }

      case Sqrt(lhs) =>
        Sqrt(simpleRound(lhs))

      case Sin(lhs) =>
        Sin(simpleRound(lhs))

      case Cos(lhs) =>
        Cos(simpleRound(lhs))

      case Tan(lhs) =>
        Tan(simpleRound(lhs))

      case Exp(lhs) =>
        Exp(simpleRound(lhs))

      case Log(lhs) =>
        Log(simpleRound(lhs))

      case Let(id, value, body) =>
        simpleRound(body)

      case n =>
        throw new IllegalArgumentException("Unknown expression. Simplifying expression failed. " + n.toString)
    }

    //ctx.reporter.debug(s"COMPLETE EXPR is $resExpr")
    while (!ready) {
      // simplify expression
      val tmpresExpr = simpleRound(resExpr)
      // check if expression has changed after simplifications
      ready = expressionsEqual(resExpr, tmpresExpr)
      resExpr = tmpresExpr
    }
    //ctx.reporter.debug(s"SIMPLIFIED EXPR is $resExpr")
    resExpr
  }

  /**
    * Compares two expressions
    *
    * @param e1 - first expression for comparison
    * @param e2 - second expression for comparison
    * @return true - if expressions are equal, false - otherwise
    */
  private def expressionsEqual(e1: Expr, e2: Expr): Boolean = (e1, e2) match {
    case (FunctionInvocation(id1, params1, args1, returnType1),FunctionInvocation(id2, params2, args2, returnType2)) =>
      id1.equals(id2) && params1.equals(params2) && args1.equals(args2) &&  returnType1.equals(returnType2)
    case (RealLiteral(r1), RealLiteral(r2)) => r1.equals(r2)
    case (Delta(r1), Delta(r2)) => r1.equals(r2)
    case (Epsilon(r1), Epsilon(r2)) => r1.equals(r2)
    case (Variable(r1), Variable(r2)) => r1.equals(r2)
    case (Sqrt(r1), Sqrt(r2)) => expressionsEqual(r1, r2)
    case (Sin(r1), Sin(r2)) => expressionsEqual(r1, r2)
    case (Cos(r1), Cos(r2)) => expressionsEqual(r1, r2)
    case (Tan(r1), Tan(r2)) => expressionsEqual(r1, r2)
    case (Atan(r1), Atan(r2)) => expressionsEqual(r1, r2)
    case (Exp(r1), Exp(r2)) => expressionsEqual(r1, r2)
    case (Log(r1), Log(r2)) => expressionsEqual(r1, r2)
    case (UMinus(r1), UMinus(r2)) => expressionsEqual(r1, r2)
    case (Let(id1, val1, body1), Let(id2, val2, body2)) =>
      id1.equals(id2) && val1.equals(val2) && expressionsEqual(body1, body2)
    // FIXME handle different order of terms inside Plus()
    // case (Plus(lhs1, Plus(lhsIn1, rhsIn1)), Plus(Plus(lhsIn2, rhsIn2), rhs2)) =>
    // expressionsEqual(lhs1, lhsIn2) && expressionsEqual(lhsIn1, lhsIn2) && expressionsEqual(rhsIn1, rhs2)
    case (Plus(lhs1, rhs1), Plus(lhs2, rhs2)) =>
      (expressionsEqual(lhs1, lhs2) && expressionsEqual(rhs1, rhs2)) ||
        (expressionsEqual(lhs1, rhs2) && expressionsEqual(rhs1, lhs2))
    // FIXME handle different order of terms inside Times()
    // case (Times(lhs1, Times(lhsIn1, rhsIn1)), Times(Times(lhsIn2, rhsIn2), rhs2)) =>
    // expressionsEqual(lhs1, lhsIn2) && expressionsEqual(lhsIn1, lhsIn2) && expressionsEqual(rhsIn1, rhs2)
    case (Times(lhs1, rhs1), Times(lhs2, rhs2)) =>
      (expressionsEqual(lhs1, lhs2) && expressionsEqual(rhs1, rhs2)) ||
        (expressionsEqual(lhs1, rhs2) && expressionsEqual(rhs1, lhs2))
    case (Minus(lhs1, rhs1), Minus(lhs2, rhs2)) =>
      // FIXME handle reverse order
      expressionsEqual(lhs1, lhs2) && expressionsEqual(rhs1, rhs2)
    case (Division(lhs1, rhs1), Division(lhs2, rhs2)) =>
      expressionsEqual(lhs1, lhs2) && expressionsEqual(rhs1, rhs2)
    case (IntPow(x1, n1), IntPow(x2, n2)) =>
      expressionsEqual(x1, x2) && n1 == n2
    case _ => false
  }

  private def multiplyRealLiterals(ra: Rational, list: Seq[Expr]): Expr = {
    // ctx.reporter.warning(s"terms to multiply $ra and $list")
    var newMultiplier = ra
    list.foreach {
      case RealLiteral(r) => newMultiplier = r.*(newMultiplier)
      case _ => newMultiplier = newMultiplier
    }
    var res: Expr = RealLiteral(newMultiplier)
    list.foreach {
      case RealLiteral(r) => one
      case y => res = Times(res, y)
    }

    // ctx.reporter.warning(s"terms after multiplying $res")
    res
  }

  /**
    * Collects elements of the expression into the sequence
    *
    * @param ex expression to be split into multiplication terms
    * @return list of expressions that were multiplied
    */
  def listElements(ex: Expr): Seq[Expr] = ex match {
    case Times(lhs, UMinus(rhs)) =>
      listElements(lhs).++(listElements(rhs)) :+ UMinus(one)
    case Times(UMinus(lhs), rhs) =>
      listElements(lhs).++(listElements(rhs)) :+ UMinus(one)
    case Times(lhs, rhs) =>
      listElements(lhs).++(listElements(rhs))
    case UMinus(Times(lhs, rhs)) =>
      listElements(UMinus(lhs)).++(listElements(rhs))
    case IntPow(x, n) =>
      var tmp: Seq[Expr] = Seq.empty
      for (i <- 0 until n) {
        tmp = listElements(x).++(tmp)
      }
      tmp
    case UMinus(IntPow(x, n)) =>
      var tmp: Seq[Expr] = Seq.empty
      for (i <- 0 until n) {
        tmp = listElements(x).++(tmp)
      }
      // ctx.reporter.warning(s"List elements $tmp")
      tmp :+ UMinus(one)
    case x =>
      List(x)
  }

  /**
    * Cancels repeating terms in nominator and denominator of Division expression
    *
    * @param nom   nominator expression
    * @param denom demominator expression
    * @return simplified expression Division
    */
  def cancelNominators(nom: Expr, denom: Expr): Expr = {
    // FIXME function not used. contains bugs?
    // ctx.reporter.warning("=== BEFOREEXPRESSION ==== list Nominators " + nom)
    // ctx.reporter.warning("=== BEFOREEXPRESSION ==== list Denominators " + denom)

    // get lists of the elements in nominator and denominator
    val noms = listElements(nom)
    val denoms = listElements(denom)

    // ctx.reporter.warning(s"===BEFORE==== list Nominators $noms")
    // ctx.reporter.warning(s"===BEFORE==== list Denominators $denoms")

    // get repeating terms in nominator and denominator
    var newNomList = noms
    var newDenomList = denoms
    for (n <- newNomList) {
      breakable {
        var found = false
        for (d <- newDenomList) {
          found = n.equals(d)
          if (found) {
            // remove the element from denominator
            newDenomList = newDenomList.diff(List(d))
            newNomList = newNomList.diff(List(n))
            break()
          }
        }
      }
    }

    // ctx.reporter.warning(s"===AFTER==== Nominator LIST $newNomList")
    // ctx.reporter.warning(s"===AFTER==== Denominator LIST $newDenomList")

    // multiply the terms that left
    val newNom = multiply(newNomList)
    val newDenom = multiply(newDenomList)
    // ctx.reporter.warning(s"===AFTER==== Nominator $newNom")
    // ctx.reporter.warning(s"===AFTER==== Denominator $newDenom")

    if (newDenom.equals(RealLiteral(Rational.one))) {
      newNom
    } else {
      Division(newNom, newDenom)
    }
  }

  /**
    * Multiplies terms in the Seq
    *
    * @param lst - sequence of terms to be multiplied
    * @return expression Times(Times(..., term2), term1)
    */
  private def multiply(lst: Seq[Expr]): Expr = {
    if (lst.isEmpty) {
      one
    } else {
      if (lst.lengthCompare(1) == 0) {
        lst.head
      } else {
        var tmp: Expr = one
        lst.foreach(x => {
          tmp = if (x == UMinus(one)) UMinus(tmp) else Times(tmp, x)
        })
        // here IntPow will come back
        easySimplify(tmp)
      }
    }
  }

  def findDistance(subEx: Expr, expr: Expr): Int = {
    if (expr == subEx) {
      0
    } else {
      expr match {
        case ArithOperator(Seq(l, r), _) =>
          var dist = findDistance(subEx, l)
          if (dist >= 0) {
            dist + 1
          } else {
            dist = findDistance(subEx, r)
            if (dist >= 0) {
              dist + 1
            } else {
              dist
            }
          }
        case ArithOperator(Seq(l), _) =>
          val dist = findDistance(subEx, l)
          if (dist >= 0) {
            dist + 1
          } else {
            dist
          }
        case Variable(_) | Delta(_) | Epsilon(_) | RealLiteral(_) =>
          -1
      }
    }
  }

  /*
   * Naive unrolling of folds over vectors: replace by nested let statements,
   * where each temporary variable uses a vector element at index i.
   * Note: Reaches StackOverflow already at 350 elements on a simple expression x.fold(0.0)((acc: Real, i: Real) => acc + i)
   * @param v vector to fold
   * @param size number of elements in v
   * @param init initial value for fold
   * @param args arguments of the lambda function
   * @param body lambda function performing the fold
   * @return let-expression equivalent to the unrolled fold over v
   */
  def unrollFoldwLetsOnVector(v: Expr, fromInd: Int, toInd: Int, init: Expr, args: Seq[ValDef], body: Expr): Expr = {
    val accId = args.head.id
    val xId = args(1).id

    def getIdAndBody(ind: Int): (Identifier,Expr) = {
      val newId = FreshIdentifier(s"acc$ind", RealType)
      val newValue = replace{
        case Variable(id) if id == accId => if (ind == fromInd) init else Variable(newId)
        case Variable(id) if id == xId => VectorElement(v, Int32Literal(ind))
      }(body)
      (newId, newValue)
    }

    @tailrec
    def buildRecursiveLet(cInd: Int, lastID: Identifier, accBody: Expr): Expr = {
      val nextI = cInd -1
      if (nextI < fromInd)
        accBody // acc is replaced with init, no new IDs are used
      else {
        val (newId, newBody) = getIdAndBody(nextI)
        val newLet = Let(lastID, newBody, accBody)
        buildRecursiveLet(nextI, newId, newLet)
      }
    }

    val (newId, newBody) = getIdAndBody(toInd)
    buildRecursiveLet(toInd, newId, newBody)
  }

  /**
   * Unrolling of folds over vectors: replace by an arithmetic expression,
   * each acc is an unrolled expression from the previous step, x (current) - vector element x.at(i).
   * Note: Reaches StackOverflow already at 400 elements on a simple expression x.fold(0.0)((acc: Real, i: Real) => acc + i)
   * @param v vector to fold
   * @param fromInd start index of the vector to unroll
   * @param toInd end index of the vector to unroll (not necessarily the full vector)
   * @param init initial value for fold
   * @param args arguments of the lambda function
   * @param body lambda function performing the fold
   * @return let-expression equivalent to the unrolled fold over v
   * @return
   */
  def unrollFoldOnVector(v: Expr, fromInd: Int, toInd: Int, init: Expr, args: Seq[ValDef], body: Expr): Expr = {
    val accId = args.head.id
    val xId = args(1).id

    val unrolled = (fromInd to toInd).foldLeft(init)({ case (acc, x) =>
      replace {
        case Variable(id) if id == accId => acc
        case Variable(id) if id == xId => VectorElement(v, Int32Literal(x))
      }(body)
    })
    unrolled
  }


  /**
   * Inlines folds over vectors: replace by an arithmetic expression,
   * each acc is an unrolled expression from the previous step, x (current) - vector element x.at(i).
   * @param tmp temporary variable to replace the current vector element
   * @param init initial value for fold
   * @param args arguments of the lambda function
   * @param body lambda function performing the fold
   * @return let-expression equivalent to the unrolled fold over v
   * @return
   */
  def inlineFold(tmp: Expr, init: Expr, args: Seq[ValDef], body: Expr): Expr = {
    val accId = args.head.id
    val xId = args(1).id

    replace {
        case Variable(id) if id == accId => init
        case Variable(id) if id == xId => tmp
      }(body)
  }


  /**
   * Naive unrolling of folds over matrices: replace by nested let statements,
   * where each temporary variable uses a matrix row at index i.
   * Note: Reaches StackOverflow already at 350 elements on a simple expression x.fold(0.0)((acc: Real, i: Real) => acc + i)
   * @param m matrix to fold
   * @param size number of rows in m
   * @param init initial value for fold
   * @param args arguments of the lambda function
   * @param body lambda function performing the fold
   * @return let-expression equivalent to the unrolled fold over v
   */
  def unrollFoldwLetsOnMatrix(m: Expr, fromInd:Int, toInd: Int, init: Expr, args: Seq[ValDef], body: Expr): Expr = {
    val accId = args.head.id
    val xId = args(1).id

    def getIdAndBody(row: Int): (Identifier,Expr) = {
      val newId = FreshIdentifier(s"acc${row-1}", accId.getType)
      val newValue = replace{
        case Variable(id) if id == accId => if (row == fromInd) init else VectorLiteral(newId)
        case Variable(id) if id == xId => RowOfMatrix(m, Int32Literal(row))
        case DSOperations(seq, fnc) if seq.nonEmpty && isVector(seq.head) && seq.head == Variable(accId) =>
          val acc = if (row == fromInd) init else VectorLiteral(newId)
          fnc(acc +: seq.tail)
        case DSOperations(seq, fnc) if seq.nonEmpty && isVector(seq.head) && seq.head == Variable(xId) =>
          fnc(RowOfMatrix(m, Int32Literal(row)) +: seq.tail)
      }(body)
      (newId, newValue)
    }

    @tailrec
    def buildRecursiveLet(cRow: Int, lastID: Identifier, accBody: Expr): Expr = {
      val nextR = cRow - 1
      if (nextR == fromInd-1)
        accBody // acc is replaced with init, no new IDs are used
      else {
        val (newId, newBody) = getIdAndBody(nextR)
        val newLet = Let(lastID, newBody, accBody)
        buildRecursiveLet(nextR, newId, newLet)
      }
    }

    val (newId, newBody) = getIdAndBody(toInd)
    buildRecursiveLet(toInd, newId, newBody)
  }

  /**
   * Unrolling of folds over matrices: replace by an arithmetic expression,
   * each acc is an unrolled expression from the previous step, x (current) - row x.row(i).
   * @param m matrix which rows are folded
   * @param fromInd start index of the vector to unroll
   * @param toInd end index of the vector to unroll (not necessarily the full vector)
   * @param init initial value for fold
   * @param args arguments of the lambda function
   * @param body lambda function performing the fold
   * @return let-expression equivalent to the unrolled fold over v
   * @return
   */
  def unrollFoldOnMatrix(m: Expr, fromInd: Int, toInd: Int, init: Expr, args: Seq[ValDef], body: Expr): Expr = {
    val accId = args.head.id
    val xId = args(1).id

    val unrolled = (fromInd to toInd).foldLeft(init)({ case (acc, x) =>
      replace {
        case Variable(id) if id == accId => acc
        case Variable(id) if id == xId => RowOfMatrix(m, Int32Literal(x))
        case DSOperations(seq, fnc) if seq.nonEmpty && isVector(seq.head) && seq.head == Variable(accId) =>
          fnc(acc +: seq.tail)
        case DSOperations(seq, fnc) if seq.nonEmpty && isVector(seq.head) && seq.head == Variable(xId) =>
          fnc(RowOfMatrix(m, Int32Literal(x)) +: seq.tail)
      }(body)
    })
    unrolled
  }
  /**
   * Naive unrolling of folds over matrices: replace by nested let statements,
   * where each temporary variable uses a matrix element at index (i,j).
   * Note: Reaches StackOverflow already at 350 elements on a simple expression x.fold(0.0)((acc: Real, i: Real) => acc + i)
   * @param m matrix to fold
   * @param fromInd start index of the considered set of matrix elements
   * @param toInd end index
   * @param numCols total number of columns in m
   * @param init initial value for fold
   * @param args arguments of the lambda function
   * @param body lambda function performing the fold
   * @return let-expression equivalent to the unrolled fold over v
   */
  def unrollFoldElemsWLetsOnMatrix(m: Expr, fromInd: MatrixIndex, toInd: MatrixIndex, numCols: Int, init: Expr, args: Seq[ValDef], body: Expr): Expr = {
    val accId = args.head.id
    val xId = args(1).id

    def getIdAndBody(row: Int, col: Int): (Identifier,Expr) = {
      val newId = FreshIdentifier(s"acc${row}_$col", RealType)
      val newValue = replace{
        case Variable(id) if id == accId => if (col == fromInd.j && row == fromInd.i) init else Variable(newId)
        case Variable(id) if id == xId => MatrixElement(m, Int32Literal(row), Int32Literal(col))
      }(body)
      (newId, newValue)
    }

    @tailrec
    def buildRecursiveLet(cRow: Int, cCol: Int, lastID: Identifier, accBody: Expr): Expr = {
        val nextI = nextMatrixIndex(cRow, cCol, numCols)
        if (nextI.isEmpty)
          accBody // acc is replaced with init, no new IDs are used
        else {
          val (nextR, nextC) = nextI.get
          if (MatrixIndex(nextR, nextC) < fromInd)
            accBody
          else {
            val (newId, newBody) = getIdAndBody(nextR, nextC)
            val newLet = Let(lastID, newBody, accBody)
            buildRecursiveLet(nextR, nextC, newId, newLet)
          }
        }
      }

    val (newId, newBody) = getIdAndBody(toInd.i, toInd.j)
    buildRecursiveLet(toInd.i, toInd.j, newId, newBody)
  }

  /**
   * Unrolling of folds over matrix elementa: replace by an arithmetic expression,
   * each acc is an unrolled expression from the previous step, x (current) - row x.row(i).
   * @param m matrix which elements are folded
   * @param indices sequence of indices, for which we unroll
   * @param init initial value for fold
   * @param args arguments of the lambda function
   * @param body lambda function performing the fold
   * @return let-expression equivalent to the unrolled fold over m
   * @return
   */
  def unrollFoldOnMatrixElts(m: Expr, indices: Seq[Index], init: Expr, args: Seq[ValDef], body: Expr): Expr = {
    val accId = args.head.id
    val xId = args(1).id

    val unrolled = indices.foldLeft(init)({ case (acc, x) =>
      val mIndex = x.asInstanceOf[MatrixIndex]
      replace {
        case Variable(id) if id == accId => acc
        case Variable(id) if id == xId => MatrixElement(m, Int32Literal(mIndex.i), Int32Literal(mIndex.j))
      }(body)
    })
    unrolled
  }

  /**
   * Iterate over elements of a matrix from the last element (numRows-1,numCols-1) to the first (0,0),
     first list elements of one row (from larger index to smaller)
   * @param cRow current row index
   * @param cCol current column index
   * @param numCols number of columns in the matrix
   * @return [Option] Some(i,j) an index of the next element, None if reached the (0,0)
   */
  def nextMatrixIndex(cRow: Int, cCol: Int, numCols: Int): Option[(Int, Int)] = {
    assert(cRow>=0 && cCol>=0)
    if (cRow == 0 && cCol ==0) None
    else if (cCol == 0) Some((cRow-1, numCols-1)) // one row higher, start from the last column
    else Some(cRow, cCol-1)
  }

  /**
   * Determines whether an expression operates on data structures
   * or contains sub-expressions that do
   *
   * @param e expression
   * @return true/false
   */
  def isDsExpr(e: Expr): Boolean = e match {
    case DSOperations(_) => true

    case Variable(_) => false // all other variables
    case ElemFnc(x, _) => isDsExpr(x)
    case Operator(seq, _) => seq.exists(isDsExpr)
    case x => throw new DaisyFatalError(Some(s"Unmatched expression $x"))
  }

  /**
   * Determines whether an expression has a vector or matrix type
   * @param e expression for which the type is checked
   * @return true/false
   */
  def hasDsType(e: Expr): Boolean = e.getType match {
    case VectorType(_) => true
    case MatrixType(_) => true
    case _ => false
  }

  def isVector(ds: Expr): Boolean = ds.getType match {
    case VectorType(_) => true
    case FinitePrecisionType(VectorFloat64) => true
    case _ => false
  }
  def isMatrix(ds: Expr): Boolean = ds.getType match {
    case MatrixType(_) => true
    case FinitePrecisionType(MatrixFloat64) => true
    case _ => false
  }

  def isScalar(ds: Expr): Boolean = ds.getType match {
    case RealType => true
    case FinitePrecisionType(FloatPrecision(_)) => true
    case FinitePrecisionType(FixedPrecision(_)) => true
    case _ => false
  }
  // todo also add var1*var2 if one of the vars is a constant with respect to the loop body (?)
  def isLinear(e:Expr): Boolean = e match {
    case Times(Variable(_), RealLiteral(_)) => true
    case Times(RealLiteral(_), Variable(_)) => true
    case Variable(_) => true
    case RealLiteral(_) => true
    case UMinus(_) => true
    case Plus(lhs, rhs) => isLinear(lhs) && isLinear(rhs)
    case Minus(lhs, rhs) => isLinear(lhs) && isLinear(rhs)
    case _ => false
  }

  /**
   * Returns the linear coefficients (needed for the closed-form range computation).
   * Limitations: currently can only handle expressions a*x +|- b*acc +|- c,
   * where c is either a constant, or a variable
   * @param e - expression to parse
   * @param xId - id of the iterator x
   * @param accId - id of the accumulator acc
   * @param coefs accumulator for the coefficients
   * @return Map("a" -> coefficient before x, "b" -> coefficient before acc, "c" - constant)
   */
  def getLinearCoefficients(e: Expr, xId: Identifier, accId: Identifier, coefs: Map[String, Expr], sign: Sign = Sign.Positive): Map[String, Expr] = e match {
    case Variable(id) if id == xId =>
      val a =
        if (sign == Sign.Positive)
          RealLiteral(Rational.one)
        else RealLiteral(Rational(-1))
      coefs + ("a"-> a)
    case UMinus(Variable(id)) if id == xId =>
      val a = if (sign == Sign.Positive)
        RealLiteral(Rational(-1))
      else RealLiteral(Rational.one)
      coefs + ("a"-> a)
    case Variable(id) if id == accId =>
      val b = if (sign == Sign.Positive)
          RealLiteral(Rational.one)
        else RealLiteral(Rational(-1))
      coefs + ("b"-> b)
    case UMinus(Variable(id)) if id == accId =>
      val b = if (sign == Sign.Positive)
        RealLiteral(Rational(-1))
      else RealLiteral(Rational.one)
      coefs + ("b"-> b)
    case c@Variable(_) =>
      if (sign == Sign.Positive)
        coefs + ("c"-> c)
        else coefs + ("c"-> UMinus(c))
    case c@UMinus(v@Variable(_)) =>
      if (sign == Sign.Positive)
        coefs + ("c"-> c)
      else coefs + ("c"-> v)
    // coefs \neq one
    case Times(a@RealLiteral(r), Variable(id)) if id == xId =>
      if (sign == Sign.Positive)
        coefs + ("a"-> a)
      else coefs + ("a" -> RealLiteral(-r))
    case Times(Variable(id), a@RealLiteral(r)) if id == xId =>
      if (sign == Sign.Positive)
        coefs + ("a"-> a)
      else coefs + ("a" -> RealLiteral(-r))
    case Times(b@RealLiteral(r), Variable(id)) if id == accId =>
      if (sign == Sign.Positive)
        coefs + ("b"-> b)
      else coefs + ("b" -> RealLiteral(-r))
    case Times(Variable(id), b@RealLiteral(r)) if id == accId =>
      if (sign == Sign.Positive)
        coefs + ("b"-> b)
      else coefs + ("b" -> RealLiteral(-r))

    case Plus(lhs, c@RealLiteral(_)) => getLinearCoefficients(lhs, xId, accId, coefs + ("c" -> c), sign)
    case Plus(c@RealLiteral(_), rhs) => getLinearCoefficients(rhs, xId, accId, coefs + ("c" -> c), sign)
    case Minus(lhs, RealLiteral(r)) => getLinearCoefficients(lhs, xId, accId, coefs + ("c" -> RealLiteral(-r)), sign)
    case Minus(c@RealLiteral(_), rhs) => getLinearCoefficients(rhs, xId, accId, coefs + ("c" -> c), sign)

    case Plus(lhs, rhs) => getLinearCoefficients(lhs, xId, accId, coefs, sign) ++ getLinearCoefficients(rhs, xId, accId, coefs, sign)
    case Minus(lhs, rhs) => getLinearCoefficients(lhs, xId, accId, coefs, sign) ++ getLinearCoefficients(rhs, xId, accId, coefs, Sign.negate(sign))
  }
}
