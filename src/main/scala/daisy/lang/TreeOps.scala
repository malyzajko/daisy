// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package lang

import scala.collection.immutable.Seq
import Extractors._
import Trees._
import Constructors._
import daisy.lang.Types.{FinitePrecisionType, MatrixType, RealType, VectorType}
import daisy.tools.FinitePrecision.{FixedPrecision, FloatPrecision, MatrixFloat64, VectorFloat64}
import daisy.tools.Sign.Sign
import daisy.tools.{DSAbstraction, Index, MatrixIndex, Rational, Sign}
import lang.Identifiers._

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
