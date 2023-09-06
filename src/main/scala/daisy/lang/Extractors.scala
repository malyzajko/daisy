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
      case Asin(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Asin(es.head)))
      case Acos(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Acos(es.head)))
      case Atan(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Atan(es.head)))
      case Exp(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Exp(es.head)))
      case Log(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Log(es.head)))
      case Approx(org, t, errorBound, errorMultiplier, functionName, dd) =>
        Some((Seq(t), (es: Seq[Expr]) => Approx(org, es.head, errorBound, errorMultiplier, functionName, dd)))

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
      case Asin(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Asin(es.head)))
      case Acos(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Acos(es.head)))
      case Atan(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Atan(es.head)))
      case Exp(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Exp(es.head)))
      case Log(t) =>
        Some((Seq(t), (es: Seq[Expr]) => Log(es.head)))
      case RightShift(t, by) =>
        Some((Seq(t), (es: Seq[Expr]) => RightShift(es.head, by)))
      case LeftShift(t, by) =>
        Some((Seq(t), (es: Seq[Expr]) => LeftShift(es.head, by)))
      case Cast(e, newType) =>
        Some(Seq(e), (es: Seq[Expr]) => Cast(es.head, newType))

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
      case ApproxPoly(origin, arg, approxFnc, totalError) =>
        Some((Seq(arg), (es: Seq[Expr]) => ApproxPoly(origin, es.head, approxFnc, totalError)))

      case And(args) => Some((args, and))
      case Or(args) => Some((args, or))
      case IfExpr(cond, thenn, elze) => Some(Seq(cond, thenn, elze), (es: Seq[Expr]) => IfExpr(es(0), es(1), es(2)))

      case Approx(org, t, errorBound, errorMultiplier, functionName, dd) =>
        Some((Seq(t), (es: Seq[Expr]) => Approx(org, es.head, errorBound, errorMultiplier, functionName, dd)))

      case Tuple(args) => Some(args, (es: Seq[Expr]) => Tuple(es))
      /* Terminals */
      case t: Terminal => Some(Seq[Expr](), (_: Seq[Expr]) => t)

      /* Expr's not handled here should implement this trait */
      // case e: Extractable =>
      //  e.extract
      case DSOperations(t) => Some(t)
    }
  }

  object ElemFnc {

    def unapply(expr: Expr): Option[(Expr, Expr => Expr)] = expr match {
      case x @ Sqrt(t) => Some((t, (es: Expr) => Sqrt(es)))
      case x @ Log(t) => Some((t, (es: Expr) => Log(es)))
      case x @ Sin(t) => Some((t, (es: Expr) => Sin(es)))
      case x @ Cos(t) => Some((t, (es: Expr) => Cos(es)))
      case x @ Tan(t) => Some((t, (es: Expr) => Tan(es)))
      case x @ Exp(t) => Some((t, (es: Expr) => Exp(es)))
      case x @ Asin(t) => Some((t, (es: Expr) => Asin(es)))
      case x @ Acos(t) => Some((t, (es: Expr) => Acos(es)))
      case x @ Atan(t) => Some((t, (es: Expr) => Atan(es)))
      case _ => None
    }
  }
  
  object DSOperations {
    // todo determine what should DSOperation be deconstructed to
    def unapply(expr: Expr): Option[(Seq[Expr], Seq[Expr] => Expr)] = expr match {
      case VectorLiteral(t) => Some(Seq[Expr](), (_: Seq[Expr]) => VectorLiteral(t))
      case MatrixLiteral(t) => Some(Seq[Expr](), (_: Seq[Expr]) => MatrixLiteral(t))
      case VectorFromList(t, _) => Some(t, (es: Seq[Expr]) => VectorFromList(es, es.size))
      //case VectorFromExpr(t) => Some(expr)
      case MatrixFromLists(t,numRows,numCols) => Some(t.flatten, (es: Seq[Expr]) => MatrixFromLists(es.sliding(numCols,numCols).toSeq, numRows, numCols))
      //case MatrixFromExpr(t) => Some(expr)
      //case FlatVector(t) => Some(expr)
      //case VectorRange(t,_,_,_,_) => Some(expr)
      //case MatrixRange(t,_,_,_) => Some(expr)
      //case SizeLessEquals(t,_,_) => Some(expr)
      case SizeLength(t) => Some(Seq(t), (es: Seq[Expr]) => SizeLength(es.head))
      case SizeNumRows(t) => Some(Seq(t), (es: Seq[Expr]) => SizeNumRows(es.head))
      case SizeNumCols(t) => Some(Seq(t), (es: Seq[Expr]) => SizeNumCols(es.head))
      case SubVector(v,from,to) => Some(Seq(v, from, to), (es: Seq[Expr]) => SubVector(es.head, es(1), es(2)))
      case EveryNthVector(ds,n,from) => Some(Seq(ds, n, from), (es: Seq[Expr]) => EveryNthVector(es.head, es(1), es(2)))
      //case SubMatrix(t,_) => Some(expr)
      case EveryNthMatrix(ds,n,from) => Some(Seq(ds, n, from), (es: Seq[Expr]) => EveryNthMatrix(es.head, es(1), es(2)))
      case RowOfMatrix(t,ind) => Some(Seq(t, ind), (es: Seq[Expr]) => RowOfMatrix(es.head, es(1)))
      case CrossProduct(lhs,rhs) =>  Some(Seq(lhs, rhs), (es: Seq[Expr]) => CrossProduct(es.head, es(1)))
      case MinOf(t) => Some(Seq(t), (es: Seq[Expr]) => MinOf(es.head))
      case MaxOf(t) => Some(Seq(t), (es: Seq[Expr]) => MaxOf(es.head))
      case Concat(lhs, rhs) => Some(Seq(lhs, rhs), (es: Seq[Expr]) => Concat(es.head, es(1)))
      case AppendElement(ds,el) => Some(Seq(ds, el), (es: Seq[Expr]) => AppendElement(es.head, es(1)))
      case PrependElement(ds,el) => Some(Seq(ds, el), (es: Seq[Expr]) => PrependElement(es.head, es(1)))
      case ZipVectors(lhs, rhs) => Some(Seq(lhs, rhs), (es: Seq[Expr]) => ZipVectors(es.head, es(1)))
      //case Determinant(t) => Some(expr)
      //case MatrixInverse(t) => Some(expr)
      //case FlattenMatrix(t) => Some(expr)
      //case Transpose(t) => Some(expr)
      case FlipUpsideDown(t) => Some(Seq(t), (es: Seq[Expr]) => FlipUpsideDown(es.head))
      case FlipLeftToRight(t) => Some(Seq(t), (es: Seq[Expr]) => FlipLeftToRight(es.head))
      case PadVector(v, padSize) => Some(Seq(v, padSize), (es: Seq[Expr]) => PadVector(es.head, es(1)))
      case PadMatrix(m, padCol, padRows) => Some(Seq(m, padCol, padRows), (es: Seq[Expr]) => PadMatrix(es.head, es(1), es(2)))
      case MapIter(t,fnc) => Some(Seq(t, fnc), (es: Seq[Expr]) => MapIter(es.head, es(1).asInstanceOf[Lambda]))
      //case MapElemsIter(t,_) => Some(expr)
      case Sum(t,init) => Some(Seq(t, init), (es: Seq[Expr]) => Sum(es.head, es(1)))
      case FoldIter(t,init,fnc) => Some(Seq(t, init, fnc), (es: Seq[Expr]) => FoldIter(es.head, es(1), es(2).asInstanceOf[Lambda]))
      case FoldElemsIter(t,init,fnc) => Some(Seq(t, init, fnc), (es: Seq[Expr]) => FoldElemsIter(es.head, es(1), es(2).asInstanceOf[Lambda]))
      case FilterIter(t, fnc) => Some(Seq(t, fnc), (es: Seq[Expr]) => MapIter(es.head, es(1).asInstanceOf[Lambda]))
      case EnumVectorAndMap(v, fnc) => Some(Seq(v, fnc), (es: Seq[Expr]) => EnumVectorAndMap(es.head, es(1).asInstanceOf[Lambda]))
      case EnumSlideFlatMap(v,step, fnc) => Some(Seq(v, step, fnc), (es: Seq[Expr]) => EnumSlideFlatMap(es.head, es(1).asInstanceOf[Int32Literal], es(2).asInstanceOf[Lambda]))
      case EnumRowsAndMap(m, fnc) => Some(Seq(m, fnc), (es: Seq[Expr]) => EnumRowsAndMap(es.head, es(1).asInstanceOf[Lambda]))
      //case SlideIter(t, _, _) => Some(expr)
      case SlideReduceIter(ds, size, step, fnc) =>
        Some(Seq(ds, size, step, fnc),
          (es: Seq[Expr]) => SlideReduceIter(es.head, es(1).asInstanceOf[Int32Literal], es(2).asInstanceOf[Int32Literal], es(3).asInstanceOf[Lambda]))
      case VectorElement(v, ind) => Some(Seq(v, ind), (es: Seq[Expr]) => VectorElement(es.head, es(1)))
      case MatrixElement(m, irow, icol) => Some(Seq(m, irow, icol), (es: Seq[Expr]) => MatrixElement(es.head, es(1), es(2)))

      case _ => None
    }
  }
}