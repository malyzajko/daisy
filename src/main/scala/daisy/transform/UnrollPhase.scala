// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package transform

import daisy.lang.Extractors._
import daisy.lang.Identifiers._
import daisy.lang.TreeOps
import daisy.lang.TreeOps.{getLastExpression, isMatrix, isVector, replace}
import daisy.lang.Trees._
import daisy.lang.Types.RealType
import daisy.tools.FinitePrecision.Precision
import daisy.tools.{DSAbstraction, Interval, Rational}

import scala.annotation.tailrec
import scala.collection.immutable.Seq

/**
 * Transforms the code by unrolling all loops over DS.
 *
 * Note:
 * - ...
 *
 * Prerequisites:
 * SpecProcessingPhase
 */
object UnrollPhase extends DaisyPhase {
  override val name = "Loop unrolling"
  override val description = "Unrolls all loops over data structures."
  override implicit val debugSection = DebugSectionTransform

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val uniformPrecision = ctx.option[Precision]("precision")
    // need to replace function bodies, create a copy of the whole program
    val fncsToConsider = functionsToConsider(ctx, prg)
    var specInputRanges: Map[Identifier, Map[Identifier, Interval]] = ctx.specInputRanges
    var specInputErrors: Map[Identifier, Map[Identifier, Rational]] = ctx.specInputErrors

    val newDefs = fncsToConsider.map(fnc => {
      val transformed = unrollAll(fnc.body.get, ctx.dsAbstractions(fnc.id), uniformPrecision)
      specInputRanges += (fnc.id -> (specInputRanges(fnc.id) ++ transformed._2))
      specInputErrors += (fnc.id -> (specInputErrors(fnc.id) ++ transformed._3))
      val args = transformed._2.keySet.map(ValDef).toSeq
      val realArgs = fnc.params.filter(vd => vd.getType == RealType)
      fnc.copy(body = Some(transformed._1), params = realArgs ++ args)
    })
    (ctx.copy(specInputRanges = specInputRanges,
      specInputErrors = specInputErrors), Program(prg.id, newDefs ++ functionsToConsider(ctx, prg).diff(fncsToConsider)))
  }


  /**
   * Unroll all loops over data structures to operations on Reals
   *
   * @param fnc FunDef of the program to be unrolled
   * @return unrolled expression [[Expr]]
   */
  def unrollAll(fnc: Expr, dsaRangeMap: Map[Expr, DSAbstraction], prec: Precision):
  (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) = {
    var addedIds: Map[(Expr, Int), Identifier] = Map()

    def rec(e: Expr): (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) = e match {
      case SizeLength(v) if isVector(v) => (RealLiteral(dsaRangeMap(v).dsSize), Map(), Map())
      case VectorElement(v, Int32Literal(index)) =>
        val tmpId = addedIds.getOrElse( (v,index),FreshIdentifier(s"${v}_$index", RealType))
        if (!addedIds.contains(v,index)) {
          addedIds += (v,index) -> tmpId
        }
        val range = dsaRangeMap(v).at(index).toInterval
        val err = prec.absRoundoff(range)
        (Variable(tmpId), Map(tmpId -> range), Map(tmpId -> err))
      //case VectorLiteral(vid) => ???
      // unrolling
      case SlideReduceIter(v, size, step, fnc) if isVector(v) => ???
      case SlideReduceIter(m, size, step, fnc) if isMatrix(m) => ???
      case FoldIter(v, init, Lambda(args, body)) if isVector(v) =>
        val initTransf = rec(init)
        val vSize = dsaRangeMap(v).dsSize
        val res = unrollFoldwLetsOnVector(v, 0, vSize, initTransf._1, args, body, dsaRangeMap(v), prec, addedIds)
        addedIds ++= res._4
        (res._1, initTransf._2 ++ res._2, initTransf._3 ++ res._3)
      case Sum(v, init) if isVector(v) =>
        val vSize = dsaRangeMap(v).dsSize
        val accId = FreshIdentifier(s"acc$v", RealType)
        val xId = FreshIdentifier(s"x$v", RealType)
        val args = Seq(ValDef(accId), ValDef(xId))
        val body = Plus(Variable(accId), Variable(xId))
        val res = unrollFoldwLetsOnVector(v, 0, vSize, init, args, body, dsaRangeMap(v), prec, addedIds)
        addedIds ++= res._4
        (res._1, res._2, res._3)
      //case FoldIter(m, init, Lambda(args, body)) if isMatrix(m) =>
      //  val numRows = dsaRangeMap(m).numRows
      //  unrollFoldwLetsOnMatrix(m, 0, numRows, init, args, body)
      //case FoldElemsIter(m, init, Lambda(args, body)) if isMatrix(m) =>
      //  val numRows = dsaRangeMap(m).numRows
      //  val numCols = dsaRangeMap(m).numCols
      //  unrollFoldElemsWLetsOnMatrix(m, MatrixIndex(0,0), MatrixIndex(numRows-1, numCols-1), numCols, init, args, body)
      case Let(binder, value, VectorFromList(lst, _)) => //if isVector(value) =>
        val bind = rec(value)
        //val tmp_ = lst.tail.foldLeft(lst.head)((acc, t) => rec(t)._1) // todo do not skip the evaluation
        bind._1 match {
          case inl@Let(inb, inv, inbody) =>
            val last = TreeOps.getLastExpression(inl)
            val upd = replace({case x if x == last => Let(binder, last, lst.head)})(inl)
            (upd, bind._2, bind._3)
          case _ => (Let(binder, bind._1, lst.head), bind._2, bind._3) // only take the first element
        }
      // ops
      case Let(binder, FoldIter(v, init, Lambda(args, body)), letBody) if isVector(v) =>
        val initTransf = rec(init)
        val vSize = dsaRangeMap(v).dsSize
        val unrolled = unrollFoldwLetsOnVector(v, 0, vSize, initTransf._1, args, body, dsaRangeMap(v), prec, addedIds)
        addedIds ++= unrolled._4
        val valueAfterUnroll = getLastExpression(unrolled._1)
        val newBody = rec(letBody)

        val newExpr = replace({case x if x== valueAfterUnroll => Let(binder, valueAfterUnroll, newBody._1) })(unrolled._1)
        (newExpr, unrolled._2 ++ initTransf._2 ++ newBody._2, unrolled._3 ++ initTransf._3 ++ newBody._3)

      case Let(binder, Sum(v, init), letBody) if isVector(v) =>
        val vSize = dsaRangeMap(v).dsSize
        val accId = FreshIdentifier(s"acc$v", RealType)
        val xId = FreshIdentifier(s"x$v", RealType)
        val args = Seq(ValDef(accId), ValDef(xId))
        val body = Plus(Variable(accId), Variable(xId))
        val unrolled = unrollFoldwLetsOnVector(v, 0, vSize, init, args, body, dsaRangeMap(v), prec, addedIds)
        addedIds ++= unrolled._4
        val valueAfterUnroll = getLastExpression(unrolled._1)
        val newBody = rec(letBody)

        val newExpr = replace({case x if x== valueAfterUnroll => Let(binder, valueAfterUnroll, newBody._1) })(unrolled._1)
        (newExpr, unrolled._2 ++ newBody._2, unrolled._3 ++ newBody._3)

      case Let(binder, value, body) =>
        val newValue = rec(value)
        val newBody = rec(body)
        (Let(binder, newValue._1, newBody._1), (newValue._2 ++ newBody._2), (newValue._3 ++ newBody._3))
      case ElemFnc(t, recons) =>
        val res = rec(t)
        (recons(res._1), res._2, res._3)

      //case x@ArithOperator(seq, recons) if isMatrix(x) =>
      //  seq match {
      //    case Seq(fst, snd) if isMatrix(fst) && isMatrix(snd) => // element-wise operation
      //      val cols = dsaRangeMap(fst).numCols
      //      val rows = dsaRangeMap(fst).numRows
      //      val elts = for(i <- (0 until rows); j<- (0 until cols)) yield (i,j)
      //      val newBds = elts.map({case (i,j) =>
      //        val lhsId = FreshIdentifier(s"$fst${i}_$j", RealType)
      //        val rhsId = FreshIdentifier(s"$snd${i}_$j", RealType)
      //        // todo save intermediate results
      //        recons(Seq(Variable(lhsId), Variable(rhsId)))
      //      })
      //    case Seq(fst, snd) if isMatrix(fst) => // operation with a constant
      //      val cols = dsaRangeMap(fst).numCols
      //      val rows = dsaRangeMap(fst).numRows
      //      val elts = for(i <- (0 until rows); j<- (0 until cols)) yield (i,j)
      //      val newBds = elts.map({case (i,j) =>
      //        val lhsId = FreshIdentifier(s"$fst${i}_$j", RealType)
      //        // todo save intermediate results
      //        recons(Seq(Variable(lhsId), snd))
      //      })
      //    case Seq(t) => // unary operation
      //      ???
      //    case _ => throw new DaisyFatalError(Some(s"Unsupported operator on DS $x"))
      //  }
      //  val args =
      //    if (seq.size > 1) {
      //      seq.tail.foldLeft(Seq[Expr](seq.head), Map[Identifier, Interval](), Map[Identifier, Rational]())({
      //        case (acc, x) =>
      //          val tmp = rec(x)
      //          (acc._1 :+ tmp._1, acc._2 ++ tmp._2, acc._3 ++ tmp._3)
      //      })
      //    } // DFS on the AST
      //    else {
      //      val tmp = rec(seq.head)
      //      (Seq(tmp._1), tmp._2, tmp._3)
      //    }
      //  (recons(args._1), args._2, args._3)

      case ArithOperator(seq, recons) =>
        val args =
          if (seq.size > 1) {
            seq.tail.foldLeft(Seq[Expr](seq.head), Map[Identifier, Interval](), Map[Identifier, Rational]())({
              case (acc, x) =>
                val tmp = rec(x)
                (acc._1 :+ tmp._1, acc._2 ++ tmp._2, acc._3 ++ tmp._3)
            })
          } // DFS on the AST
          else {
            val tmp = rec(seq.head)
            (Seq(tmp._1), tmp._2, tmp._3)
          }
        (recons(args._1), args._2, args._3)
      case x => (x, Map[Identifier, Interval](), Map[Identifier, Rational]())
    }

    rec(fnc)
  }
  // TODO don't forget to add to the context modified intermediate results/input ranges for all vector/matrix elements

  /**
   * Unrolls map over vectors/{matrix elements} with temporary Real variables:
   * replace by nested let statements.
   *
   * @param v        vector to fold
   * @param fromInd  start index of v
   * @param toInd    end index of the unrolling on v
   * @param init     initial value for fold
   * @param args     arguments of the lambda function
   * @param body     lambda function performing the fold
   * @param dsaRange vector abstraction for ranges
   * @param prec     uniform precision for inputs
   * @return let-expression equivalent to the unrolled fold over v, intermediate ranges and errors updated with fresh variables ranges
   */
  def unrollMapwLetsOnVector(v: Expr,
                              fromInd: Int,
                              toInd: Int,
                              init: Expr,
                              args: Seq[ValDef],
                              body: Expr,
                              dsaRange: DSAbstraction,
                              prec: Precision): (Expr, Map[Identifier, Interval], Map[Identifier, Rational]) = {
    //val accId = args.head.id
    val xId = args.head.id
    var updSpecRanges: Map[Identifier, Interval] = Map()
    var updSpecErrs: Map[Identifier, Rational] = Map()

    def getIdAndBody(ind: Int): (Identifier, Expr) = {
      val newId = FreshIdentifier(s"acc$v$ind", RealType, alwaysShowUniqueID = true)
      val eltId = FreshIdentifier(s"${v}_$ind", RealType, alwaysShowUniqueID = true)
      val newValue = replace {
        case Variable(id) if id == xId =>
          updSpecRanges += eltId -> dsaRange.at(ind).toInterval
          updSpecErrs += eltId -> prec.absRoundoff(dsaRange.at(ind).toInterval)
          Variable(eltId)
      }(body)
      (newId, newValue)
    }

    @tailrec
    def buildRecursiveLet(cInd: Int, lastID: Identifier, accBody: Expr): Expr = {
      val nextI = cInd - 1
      if (nextI < fromInd)
        accBody // acc is replaced with init, no new IDs are used
      else {
        val (newId, newBody) = getIdAndBody(nextI)
        val newLet = Let(lastID, newBody, accBody)
        buildRecursiveLet(nextI, newId, newLet)
      }
    }

    val (newId, newBody) = getIdAndBody(toInd-1)
    val unrolled = buildRecursiveLet(toInd-1, newId, newBody)
    (unrolled, updSpecRanges, updSpecErrs)
  }

  /**
   * Unrolls folds over vectors with temporary Real varibales: replace by nested let statements.
   *
   * @param v        vector to fold
   * @param fromInd  start index of v
   * @param toInd    end index of the unrolling on v
   * @param init     initial value for fold
   * @param args     arguments of the lambda function
   * @param body     lambda function performing the fold
   * @param dsaRange vector abstraction for ranges
   * @param prec     uniform precision for inputs
   * @return let-expression equivalent to the unrolled fold over v, intermediate ranges and errors updated with fresh variables ranges
   */
  def unrollFoldwLetsOnVector(v: Expr,
                              fromInd: Int,
                              toInd: Int,
                              init: Expr,
                              args: Seq[ValDef],
                              body: Expr,
                              dsaRange: DSAbstraction,
                              prec: Precision,
                              addedIds: Map[(Expr, Int), Identifier]):
  (Expr, Map[Identifier, Interval], Map[Identifier, Rational], Map[(Expr, Int), Identifier]) = {
    val accId = args.head.id
    val xId = args(1).id
    var updSpecRanges: Map[Identifier, Interval] = Map()
    var updSpecErrs: Map[Identifier, Rational] = Map()
    var moreAddedIds: Map[(Expr, Int), Identifier] = Map()

    def getIdAndBody(ind: Int): (Identifier, Expr) = {
      val newId = FreshIdentifier(s"acc$v$ind", RealType)
      val eltId = addedIds.getOrElse((v, ind), FreshIdentifier(s"${v}_$ind", RealType, alwaysShowUniqueID = true))
      if (!addedIds.contains(v, ind)) {
        moreAddedIds += (v,ind) -> eltId
      }
      val newValue = replace {
        case Variable(id) if id == accId => if (ind == fromInd) init else Variable(newId)
        case Variable(id) if id == xId =>
          updSpecRanges += eltId -> dsaRange.at(ind).toInterval
          updSpecErrs += eltId -> prec.absRoundoff(dsaRange.at(ind).toInterval)
          Variable(eltId)
      }(body)
      (newId, newValue)
    }

    @tailrec
    def buildRecursiveLet(cInd: Int, lastID: Identifier, accBody: Expr): Expr = {
      val nextI = cInd - 1
      if (nextI < fromInd)
        accBody // acc is replaced with init, no new IDs are used
      else {
        val (newId, newBody) = getIdAndBody(nextI)
        val newLet = Let(lastID, newBody, accBody)
        buildRecursiveLet(nextI, newId, newLet)
      }
    }

    val (newId, newBody) = getIdAndBody(toInd-1)
    val unrolled = buildRecursiveLet(toInd-1, newId, newBody)
    (unrolled, updSpecRanges, updSpecErrs, moreAddedIds)
  }
}
