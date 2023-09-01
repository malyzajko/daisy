package daisy.analysis

import daisy._
import daisy.lang.Extractors.ArithOperator
import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.TreeOps
import daisy.lang.TreeOps._
import daisy.lang.Trees._
import daisy.lang.Types.{Int32Type, MatrixType, RealType, VectorType}
import daisy.tools.FinitePrecision._
import daisy.tools.Interval._
import daisy.tools._
import daisy.utils.CachingMap

/**
  * This phase computes the abstraction of data structures
  * *
  * *
  * Prerequisites:
  *- SpecsProcessingPhase
  */
object DSNaivePhase extends DaisyPhase with tools.RoundoffEvaluators with tools.RangeEvaluators {
  override val name = "DS Naive"
  override val description = "Computes ranges and errors for programs with data structures"
  type IntermedResults = Map[(Expr, PathCond), Interval]
  val unfoldLimit: Int = 1
  val unfoldLimitHighOcc: Int = 1
  val occurrenceLimit: Int = 1 // how often can a variable occur in the lambda body to be efficiently inlined

  override implicit val debugSection: DebugSection = DebugSectionAnalysis
  var rangeMethod = ""
  var errorMethod = ""
  var trackRoundoffErrs = true

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    rangeMethod = ctx.option[String]("rangeMethod")
    errorMethod = ctx.option[String]("errorMethod")
    trackRoundoffErrs = !ctx.hasFlag("noRoundoff")
    val uniformPrecision = ctx.option[Precision]("precision")

    val res = functionsToConsider(ctx, prg).map(fnc => {
      ctx.timers.get("DSPhase-"+ fnc.id.toString).start()
      assert(ctx.dsAbstractions.contains(fnc.id), s"No abstraction for data structures in the function $fnc") // todo allow functions with Real inputs only
      val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

      val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id) ++ allIDsOf(fnc.body.get).diff(ctx.specInputPrecisions(fnc.id).keySet).map(id => id -> uniformPrecision).toMap // add variables from let statements
      val (err, range) = computeResults(fnc.body.get, ctx.dsAbstractions(fnc.id), ctx.specInputRanges(fnc.id), inputErrorMap, precisionMap, uniformPrecision, fnc.id) // todo also call with precond (for SMTRange)
      ctx.timers.get("DSPhase-"+ fnc.id.toString).stop()
      (fnc.id -> err, fnc.id -> range)
    })

    val (errors, ranges) = res.unzip
    (ctx.copy(
      resultAbsoluteErrors = ctx.resultAbsoluteErrors ++ errors,
      resultRealRanges = ctx.resultRealRanges ++ ranges
    ), prg)
  }

  private def withPrecision(errs: Map[(Expr, PathCond), Interval], prec: Precision): Map[(Expr, PathCond), (Interval, Precision)] =
    errs.map({ case (k, v) => k -> (v, prec) })

  /**
   * @param expr function body to be evaluated
   * @param dsas DS AbstractionS taken from teh Context
   * @param initValMap input ranges for variables
   * @param inputErrorMap input errors for variables
   * @param precisionMap precision assignment on all IDs in expr
   * @param constPrecision a uniform precision to be used for constants
   * @param currentFncId ID of the currently evaluated body
   * @return
   */
  def computeResults(expr: Expr,
                     dsas: Map[Expr, DSAbstraction],
                     initValMap: Map[Identifier, Interval],
                     inputErrorMap: Map[Identifier, Rational],
                     precisionMap: Map[Identifier, Precision],
                     constPrecision: Precision,
                     currentFncId: Identifier): (Rational, Interval) = {
    // todo include other range methods
    //val rangeFromInt: Interval => Interval = rangeMethod match {
    //  case "interval" => Interval.apply
    //  case _ => throw new Exception("This range method is not yet supported for data structures")
    //}

    /**
     * Evaluates the expressions and computes its (intermediate) range(s) and rounding error(s)
     * @param e expression to be evaluated
     * @param rangesAbstr a mapping between expression and a [[DSAbstraction]] for its range(s)
     * @param errorsAbstr a mapping between expression and a [[DSAbstraction]] for its rounding error(s)
     * @param indices a mapping between an expression and a set of indices on which the current evaluation should happen
     * @param path a sequence of conditions that leads to an expression e, empty for expressions without branching (see [[PathCond]])
     * @param forceEval true/false used to indicate that sub-expressions of e are already evaluated and intermediate
     *                  results can be used in evaluation of e
     * @return (rangesAbstractions, errorsAbstractions) - range and error abstractions of teh expression e and its subexpressions
     */
    def eval(e: Expr,
             rangesAbstr: Map[Expr, DSAbstraction],
             errorsAbstr: Map[Expr, DSAbstraction],
             indices: Map[Expr, Set[Index]],
             path: Seq[Expr] = emptyPath,
             forceEval: Boolean = false,
             unrollingScope: Boolean = false
            ):
      // (Ranges, Errors)
    (Map[Expr, DSAbstraction], Map[Expr, DSAbstraction]) = {
      // todo also check for errorAbstr?
      // todo check that this is a sufficient condition!
      if (!unrollingScope && rangesAbstr.contains(e) && (!indices.contains(e) || indices(e).diff(rangesAbstr(e).indices).isEmpty)) {
        (rangesAbstr, errorsAbstr)
      } else
      e match {
      case x@VectorLiteral(_) =>
        assert(rangesAbstr.contains(x) && errorsAbstr.contains(x), s"Unknown vector $x")
        (rangesAbstr, errorsAbstr)

      case x@MatrixLiteral(_) =>
        assert(rangesAbstr.contains(x) && errorsAbstr.contains(x), s"Unknown matrix $x")
        (rangesAbstr, errorsAbstr)

      case x@VectorElement(v, index) =>
        val (indexRanges, indexErrs) = eval(index, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val indexDS = indexRanges(index)
        assert(indexDS.hasSingleRange, s"Vector element index should have a single range, $index has ${indexDS.ranges}")
        val indRange = MPFRInterval.integersIn(indexDS.fullInterval).map(x => VectorIndex(x).asInstanceOf[Index])
        val updIndices = indices + (v -> indRange)
        val (vRanges, vErrors) = eval(v, indexRanges, indexErrs, updIndices, unrollingScope = unrollingScope)
        val xDSRanges = vRanges(v).getAbstractionOnlyAtIndices(indRange)
        val xDSErrors = vErrors(v).getAbstractionOnlyAtIndices(indRange)
        (vRanges + (x -> xDSRanges), vErrors + (x -> xDSErrors))

      case x@MatrixElement(m, irow, icol) =>
        val (iRowRanges, iRowErrs) = eval(irow, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (iColRanges, iColErrs) = eval(icol, iRowRanges, iRowErrs, indices, unrollingScope = unrollingScope)
        val iRowDS = iRowRanges(irow)
        val iColsDS = iColRanges(icol)
        assert(iRowDS.hasSingleRange && iColsDS.hasSingleRange, s"Matrix element index should have a single range, ($irow,$icol) has (${iRowDS.ranges},${iColsDS.ranges})")
        val iRowRangeInt = MPFRInterval.integersIn(iRowDS.fullInterval).toSeq
        val iColRangeInt = MPFRInterval.integersIn(iColsDS.fullInterval).toSeq
        val mIndices = for (ir <- iRowRangeInt; ic <- iColRangeInt) yield { MatrixIndex(ir, ic).asInstanceOf[Index] }

        val updIndices = indices + (m -> mIndices.toSet)
        val (mRanges, mErrors) = eval(m, iColRanges, iColErrs, updIndices, unrollingScope = unrollingScope)
        val xDSRanges = mRanges(m).getAbstractionOnlyAtIndices(mIndices.toSet)
        val xDSErrors = mErrors(m).getAbstractionOnlyAtIndices(mIndices.toSet)
        (mRanges + (x -> xDSRanges), mErrors + (x -> xDSErrors))

      case x@RowOfMatrix(m, row) =>
        assert(rangesAbstr.contains(m) && errorsAbstr.contains(m), s"Row of unknown matrix $m")
        val (indexRanges, indexErrs) = eval(row, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val indexDS = indexRanges(row)
        assert(indexDS.hasSingleRange, s"Matrix row index should have a single range, $row has ${indexDS.ranges}")
        val indRange = MPFRInterval.integersIn(indexDS.fullInterval).flatMap(i => rangesAbstr(m).indicesAtRow(i))
        val updIndices = indices + (m -> indRange)
        val (mRanges, mErrors) = eval(m, indexRanges, indexErrs, updIndices, unrollingScope = unrollingScope)
        val xDSRanges = mRanges(m).getAbstractionOnlyAtIndices(indRange).convertMatrixIndToRowInd()
        val xDSErrors = mErrors(m).getAbstractionOnlyAtIndices(indRange).convertMatrixIndToRowInd()
        (mRanges + (x -> xDSRanges), mErrors + (x -> xDSErrors))

      case x@SubVector(v, from, to) =>
        val (fromRanges, fromErrs) = eval(from, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (toRanges, toErrs) = eval(to, fromRanges, fromErrs, indices, unrollingScope = unrollingScope)
        val fromDS = toRanges(from)
        val toDS = toRanges(to)
        assert(fromDS.hasSingleRange && toDS.hasSingleRange, s"Slice indices should be numbers and have a single range,\n\t $from has ${fromDS.ranges} and $to has ${toDS.ranges} ")
        val indRange = MPFRInterval.integersIn(MPFRInterval.union(Set(fromDS.fullInterval, toDS.fullInterval))).map(x => VectorIndex(x).asInstanceOf[Index])
        val indsToTake = indices + (v -> indRange)
        val (vRanges, vErrors) = eval(v, toRanges, toErrs, indsToTake, unrollingScope = unrollingScope) // in case v is an expression
        val subDSRanges = vRanges(v).getAbstractionOnlyAtIndices(indRange)
        val subDSErrors = vErrors(v).getAbstractionOnlyAtIndices(indRange)

        val subIndices = indRange.toSeq.indices.map(VectorIndex).sorted // from 0 to the number of elements in the sub-vector
        val xDSRangeMap = indRange.toSeq.sorted.zip(subIndices).map({
          case (oi,newI) => Set(newI.asInstanceOf[Index])->subDSRanges.at(oi)
        }).toMap
        val xDSErrorMap = indRange.toSeq.sorted.zip(subIndices).map({
          case (oi,newI) => Set(newI.asInstanceOf[Index])->subDSErrors.at(oi)
        }).toMap
        (vRanges + (x -> DSAbstraction(xDSRangeMap).regroup()), vErrors + (x -> DSAbstraction(xDSErrorMap).regroup()))

      case x@PadVector(v, Int32Literal(padSize)) =>
        val (vRanges, vErrors) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        // change indices
        val origVRanges = vRanges(v)
        val newIndValues = (0 until padSize).toList ++ ((origVRanges.dsSize + padSize) until (origVRanges.dsSize + 2*padSize))
        val newInds = newIndValues.map(x=>VectorIndex(x).asInstanceOf[Index]).toSet

        val updatedIndicesRMap: Map[Set[Index], MPFRInterval] = origVRanges.indexToRange.map({
          case (indices, range) =>
            val increasedInd = indices.map({case VectorIndex(i) => VectorIndex(i+padSize).asInstanceOf[Index]})
            increasedInd -> range
        })
        val xRangeDS = DSAbstraction(updatedIndicesRMap + (newInds -> MPFRInterval.zero))
        // sanity check
        assert(xRangeDS.dsSize == origVRanges.dsSize + 2*padSize)

        val origVErrors = vErrors(v)
        val updatedIndicesEMap: Map[Set[Index], MPFRInterval] = origVErrors.indexToRange.map({
          case (indices, range) =>
            val increasedInd = indices.map({case VectorIndex(i) => VectorIndex(i+padSize).asInstanceOf[Index]})
            increasedInd -> range
        })
        val xErrorDS = DSAbstraction(updatedIndicesEMap + (newInds -> MPFRInterval.zero))
        (vRanges + (x->xRangeDS), vErrors + (x->xErrorDS))

      case x@PadMatrix(m, Int32Literal(padRows), Int32Literal(padCols)) =>
        val (mRanges, mErrors) = eval(m, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        // change indices
        val origMRanges = mRanges(m)
        val newRowInds = (0 until padRows).toList ++ ((origMRanges.numRows + padRows) until (origMRanges.numRows + 2*padRows)).toList
        val newColInds = (0 until padCols).toList ++ ((origMRanges.numCols + padCols) until (origMRanges.numCols + 2*padCols)).toList
        val oldRowIndices = padRows until origMRanges.numRows+padRows
        val oldColIndices = padCols until origMRanges.numCols+padCols
        val newRowIndices = for { x <- newRowInds; y <- (newColInds ++ oldColIndices) } yield MatrixIndex(x, y).asInstanceOf[Index]
        val newColIndices = for { x <- (newRowInds ++ oldRowIndices); y <- newColInds } yield MatrixIndex(x, y).asInstanceOf[Index]
        val newIndices = (newRowIndices ++ newColIndices).toSet

        val updatedIndicesRMap: Map[Set[Index], MPFRInterval] = origMRanges.indexToRange.map({
          case (indices, range) =>
            val increasedInd = indices.map({case MatrixIndex(i,j) => MatrixIndex(i+padRows, j+padCols).asInstanceOf[Index]})
            increasedInd -> range
        })
        val xRangeDS = DSAbstraction(updatedIndicesRMap + (newIndices -> MPFRInterval.zero))
        // sanity check
        assert(xRangeDS.numCols == origMRanges.numCols + 2*padCols && xRangeDS.numRows == origMRanges.numRows + 2*padRows )

        val origMErrors = mErrors(m)
        val updatedIndicesEMap: Map[Set[Index], MPFRInterval] = origMErrors.indexToRange.map({
          case (indices, range) =>
            val increasedInd = indices.map({case MatrixIndex(i,j) => MatrixIndex(i+padRows, j+padCols).asInstanceOf[Index]})
            increasedInd -> range
        })
        val xErrorDS = DSAbstraction(updatedIndicesEMap + (newIndices -> MPFRInterval.zero))
        (mRanges + (x->xRangeDS), mErrors + (x->xErrorDS))

      case x@ZipVectors(lhs, rhs) =>
        assert(rangesAbstr(lhs).dsSize == rangesAbstr(rhs).dsSize, s"Trying to zip vectors of different size $lhs:${rangesAbstr(lhs).dsSize} and $rhs:${rangesAbstr(rhs).dsSize} ")
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        val newRowIndices = 0 until rangesAbstr(lhs).dsSize
        val (newRangesM, newErrsM) = newRowIndices.map(i => {
          val mIndexLhs = MatrixIndex(i, 0).asInstanceOf[Index]
          val mIndexRhs = MatrixIndex(i, 1).asInstanceOf[Index]
          (Set(Set(mIndexLhs) -> rhsRanges(lhs).at(i), Set(mIndexRhs) -> rhsRanges(rhs).at(i)),
            Set(Set(mIndexLhs) -> rhsErrors(lhs).at(i), Set(mIndexRhs) -> rhsErrors(rhs).at(i)))
        }).unzip
        val xRangeDS = DSAbstraction(newRangesM.flatten.toMap)
        val xErrorDS = DSAbstraction(newErrsM.flatten.toMap)
        (rangesAbstr + (x->xRangeDS), errorsAbstr + (x->xErrorDS))

      case x@FlipUpsideDown(m) =>
        val (mRanges, mErrors) = eval(m, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = mRanges(m)
        val xRangeDS = flipDSIndices(origDSRange, row=true)
        val origDSErr = mErrors(m)
        val xErrorDS = flipDSIndices(origDSErr, row=true)
        (mRanges + (x->xRangeDS), mErrors + (x->xErrorDS))

      case x@FlipLeftToRight(m) =>
        val (mRanges, mErrors) = eval(m, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = mRanges(m)
        val xRangeDS = flipDSIndices(origDSRange, row=false)
        val origDSErr = mErrors(m)
        val xErrorDS = flipDSIndices(origDSErr, row=false)
        (mRanges + (x->xRangeDS), mErrors + (x->xErrorDS))

      case x@AppendElement(v, el) if isVector(v) =>
        val (vRanges, vErrors) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = vRanges(v)
        val origDSErr = vErrors(v)

        val (eltRanges, eltErrors) = eval(el, vRanges, vErrors, indices, unrollingScope = unrollingScope)
        val newIndex = VectorIndex(origDSRange.dsSize).asInstanceOf[Index]
        val xRangeDS = origDSRange.addSpec(Set(newIndex), eltRanges(el).fullInterval)
        val xErrorDS = origDSErr.addSpec(Set(newIndex), eltErrors(el).fullInterval)
        (eltRanges + (x->xRangeDS), eltErrors + (x->xErrorDS))

      case x@AppendElement(m, el) if isMatrix(m) =>
        val (mRanges, mErrors) = eval(m, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = mRanges(m)
        val origDSErr = mErrors(m)

        val (eltRanges, eltErrors) = eval(el, mRanges, mErrors, indices, unrollingScope = unrollingScope)
        // can only append a whole row
        val newRanges = (0 until origDSRange.numCols).map(i => {
          val eltRange = eltRanges(el).at(i)
          Set(MatrixIndex(origDSRange.numRows, i).asInstanceOf[Index]) -> eltRange
        }).toMap
        val newErrs = (0 until origDSRange.numCols).map(i => {
          val eltError = eltErrors(el).at(i)
          Set(MatrixIndex(origDSRange.numRows, i).asInstanceOf[Index]) -> eltError
        }).toMap
        val xRangeDS = origDSRange.updateWith(newRanges)
        val xErrorDS = origDSErr.updateWith(newErrs)
        (eltRanges + (x->xRangeDS), eltErrors + (x->xErrorDS))

      case x@PrependElement(v, el) if isVector(v) =>
        val (vRanges, vErrors) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = vRanges(v)
        val origDSErr = vErrors(v)

        val (eltRanges, eltErrors) = eval(el, vRanges, vErrors, indices, unrollingScope = unrollingScope)
        val newRange = Set(VectorIndex(0).asInstanceOf[Index]) -> eltRanges(el).fullInterval
        val updIndRanges = origDSRange.indices.map(
          {case ig@VectorIndex(intI) =>
            val newInds = Set(VectorIndex(intI + 1).asInstanceOf[Index])
            newInds -> origDSRange.at(ig)
        }).toMap
        val xRangeDS = DSAbstraction(updIndRanges + newRange)

        val newErr = Set(VectorIndex(0).asInstanceOf[Index]) -> eltErrors(el).fullInterval
        val updIndErrs = origDSErr.indices.map(
          {case ig@VectorIndex(intI) =>
            val newInds = Set(VectorIndex(intI + 1).asInstanceOf[Index])
            newInds -> origDSErr.at(ig)
        }).toMap
        val xErrorDS = DSAbstraction(updIndErrs + newErr)
        (eltRanges + (x->xRangeDS), eltErrors + (x->xErrorDS))
        
      case x@PrependElement(m, el) if isMatrix(m) =>
        val (mRanges, mErrors) = eval(m, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = mRanges(m)
        val origDSErr = mErrors(m)

        val (eltRanges, eltErrors) = eval(el, mRanges, mErrors, indices, unrollingScope = unrollingScope)
        // can only append a whole row
        val newRanges = (0 until origDSRange.numCols).map(i => {
          val eltRange = eltRanges(el).at(i)
          Set(MatrixIndex(0, i).asInstanceOf[Index]) -> eltRange
        }).toMap
        val updIndRanges = origDSRange.indices.map({
          case ig@MatrixIndex(intI, j) =>
            val newInds = Set(MatrixIndex(intI + 1, j).asInstanceOf[Index])
            newInds -> origDSRange.at(ig)
        }).toMap
        val xRangeDS = DSAbstraction(updIndRanges ++ newRanges)

        val newErrs = (0 until origDSRange.numCols).map(i => {
          val eltError = eltErrors(el).at(i)
          Set(MatrixIndex(0, i).asInstanceOf[Index]) -> eltError
        }).toMap
        val updIndErrs = origDSErr.indices.map({
          case ig@MatrixIndex(intI, j) =>
            val newInds = Set(MatrixIndex(intI + 1, j).asInstanceOf[Index])
          newInds -> origDSErr.at(ig)
        }).toMap
        val xErrorDS = DSAbstraction(updIndErrs ++ newErrs)
        (eltRanges + (x->xRangeDS), eltErrors + (x->xErrorDS))

      case x@Concat(lhs, rhs) if isVector(lhs) && isVector(rhs) =>
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        val offset = rhsRanges(lhs).dsSize
        val rhsNewRanges = rhsRanges(rhs).indexToRange.map({
          case (ind, range) =>
            val newInds = ind.map({ case VectorIndex(i) => VectorIndex(i+offset).asInstanceOf[Index]})
            newInds -> range
        })
        val rhsNewErrs = rhsErrors(rhs).indexToRange.map({
          case (ind, range) =>
            val newInds = ind.map({ case VectorIndex(i) => VectorIndex(i+offset).asInstanceOf[Index]})
            newInds -> range
        })
        val xDSRange = rhsRanges(lhs).updateWith(rhsNewRanges).regroup()
        val xDSErrs = rhsErrors(lhs).updateWith(rhsNewErrs).regroup()
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErrs))

      case x@Concat(lhs, rhs) if isMatrix(lhs) && isMatrix(rhs) =>
        // append the rows of the rhs to the lhs
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        val leftcols = rhsRanges(lhs).numCols
        val leftRows = rhsRanges(lhs).numRows
        val rightcols = rhsRanges(rhs).numCols
        assert(leftcols == rightcols,
          s"Trying to concatenate matrices with different numbers of columns $lhs:$leftcols and $rhs:$rightcols")
        val rhsNewRanges = rhsRanges(rhs).indexToRange.map({
          case (ind, range) =>
            val newInds = ind.map({ case MatrixIndex(i,j) => MatrixIndex(i+leftRows, j).asInstanceOf[Index]})
            newInds -> range
        })

        val rhsNewErrs = rhsErrors(rhs).indexToRange.map({
          case (ind, range) =>
            val newInds = ind.map({ case MatrixIndex(i,j) => MatrixIndex(i+leftRows, j).asInstanceOf[Index]})
            newInds -> range
        })
        val xDSRange = rhsRanges(lhs).updateWith(rhsNewRanges)
        val xDSErrs = rhsErrors(lhs).updateWith(rhsNewErrs)
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErrs))

      case x@Cast(n, Int32Type) => // for n.intValue()
        // only allow simple expressions here
        val (nRanges, nErrors) = eval(n, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val half = MPFRFloat.one / MPFRFloat.two
        assert(nRanges(n).hasSingleRange, s"Expression $n is expected to have one range, but has the following: ${nRanges(n).ranges}")
        val range = nRanges(n).fullInterval // n is expected to be a Real number -> one range only
        val err = if (range.isPointRange) {
          // round to the nearest
          if (range.xhi.fractionPart > half)
          // up
            MPFRFloat.abs(range.xhi.fractionPart - half)
          else {
            // down
            range.xhi.fractionPart
          }
        } else
          half
        val ind = nRanges(n).indices // should be empty

        (nRanges + (x -> DSAbstraction(Map(ind -> range))), nErrors + (x -> DSAbstraction(Map(ind -> MPFRInterval(err)))))

      case x@VectorFromList(list, length) =>
        val (listRanges, listErrs) = (0 until length).foldLeft((rangesAbstr, errorsAbstr))({
          case (acc, i) =>
            val listElt = list(i)
            val (eltRanges, eltErrors) = eval(listElt, acc._1, acc._2, indices + (x -> Set(VectorIndex(i))), unrollingScope = unrollingScope)
            val xRangeAbstr = (acc._1).getOrElse(x, DSAbstraction.empty())
            val updatedRangeAbstr = xRangeAbstr.addSpec(Set(VectorIndex(i)), eltRanges(listElt).fullInterval)

            val xErrAbstr = (acc._2).getOrElse(x, DSAbstraction.empty())
            val updatedErrAbstr = xErrAbstr.addSpec(Set(VectorIndex(i)), eltErrors(listElt).fullInterval)
            (eltRanges + (x -> updatedRangeAbstr), eltErrors + (x -> updatedErrAbstr))
        })
        (listRanges, listErrs)

      case x@MatrixFromLists(listOflists, numRows, numCols) =>
        val (matrixRanges, matrixErrs) = (0 until numRows).foldLeft((rangesAbstr, errorsAbstr))({
              // iterate over rows
          case (acc, i) =>
            (0 until numCols).foldLeft(acc)({
              case (accC, j) =>
                val matrixElt = listOflists(i)(j)
                val (eltRanges, eltErrors) = eval(matrixElt, accC._1, accC._2, indices + (x -> Set(MatrixIndex(i, j))), unrollingScope = unrollingScope)
                val xRangeAbstr = (accC._1).getOrElse(x, DSAbstraction.empty())
                val updatedRangeAbstr = xRangeAbstr.addSpec(Set(MatrixIndex(i, j)), eltRanges(matrixElt).fullInterval)

                val xErrAbstr = (accC._2).getOrElse(x, DSAbstraction.empty())
                val updatedErrAbstr = xErrAbstr.addSpec(Set(MatrixIndex(i, j)), eltErrors(matrixElt).fullInterval)

                (eltRanges + (x -> updatedRangeAbstr), eltErrors + (x -> updatedErrAbstr))
            })

        })
        (matrixRanges, matrixErrs)

      case x@EveryNthVector(v, Int32Literal(n), Int32Literal(from)) =>
        val (vRanges, vErrs) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = vRanges(v)
        val origDSErr = vErrs(v)
        assert(from < origDSRange.dsSize, s"Start index is out of scope: $x, $v has ${origDSRange.dsSize} elements.")
        val eltsToGet = (from until origDSRange.dsSize).grouped(n).map(_.head).toSeq.sorted
        val newIndices = eltsToGet.indices
        val selected = eltsToGet.zip(newIndices).map({ case (i, toI) =>
          val range = origDSRange.at(i)
          val err = origDSErr.at(i)
          val newInd = Set(VectorIndex(toI).asInstanceOf[Index])
          (newInd -> range, newInd -> err)
        }).unzip
        val xDSRange = DSAbstraction(selected._1.toMap)
        val xDSErr = DSAbstraction(selected._2.toMap)
        (vRanges + (x->xDSRange), vErrs + (x->xDSErr))

      case x@EveryNthMatrix(m, Int32Literal(n), Int32Literal(from)) =>
        val (mRanges, mErrs) = eval(m, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = mRanges(m)
        val origDSErr = mErrs(m)
        assert(from < origDSRange.numRows, s"Start row index is out of scope: $x, $m has ${origDSRange.numRows} rows.")
        val rowsToGet = (from until origDSRange.numRows).grouped(n).map(_.head).toSeq
        val newRows = rowsToGet.indices
        val selected = rowsToGet.zip(newRows).map({ case (i, newRow) =>
          val inds = origDSRange.indicesAtRow(i)
          val ranges = origDSRange.getAbstractionOnlyAtIndices(inds).indexToRange.map({
            case (k, v) => k.map({ case MatrixIndex(_, jj) => MatrixIndex(newRow, jj).asInstanceOf[Index] }) -> v
          }).toSeq
          val errs = origDSErr.getAbstractionOnlyAtIndices(inds).indexToRange.map({
            case (k, v) => k.map({ case MatrixIndex(_, jj) => MatrixIndex(newRow, jj).asInstanceOf[Index] }) -> v
          })
          (ranges, errs)
        }).unzip
        val xDSRange = DSAbstraction(selected._1.flatten.toMap)
        val xDSErr = DSAbstraction(selected._2.flatten.toMap)
        (mRanges + (x->xDSRange), mErrs + (x->xDSErr))

      case x@MaxOf(v) =>
        val (vRanges, vErrs) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        // find the elements with the largest upper bound of the range
        val maxUBRange = vRanges(v).indexToRange.maxBy({ case (_, interval) => interval.xhi })
        val maxLB = vRanges(v).indexToRange.maxBy({ case (_, interval) => interval.xlo })._2.xlo
        val maxIndices = vRanges(v).indexToRange.filter({ case (_, v) => v.xhi == maxUBRange._2.xhi})

        // take the most precise range (among those with the same LUB), updabe with the max lower bound
        val xRange = vRanges(v).atIndicesJoint(maxIndices.minBy(_._2.width)._1).intersect(MPFRInterval(maxLB, maxUBRange._2.xhi)).get // here is definitely not None (largest upper bound must be at least as large as largest lower bound
        val xRangeDS = DSAbstraction(xRange)
        // take the worst-case estimate (union of error ranges) among those with the same LUB (of the value range)
        // todo better options?
        val xError = vErrs(v).atIndicesJoint(maxIndices.keySet.flatten)
        val xErrDS = DSAbstraction(xError)
        (vRanges + (x -> xRangeDS), vErrs + (x -> xErrDS))

      case x@MinOf(v) =>
        val (vRanges, vErrs) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val minUBRange = vRanges(v).indexToRange.minBy({ case (_, interval) => interval.xlo })
        val minIndices = vRanges(v).indexToRange.filter({ case (_, v) => v.xlo == minUBRange._2.xlo})

        // take the most precise range (among those with the same GLB)
        val xRange = vRanges(v).atIndicesJoint(minIndices.minBy(_._2.width)._1)
        val xRangeDS = DSAbstraction(xRange)
        // take the worst-case estimate (union of error ranges) among those with the same GLB (of the value range)
        // todo better options?
        val xError = vErrs(v).atIndicesJoint(minIndices.keySet.flatten)
        val xErrDS = DSAbstraction(xError)
        (vRanges + (x -> xRangeDS), vErrs + (x -> xErrDS))

      case x@SizeLength(v@VectorFromList(_, length)) =>
        val (vRanges, vErrs) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val addRanges = DSAbstraction(MPFRInterval(length))
        val addErrs = DSAbstraction(MPFRInterval.zero)
        (vRanges + (x -> addRanges), vErrs + (x -> addErrs))

      case x@SizeLength(ds) =>
        val (dsRanges, dsErrors) =
          if (rangesAbstr.contains(ds))
            (rangesAbstr, errorsAbstr)
          else
            eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val addRanges = DSAbstraction(MPFRInterval(dsRanges(ds).dsSize))
        val addErrs = DSAbstraction(MPFRInterval.zero)
        (dsRanges + (x -> addRanges), dsErrors + (x -> addErrs))

      case x@SizeNumRows(ds) =>
        val (dsRanges, dsErrors) =
          if (rangesAbstr.contains(ds))
            (rangesAbstr, errorsAbstr)
          else
            eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val addRanges = DSAbstraction(MPFRInterval(dsRanges(ds).numRows))
        val addErrs = DSAbstraction(MPFRInterval.zero)
        (dsRanges + (x -> addRanges), dsErrors + (x -> addErrs))

      case x@SizeNumCols(ds) =>
        val (dsRanges, dsErrors) =
          if (rangesAbstr.contains(ds))
            (rangesAbstr, errorsAbstr)
          else
            eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val addRanges = DSAbstraction(MPFRInterval(dsRanges(ds).numCols))
        val addErrs = DSAbstraction(MPFRInterval.zero)
        (dsRanges + (x -> addRanges), dsErrors + (x -> addErrs))

      case x@IfExpr(cond, thenn, elze) =>
        // todo also check that only integers are a part of condition
        assert(TreeOps.exists({case SizeLength(_) => true; case SizeNumRows(_) => true; case SizeNumCols(_) => true })(cond), "Currently only support conditions on the size of data structures.")
        val norm = normalizeCondition(cond)
        // todo this code is for a single condition only (no con-disjunction)
        val (condRanges, condErrs) = eval(norm, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val condIntervals = Seq(condRanges(norm).fullInterval) // todo something better?
        val holds = conditionHolds(cond, condIntervals)
        if (holds) {
          val (thenRanges, thenErrs) = eval(thenn, condRanges.removed(norm), condErrs.removed(norm), indices, unrollingScope = unrollingScope)
          (thenRanges + (x->thenRanges(thenn)), thenErrs + (x->thenErrs(thenn)))
        } else {
          val (elseRanges, elseErrs) = eval(elze, condRanges.removed(norm), condErrs.removed(norm), indices, unrollingScope = unrollingScope)
          (elseRanges + (x->elseRanges(elze)), elseErrs + (x->elseErrs(elze)))
        }

        // recursive call
      case x@FunctionInvocation(fdId, params, args, returnType) if fdId == currentFncId =>
        val (argsRanges, argsErrs) = args.foldLeft((rangesAbstr,errorsAbstr))({
          case (acc, argExpr) =>
            // todo indices for argExpr?
            eval(argExpr, acc._1, acc._2, indices, unrollingScope = unrollingScope)
        })
        val (updateRange, updateErrs) = args.zip(params).map({
          case (argExpr, argValdDef) =>
            val newVar = argValdDef.getType match {
              case RealType => Variable(argValdDef.id)
              case VectorType(_) => VectorLiteral(argValdDef.id)
              case MatrixType(_) => MatrixLiteral(argValdDef.id)
            }
            (newVar -> argsRanges(argExpr), newVar -> argsErrs(argExpr))
        }).unzip
        // expr is the function body
        // todo indices?
        val (resR, resE) = eval(expr, rangesAbstr++updateRange.toMap, errorsAbstr++updateErrs.toMap, indices, unrollingScope = true ) // todo inner scope?
        (argsRanges + (x-> resR(expr)), argsErrs + (x-> resE(expr)))

      case x@FilterIter(ds, ld@Lambda(args, body)) if !forceEval =>
        val (dsRanges, dsErrors) =
          if (rangesAbstr.contains(ds))
            (rangesAbstr, errorsAbstr)
          else
            eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val normalizedLambda = normalizeCondition(body)
        val lambdas = args.map({ case ValDef(id) => Variable(id) })
        assert(args.length == 1, s"Unexpected number of variables $args in $ld")
        val lambda = lambdas.head

        def evalCondition(cond: Expr): (Set[Index], Map[Expr, DSAbstraction] , Map[Expr, DSAbstraction]) = cond match {
          case And(nc) =>
            val fst = evalCondition(nc.head)
            // todo check if cond -> DSA needs to be added
            nc.tail.foldLeft(fst)({case (acc, x) =>
              val current = evalCondition(x)
              (current._1.intersect(acc._1), acc._2 + (x -> current._2(x)), acc._3 + (x -> current._3(x)))
            }) // indices that satisfy all conditions
          case Or(nc) =>
            val fst = evalCondition(nc.head)
            // todo check if cond -> DSA needs to be added
            nc.tail.foldLeft(fst)({case (acc, x) =>
              val current = evalCondition(x)
              (current._1 ++ acc._1, acc._2 + (x -> current._2(x)), acc._3 + (x -> current._3(x)))
            }) // indices that satisfy at least one condition
          case cond =>
            // eval for each index group separately
            val (updRanges, updErrors) = dsRanges(ds).indices.foldLeft((dsRanges,dsErrors))({
              case (acc, ind) =>
                eval(cond, acc._1 + (lambda -> dsRanges(ds)), acc._2 + (lambda -> dsErrors(ds)),
                  indices ++ Map(lambda -> Set(ind), cond -> Set(ind), ds -> Set(ind)), unrollingScope = unrollingScope)
            })
            val condIndices = updRanges(cond).indices.filter(ind =>
              conditionHolds(cond, Seq(updRanges(cond).at(ind)))
            )

            (condIndices, updRanges, updErrors)
        }

        val (satIndices, condRanges, condErrs) = evalCondition(normalizedLambda)
        //val affectedIndices = condRanges(ds).indices.intersect(satIndices)
        val improvedRanges = satIndices.map(ind => {
          val range = updateRange(ds, condRanges(ds).at(ind), replace{ case va@Variable(_) if va == lambda => ds }(body))
          if (range.isEmpty)
            None
          else Some(Set(ind) -> range.get)
        }).filter(_.isDefined).map(_.get).toMap
        val xRangeAbstr = condRanges(ds).getAbstractionAtIndices(satIndices).updateWith(improvedRanges)
        val xErrAbstr = condErrs(ds).getAbstractionAtIndices(satIndices)
        (condRanges + (x -> xRangeAbstr), condErrs + (x -> xErrAbstr))

      case x@MapIter(ds, Lambda(args, body)) if !forceEval =>
        val (dsRanges, dsErrors) =
          if (rangesAbstr.contains(ds))
            (rangesAbstr, errorsAbstr)
          else
            eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)

        val lambdas = args.map({ case ValDef(id) => Variable(id) })
        assert(lambdas.size == 1, s"Too many arguments in a map $x")
        val lambda = lambdas.head

        val (bodyRanges, bodyErrs) = dsRanges(ds).indices.map({
          ind =>
            val (rng, err) = eval(body, dsRanges + (lambda -> dsRanges(ds)), dsErrors + (lambda -> dsErrors(ds)),
              indices ++ Map(lambda -> Set(ind), body -> Set(ind)), unrollingScope = unrollingScope)
            (Set(ind) -> rng(body).fullInterval, Set(ind) -> err(body).fullInterval)
        }).unzip

        val xDSRange = DSAbstraction(bodyRanges.toMap)
        val xDSErr = DSAbstraction(bodyErrs.toMap)


        (dsRanges + (x -> xDSRange), dsErrors + (x -> xDSErr))

      // sum on vectors
      case x@Sum(ds, init) if !forceEval && isVector(ds) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (initRange, initErrs) = eval(init, dsRange, dsErrs, indices, unrollingScope = unrollingScope)
        val accId = FreshIdentifier("acc", RealType)
        val xId = FreshIdentifier("x", RealType)
        val args = Seq(ValDef(accId), ValDef(xId))
        val body = Plus(Variable(accId), Variable(xId))
        val unrollChunks = (0 until initRange(ds).dsSize).grouped(unfoldLimit)
        val chunkedUnrollRes = unrollChunks.foldLeft((initRange, initErrs, init))({
          case (acc, inds) =>
            // inline
            val unrolledFold = TreeOps.unrollFoldOnVector(ds, fromInd = inds.head, toInd = inds.last, acc._3, args, body)
            val (foldRanges, foldErrs) = eval(unrolledFold, acc._1, acc._2, indices, unrollingScope = true)

            val newId = FreshIdentifier(s"acc${inds.head}to${inds.last}", RealType)
            val additionalRanges = Map(unrolledFold -> foldRanges(unrolledFold), Variable(newId) -> foldRanges(unrolledFold))
            val additionalErrs = Map(unrolledFold -> foldErrs(unrolledFold), Variable(newId) -> foldErrs(unrolledFold))
            // passing only the end result to the next iter - otherwise if the body contains let statements, all exprs that use would take old cached values
            (initRange ++ additionalRanges, initErrs ++ additionalErrs, Variable(newId))
        })
        val unrolledEndExpr = chunkedUnrollRes._3
        (chunkedUnrollRes._1 + (x -> chunkedUnrollRes._1(unrolledEndExpr)), chunkedUnrollRes._2 + (x -> chunkedUnrollRes._2(unrolledEndExpr)))


        // fold on vectors
      case x@FoldIter(ds, init, Lambda(args, body)) if !forceEval && isVector(ds) =>
        val (initRange, initErrs) = eval(init, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val occur = allOccurrencesOfVars(body).toList.count(_ == Variable(args.head.id))
        val groupSize =  if (occur > occurrenceLimit) unfoldLimitHighOcc else unfoldLimit
        val unrollChunks = (0 until rangesAbstr(ds).dsSize).grouped(groupSize)
        val chunkedUnrollRes = unrollChunks.foldLeft((initRange, initErrs, init))({
          case (acc, inds) =>
            val unrolledFold =
              if (occur > occurrenceLimit) // use lets
                TreeOps.unrollFoldwLetsOnVector(ds, fromInd = inds.head, toInd = inds.last, acc._3, args, body)
              else // inline
                TreeOps.unrollFoldOnVector(ds, fromInd = inds.head, toInd = inds.last, acc._3, args, body)
            val (foldRanges, foldErrs) = eval(unrolledFold, acc._1, acc._2, indices, unrollingScope = true)

            val newId = FreshIdentifier(s"acc${inds.head}to${inds.last}", RealType)
            val additionalRanges = Map(unrolledFold -> foldRanges(unrolledFold), Variable(newId) -> foldRanges(unrolledFold))
            val additionalErrs = Map(unrolledFold -> foldErrs(unrolledFold), Variable(newId) -> foldErrs(unrolledFold))
            // passing only the end result to the next iter - otherwise if the body contains let statements, all exprs that use would take old cached values
            (initRange ++ additionalRanges, initErrs ++ additionalErrs, Variable(newId))
        })
        val unrolledEndExpr = chunkedUnrollRes._3
        (chunkedUnrollRes._1 + (x -> chunkedUnrollRes._1(unrolledEndExpr)), chunkedUnrollRes._2 + (x -> chunkedUnrollRes._2(unrolledEndExpr)))

        // fold on matrices (fold by row)
      case x@FoldIter(ds, init, Lambda(args, body)) if !forceEval && isMatrix(ds) =>
        val (initRange, initErrs) = eval(init, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val occur = allOccurrencesOfOpsOnVectors(body).toList.count(_ == Variable(args.head.id))
        val groupSize =  if (occur > occurrenceLimit) unfoldLimitHighOcc else unfoldLimit
        val unrollChunks = (0 until rangesAbstr(ds).numRows).grouped(groupSize)
        val chunkedUnrollRes = unrollChunks.foldLeft((initRange, initErrs, init, indices))({ case (acc, rowInds) =>
          val unrolledFold =
            if (occur > occurrenceLimit) // use lets
              TreeOps.unrollFoldwLetsOnMatrix(ds, fromInd = rowInds.head, toInd = rowInds.last, acc._3, args, body)
            else // inline
              TreeOps.unrollFoldOnMatrix(ds, fromInd = rowInds.head, toInd = rowInds.last, acc._3, args, body)
          val (foldRanges, foldErrs) = eval(unrolledFold, acc._1, acc._2, acc._4, unrollingScope = true)

          val newId = FreshIdentifier(s"acc${rowInds.head}to${rowInds.last}", VectorType(Seq(RealType)))
          val additionalRanges = Map(unrolledFold -> foldRanges(unrolledFold), VectorLiteral(newId) -> foldRanges(unrolledFold))
          val additionalErrs = Map(unrolledFold -> foldErrs(unrolledFold), VectorLiteral(newId) -> foldErrs(unrolledFold))

          // passing only the end result to the next iter - otherwise if the body contains let statements, all exprs that use would take old cached values
          (initRange ++ additionalRanges, initErrs ++ additionalErrs, VectorLiteral(newId), acc._4 + (VectorLiteral(newId) -> foldRanges(unrolledFold).indices, unrolledFold -> foldRanges(unrolledFold).indices))
        })
        val unrolledEndExpr = chunkedUnrollRes._3
        (chunkedUnrollRes._1 + (x -> chunkedUnrollRes._1(unrolledEndExpr)), chunkedUnrollRes._2 + (x -> chunkedUnrollRes._2(unrolledEndExpr)))

        // fold on elements of a matrix (fold by element)
      case x@FoldElemsIter(ds, init, Lambda(args, body)) if !forceEval && isMatrix(ds) =>
        val (initRange, initErrs) = eval(init, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val occur = allOccurrencesOfVars(body).toList.count(_ == Variable(args.head.id))
        val groupSize =  if (occur > occurrenceLimit) unfoldLimitHighOcc else unfoldLimit
        val unrollChunks = genSortedMatrixIndices(0, rangesAbstr(ds).numRows, 0, rangesAbstr(ds).numCols).grouped(groupSize)
        val chunkedUnrollRes = unrollChunks.foldLeft((initRange, initErrs, init, System.nanoTime()))({
          case (acc, inds) =>
            val fromInd = inds.head.asInstanceOf[MatrixIndex]
            val toInd = inds.last.asInstanceOf[MatrixIndex]
            val unrolledFold =
              if (occur > occurrenceLimit) // use lets
                TreeOps.unrollFoldElemsWLetsOnMatrix(ds, fromInd = fromInd, toInd = toInd, rangesAbstr(ds).numCols, acc._3, args, body)
              else // inline
                TreeOps.unrollFoldOnMatrixElts(ds, inds, acc._3, args, body)
            val (foldRanges, foldErrs) = eval(unrolledFold, acc._1, acc._2, indices, unrollingScope = true)

            val newId = FreshIdentifier(s"acc${fromInd.i}c${fromInd.j}to${toInd.i}c${toInd.j}", RealType)
            val additionalRanges = Map(unrolledFold -> foldRanges(unrolledFold), Variable(newId) -> foldRanges(unrolledFold))
            val additionalErrs = Map(unrolledFold -> foldErrs(unrolledFold), Variable(newId) -> foldErrs(unrolledFold))

            // passing only the end result to the next iter - otherwise if the body contains let statements, all exprs that use would take old cached values
            (initRange ++ additionalRanges, initErrs ++ additionalErrs, Variable(newId), System.nanoTime())
        })
        val unrolledEndExpr = chunkedUnrollRes._3
        (chunkedUnrollRes._1 + (x -> chunkedUnrollRes._1(unrolledEndExpr)), chunkedUnrollRes._2 + (x -> chunkedUnrollRes._2(unrolledEndExpr)))


      // sliding window on vectors
      case x@SlideReduceIter(ds, Int32Literal(size),Int32Literal(step), Lambda(args, body)) if !forceEval && isVector(ds) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val dsRangeAbs = dsRange(ds)
        assert((dsRangeAbs.dsSize - size) % step == 0,
          s"The size ($size) and step ($step) of a sliding window do not match the data structure size (${dsRangeAbs.dsSize})")
        val horizontalSteps = (dsRangeAbs.dsSize - size)/step + 1
        val slideResultIndices = (0 until horizontalSteps).map(i=> VectorIndex(i).asInstanceOf[Index]).sorted
        //// for each new index evaluate the body and record the result
        val iterV = VectorLiteral(args.head.id)
        val (xRangeMap, xErrMap) = slideResultIndices.map({
          case index =>
            val from = index.i*step
            val origDSIndices = genSortedVectorIndices(from, from+size)
            val iterDSIndices = genSortedVectorIndices(0, size)
            // take ranges from the original DS and map them to new indices (iterator variable)
            val mappedRanges = iterDSIndices.zip(origDSIndices).map({ case (newInd, oldInd) => Set(newInd) -> dsRangeAbs.at(oldInd)}).toMap
            val mappedErrs = iterDSIndices.zip(origDSIndices).map({ case (newInd, oldInd) => Set(newInd) -> dsErrs(ds).at(oldInd)}).toMap

            val updRangesDS = dsRange + (iterV -> DSAbstraction(mappedRanges).regroup())
            val updErrsDS = dsErrs + (iterV -> DSAbstraction(mappedErrs).regroup())
            val (iterRanges, iterErrs) = eval(body, updRangesDS, updErrsDS, indices + (iterV -> iterDSIndices.toSet), unrollingScope = unrollingScope)

            val newEltRangeMap = Set(index) -> iterRanges(body).fullInterval // adjust if the result is a DS (not a Real type)
            val newEltErrMap = Set(index) -> iterErrs(body).fullInterval // adjust if the result is a DS (not a Real type)
            (newEltRangeMap, newEltErrMap)
        }).unzip
        val xRangeDS = DSAbstraction(xRangeMap.toMap).regroup()
        val xErrDS = DSAbstraction(xErrMap.toMap).regroup()
        (dsRange + (x -> xRangeDS), dsErrs + (x -> xErrDS))

      // sliding window on matrices
      // only handling lambdas that return Real
      case x@SlideReduceIter(ds, Int32Literal(size),Int32Literal(step), Lambda(args, body)) if !forceEval && isMatrix(ds) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val dsRangeAbs = dsRange(ds)
        assert((dsRangeAbs.numCols - size) % step == 0 && (dsRangeAbs.numRows - size) % step == 0,
            s"The size ($size) and step ($step) of a sliding window do not match the data structure size (${dsRangeAbs.numRows}x${dsRangeAbs.numCols})")
        val horizontalSteps = (dsRangeAbs.numCols - size)/step + 1
        val verticalSteps = (dsRangeAbs.numRows - size)/step + 1
        val slideResultIndices = genSortedMatrixIndices(0, horizontalSteps, 0, verticalSteps)
        // for each new index evaluate the body and record the result
        // todo do we need intermediate values? e.g. after sliding the window once/twice/...?
        val iterM = MatrixLiteral(args.head.id)
        val (xRangeMap, xErrMap) = slideResultIndices.map({
          case index@MatrixIndex(i,j) =>
            val fromRow = i*step
            val fromCol = j*step
            val origDSIndices = genSortedMatrixIndices(fromRow, fromRow+size, fromCol, fromCol+size)
            val iterDSIndices = genSortedMatrixIndices(0,size, 0, size)
            // take ranges from the original DS and map them to new indices (iterator variable)
            val mappedRanges = iterDSIndices.zip(origDSIndices).map({ case (newInd, oldInd) => Set(newInd) -> dsRangeAbs.at(oldInd)}).toMap
            val mappedErrs = iterDSIndices.zip(origDSIndices).map({ case (newInd, oldInd) => Set(newInd) -> dsErrs(ds).at(oldInd)}).toMap

            val updRangesDS = dsRange + (iterM -> DSAbstraction(mappedRanges))
            val updErrsDS = dsErrs + (iterM -> DSAbstraction(mappedErrs))
            val (iterRanges, iterErrs) = eval(body, updRangesDS, updErrsDS, indices + (iterM -> iterDSIndices.toSet), unrollingScope = unrollingScope)

            val newEltRangeMap = Set(index.asInstanceOf[Index]) -> iterRanges(body).fullInterval // adjust if the result is a DS (not a Real type)
            val newEltErrMap = Set(index.asInstanceOf[Index]) -> iterErrs(body).fullInterval // adjust if the result is a DS (not a Real type)
            (newEltRangeMap, newEltErrMap)
        }).unzip
        val xRangeDS = DSAbstraction(xRangeMap.toMap)
        val xErrDS = DSAbstraction(xErrMap.toMap)
        (dsRange + (x -> xRangeDS), dsErrs + (x -> xErrDS))

      // enumerate and map rows on matrices
      // only handling lambdas that return Vector
      case x@EnumRowsAndMap(ds, Lambda(args, body)) if !forceEval && isMatrix(ds) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val dsRangeAbs = dsRange(ds)
        // for each row evaluate the body and record the result
        val iterI = Variable(args.head.id)
        val iterV = VectorLiteral(args(1).id)
        val (xRangeMap, xErrMap) = (0 until dsRangeAbs.numRows).map(i =>
          {
            // index integer part
            val iDSRange = DSAbstraction(MPFRInterval(i))
            val iDSErr = DSAbstraction(MPFRInterval.zero)

            // vector part
            val origDSIndices = dsRangeAbs.indicesAtRow(i)
            val vectorDS = dsRangeAbs.getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()
            val vectorErr = dsErrs(ds).getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()
            val vectorIndices = vectorDS.indices

            val updRangesDS = dsRange + (iterV -> vectorDS, iterI -> iDSRange)
            val updErrsDS = dsErrs + (iterV -> vectorErr, iterI -> iDSErr)
            val (iterRanges, iterErrs) = eval(body, updRangesDS, updErrsDS, indices + (iterV -> vectorIndices), unrollingScope = unrollingScope)

            val newEltRangeMap = vectorIndices.map({
              case vi@VectorIndex(j) => Set(MatrixIndex(i,j).asInstanceOf[Index]) -> iterRanges(body).at(vi) // todo assumption: resulting vector has the same size as original row
            })
            val newEltErrMap = vectorIndices.map({
              case vi@VectorIndex(j) => Set(MatrixIndex(i,j).asInstanceOf[Index]) -> iterErrs(body).at(vi)
            })
            (newEltRangeMap, newEltErrMap)
        }).unzip
        val xRangeDS = DSAbstraction(xRangeMap.flatten.toMap)
        val xErrDS = DSAbstraction(xErrMap.flatten.toMap)
        (dsRange + (x -> xRangeDS), dsErrs + (x -> xErrDS))

      // enumerate sliding windows (sub-vectors) and flat map over them
      // only handling lambdas that return Vector
      case x@EnumSlideFlatMap(ds, Int32Literal(size), Lambda(args, body)) if !forceEval && isVector(ds) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val dsRangeAbs = dsRange(ds)
        // for each row evaluate the body and record the result
        val iterI = Variable(args.head.id)
        val iterV = VectorLiteral(args(1).id)
        val steps = dsRangeAbs.dsSize / size
        assert(dsRangeAbs.dsSize % size == 0, s"The size of sliding window on $ds does not fit the number of elements: ${dsRangeAbs.dsSize} is not divisible by $size.")
        val (xRangeMap, xErrMap) = (0 until steps).map(i =>
        {
          // index integer part
          val iDSRange = DSAbstraction(MPFRInterval(i))
          val iDSErr = DSAbstraction(MPFRInterval.zero)

          // sliding window part
          val origDSIndices = (i*size until (i+1)*size).map(x=>VectorIndex(x).asInstanceOf[Index])
          val sliceIndices = (0 until size).map(x => VectorIndex(x).asInstanceOf[Index])
          val origDS = dsRangeAbs.getAbstractionOnlyAtIndices(origDSIndices.toSet)
          val origErr = dsErrs(ds).getAbstractionOnlyAtIndices(origDSIndices.toSet)
          val sliceMap = sliceIndices.map({
            case vi@VectorIndex(j) => Set(vi.asInstanceOf[Index]) -> origDS.at(j+i*size)
          }).toMap
          val sliceErrMap = sliceIndices.map({
            case vi@VectorIndex(j) => Set(vi.asInstanceOf[Index]) -> origErr.at(j+i*size)
          }).toMap

          val updRangesDS = dsRange + (iterV -> DSAbstraction(sliceMap).regroup(), iterI -> iDSRange)
          val updErrsDS = dsErrs + (iterV -> DSAbstraction(sliceErrMap).regroup(), iterI -> iDSErr)
          val (iterRanges, iterErrs) = eval(body, updRangesDS, updErrsDS, indices + (iterV -> sliceIndices.toSet), unrollingScope = unrollingScope)

          val newEltRangeMap = sliceIndices.map({
            case vi@VectorIndex(j) => Set(VectorIndex(j+i*size).asInstanceOf[Index]) -> iterRanges(body).at(vi) // todo assumption: resulting vector has the same size as original slice
          })
          val newEltErrMap = sliceIndices.map({
            case vi@VectorIndex(j) => Set(VectorIndex(j+i*size).asInstanceOf[Index]) -> iterErrs(body).at(vi)
          })
          (newEltRangeMap, newEltErrMap)
        }).unzip
        val xRangeDS = DSAbstraction(xRangeMap.flatten.toMap).regroup()
        val xErrDS = DSAbstraction(xErrMap.flatten.toMap).regroup()
        (dsRange + (x -> xRangeDS), dsErrs + (x -> xErrDS))

      case x@Let(id, value, body) =>
        val (valRanges, valErrs) = eval(value, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val binder =
          if (isVector(value))
            VectorLiteral(id)
          else if (isMatrix(value))
            MatrixLiteral(id)
          else Variable(id)
        val (letRanges, letErrs) = eval(body, valRanges + (binder -> valRanges(value)), valErrs + (binder -> valErrs(value)), indices + (binder -> valRanges(value).indices), unrollingScope = unrollingScope)
        (letRanges + (x -> letRanges(body)), letErrs + (x -> letErrs(body)))

      case x@CrossProduct(lhs, rhs) if isVector(lhs) && isVector(rhs) =>
        /// special case (multiplication of complex numbers)
        // (a+ib)(c+id) = ac-bd + i(ad+bc)
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        assert(rhsRanges(lhs).dsSize == 2 && rhsRanges(rhs).dsSize == 2,
          s"Cannot apply cross product to vectors with length neq 2 $lhs:${rhsRanges(lhs).dsSize} and $rhs:${rhsRanges(rhs).dsSize}")
        val a = VectorElement(lhs, Int32Literal(0))
        val b = VectorElement(lhs, Int32Literal(1))
        val c = VectorElement(rhs, Int32Literal(0))
        val d = VectorElement(rhs, Int32Literal(1))
        val firstElt = Minus(Times(a,c), Times(b,d))
        val (fstRanges, fstErrs) = eval(firstElt, rhsRanges, rhsErrors, indices, unrollingScope = unrollingScope)
        val secondElt = Plus(Times(a,d), Times(b,c))
        val (sndRanges, sndErrs) = eval(secondElt, rhsRanges, rhsErrors, indices, unrollingScope = unrollingScope)
        val xDSRange = DSAbstraction(Map(Set(VectorIndex(0).asInstanceOf[Index]) -> fstRanges(firstElt).fullInterval,
          Set(VectorIndex(1).asInstanceOf[Index]) -> sndRanges(secondElt).fullInterval))
        val xDSErr = DSAbstraction(Map(Set(VectorIndex(0).asInstanceOf[Index]) -> fstErrs(firstElt).fullInterval,
          Set(VectorIndex(1).asInstanceOf[Index]) -> sndErrs(secondElt).fullInterval))
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErr))

      case x@CrossProduct(lhs, rhs) if isMatrix(lhs) && isVector(rhs) =>
        /// i-th row x vector
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        assert(rhsRanges(lhs).numCols == rhsRanges(rhs).dsSize,
          s"Wrong dimensions for multiplication of matrix with vector $lhs:${rhsRanges(lhs).dsSize} and $rhs:${rhsRanges(rhs).dsSize}")
        val mID = FreshIdentifier("mElt", RealType)
        val vID = FreshIdentifier("vElt", RealType)
        val timesExpr = Times(Variable(mID), Variable(vID))
        //val sumExpr = Plus(Variable(mID), Variable(vID))
        val rows: (Seq[(Set[Index], MPFRInterval)], Seq[(Set[Index], MPFRInterval)]) = (0 until rhsRanges(lhs).numRows).map(i => {
          // vector part
          val origDSIndices = rhsRanges(lhs).indicesAtRow(i)
          val vectorDS = rhsRanges(lhs).getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()
          val vectorErr = rhsErrors(lhs).getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()

          val toSum = (0 until vectorDS.dsSize).map(j => {
            val mElt = vectorDS.at(j)
            val vElt = rhsRanges(rhs).at(j)
            val mEltErr = vectorErr.at(j)
            val vEltErr = rhsErrors(rhs).at(j)
            val mID = FreshIdentifier(s"mElt${i}_$j", RealType)
            val vID = FreshIdentifier(s"vElt${i}_$j", RealType)
            val timesExpr = Times(Variable(mID), Variable(vID))
            (timesExpr,
              Map(Variable(mID) -> DSAbstraction(mElt), Variable(vID) -> DSAbstraction(vElt)),
              Map(Variable(mID) -> DSAbstraction(mEltErr), Variable(vID) -> DSAbstraction(vEltErr)))
          })

          val sumExpr = toSum.tail.foldLeft((toSum.head._1.asInstanceOf[Expr], rhsRanges ++ toSum.head._2, rhsErrors ++ toSum.head._3))({
            case (acc, x) =>
              val sumE = Plus(acc._1, x._1)
              val tmp = eval(sumE, acc._2 ++ x._2, acc._3 ++ x._3, indices, unrollingScope = unrollingScope)
              (sumE, tmp._1, tmp._2)
          })
          val endExpr = sumExpr._1
          (Set(VectorIndex(i).asInstanceOf[Index]) -> sumExpr._2(endExpr).fullInterval, Set(VectorIndex(i).asInstanceOf[Index]) -> sumExpr._3(endExpr).fullInterval)
        }).unzip

        val xDSRange = DSAbstraction(rows._1.toMap)
        val xDSErr = DSAbstraction(rows._2.toMap)
        //println(s"$lhs x $rhs = \n ${xDSRange.regroup()}")
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErr))

      case x@CrossProduct(lhs, rhs) if isMatrix(lhs) && isMatrix(rhs) =>
        /// i-th row x vector
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        assert(rhsRanges(lhs).numCols == rhsRanges(rhs).numRows,
          s"Wrong dimensions for multiplication of matrices $lhs:[${rhsRanges(lhs).numRows}x${rhsRanges(lhs).numCols}] and [$rhs:${rhsRanges(rhs).numRows}x${rhsRanges(rhs).numRows}]")
        val rows: (Seq[(Set[Index], MPFRInterval)], Seq[(Set[Index], MPFRInterval)]) = (0 until rhsRanges(lhs).numRows).flatMap(i => {
          val newMatrixElts = (0 until rhsRanges(rhs).numCols).map(k => {
          //var sumStr: String = ""
          val toSum = (0 until rhsRanges(lhs).numCols).map(j => {
            // i - fixed row, k - fixed column, j varies
            // lhs
            val lhsRng = rhsRanges(lhs).at(MatrixIndex(i, j))
            val lhsErr = rhsErrors(lhs).at(MatrixIndex(i, j))
            // rhs
            val rhsRng = rhsRanges(rhs).at(MatrixIndex(j, k))
            val rhsErr = rhsErrors(rhs).at(MatrixIndex(j, k))
            val lhsID = FreshIdentifier(s"lhsElt${i}_$j", RealType)
            val rhsID = FreshIdentifier(s"rhsElt${j}_$k", RealType)
            val timesExpr = Times(Variable(lhsID), Variable(rhsID))
            //sumStr+= s"$lhs[$i,$j] * $rhs[$j, $k] +"

            (timesExpr,
              Map(Variable(lhsID) -> DSAbstraction(lhsRng), Variable(rhsID) -> DSAbstraction(rhsRng)),
              Map(Variable(lhsID) -> DSAbstraction(lhsErr), Variable(rhsID) -> DSAbstraction(rhsErr)))
          })
            //_reporter.info(sumStr)

          val sumExpr = toSum.tail.foldLeft((toSum.head._1.asInstanceOf[Expr], rhsRanges ++ toSum.head._2, rhsErrors ++ toSum.head._3))({
            case (acc, x) =>
              val sumE = Plus(acc._1, x._1)
              val tmp = eval(sumE, acc._2 ++ x._2, acc._3 ++ x._3, indices, unrollingScope = unrollingScope)
              (sumE, tmp._1, tmp._2)
          })
          val endExpr = sumExpr._1
          (Set(MatrixIndex(i,k).asInstanceOf[Index]) -> sumExpr._2(endExpr).fullInterval, Set(MatrixIndex(i,k).asInstanceOf[Index]) -> sumExpr._3(endExpr).fullInterval)
          })
          newMatrixElts
        }).unzip

        val xDSRange = DSAbstraction(rows._1.toMap)
        val xDSErr = DSAbstraction(rows._2.toMap)
        // println(xDSRange.regroup())
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErr))

      // unary arithmetic operations on DS (includes ElemFnc)
      case ArithOperator(es, fnc) if !forceEval && es.length == 1 && hasDsType(es.head) =>
        val arg = es.head
        val (argRanges, argErrors) = eval(arg, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope) // todo indices: for input DS is already there, others?
        val operatorApplied = fnc(es)
        val (opRanges, opErrs) = argRanges(arg).indices.map({ indexInArg =>
          val (partRanges, partErrs) = eval(operatorApplied, argRanges, argErrors, indices + (arg -> Set(indexInArg)), path, forceEval = true, unrollingScope = unrollingScope)
          val newRangeMap = Set(indexInArg) -> partRanges(operatorApplied).fullInterval
          val newErrMap = Set(indexInArg) -> partErrs(operatorApplied).fullInterval
          (newRangeMap, newErrMap)
        }).unzip

        (argRanges + (operatorApplied -> DSAbstraction(opRanges.toMap)),
          argErrors + (operatorApplied -> DSAbstraction(opErrs.toMap)))

      // unary arithmetic operations on DS operations (of Real type)
      case ArithOperator(es, fnc) if !forceEval && es.length == 1 && isDsExpr(es.head) && es.head.getType == RealType =>
        val arg = es.head
        val (argRanges, argErrors) = eval(arg, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope) // todo indices: for input DS is already there, others?
        val operatorApplied = fnc(es)
          eval(operatorApplied, argRanges, argErrors, indices, path, forceEval = true, unrollingScope = unrollingScope) // todo indices ? + (arg -> Set())

        // arithmetic operations on DS (both operands are DS)
      // todo dstype is sufficient? && es.forall(isDsExpr)
      case ArithOperator(es, fnc) if !forceEval && es.forall(hasDsType) =>
        assert(es.length == 2, "Arithmetic operations on data structures with more than two operands are currently not supported.")

        val lhs = es.head
        val rhs = es(1)
        val lhsIndices = if (rangesAbstr.contains(lhs)) indices + (lhs -> rangesAbstr(lhs).indices) else indices
        val rhsIndices = if (rangesAbstr.contains(rhs)) indices + (rhs -> rangesAbstr(rhs).indices) else indices
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, lhsIndices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, rhsIndices, unrollingScope = unrollingScope) // todo is it ok to call with results of lhs eval?

        val operatorApplied = fnc(es)
        // assume lhs and rhs indices matchs
        val overlap = lhsRanges(lhs).indices.map(lIndex => {
          val indConfig = Map(lhs -> Set(lIndex), rhs -> Set(lIndex), operatorApplied -> Set(lIndex))
          val newRanges = eval(operatorApplied, rhsRanges, rhsErrors, indices ++ indConfig, path, forceEval = true, unrollingScope = unrollingScope)
          val range = newRanges._1(operatorApplied).at(lIndex)
          val error = newRanges._2(operatorApplied).at(lIndex)
          (Set(lIndex) -> range, Set(lIndex) -> error)
        })
        val (opAppliedRanges, opAppliedErrors) = overlap.unzip
        val dsRangesOpApplied = DSAbstraction(opAppliedRanges.toMap)
        val dsErrorsOpApplied = DSAbstraction(opAppliedErrors.toMap)
        (rhsRanges + (operatorApplied -> dsRangesOpApplied), rhsErrors + (operatorApplied -> dsErrorsOpApplied))

      // arithmetic operations on DS with constants/real variables
      case ArithOperator(es, fnc) if !forceEval && hasDsType(es.head) && (es(1).getType == RealType || es(1).getType == Int32Type) =>
        assert(es.length == 2, "Arithmetic operations on data structures with more than two operands are currently not supported.")
        val lhs = es.head
        val rhs = es(1)
        val lhsIndices = if (rangesAbstr.contains(lhs)) indices + (lhs -> rangesAbstr(lhs).indices) else indices
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, lhsIndices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, lhsIndices, unrollingScope = unrollingScope) // todo is it ok to call with results of lhs eval?

        val operatorApplied = fnc(es)
        val (opRanges, opErrs) = rhsRanges(lhs).indices.map({ indexLhs =>
            val (partRanges, partErrs) = eval(operatorApplied, rhsRanges, rhsErrors, indices + (lhs -> Set(indexLhs)), path, forceEval = true, unrollingScope = unrollingScope)
            val newRangeMap = Set(indexLhs) -> partRanges(operatorApplied).fullInterval
            val newErrMap = Set(indexLhs) -> partErrs(operatorApplied).fullInterval
            (newRangeMap, newErrMap)
        }).unzip

        (rhsRanges + (operatorApplied -> DSAbstraction(opRanges.toMap)),
          rhsErrors + (operatorApplied -> DSAbstraction(opErrs.toMap)))

      // arithmetic operations on Reals (operands may still include DS expressions, e.g. x.at(i)
      case ArithOperator(es, fnc) if !forceEval && es.exists(isDsExpr) =>
        assert(es.length == 2, "Arithmetic operations on data structures with more than two operands are currently not supported.")

        val lhs = es.head
        val rhs = es(1)
        val lhsIndices = if (rangesAbstr.contains(lhs)) indices + (lhs -> rangesAbstr(lhs).indices) else indices
        val rhsIndices = if (rangesAbstr.contains(rhs)) indices + (rhs -> rangesAbstr(rhs).indices) else indices
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, lhsIndices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, rhsIndices, unrollingScope = unrollingScope) // todo is it ok to call with results of lhs eval?

        val operatorApplied = fnc(es)
        val operatorIndices = rhsRanges(lhs).indices.intersect(rhsRanges(rhs).indices)
        val indConfig = indices ++ Map(lhs -> lhsRanges(lhs).indices, rhs -> rhsRanges(rhs).indices, operatorApplied -> operatorIndices)
        eval(operatorApplied, rhsRanges, rhsErrors, indConfig, path, forceEval = true, unrollingScope = unrollingScope)
        // todo save result above?

      case x@RealLiteral(_) if rangesAbstr.contains(x) =>
        (rangesAbstr, errorsAbstr)

      case x if !isDsExpr(x) || forceEval =>
        if (indices.isEmpty)
          _reporter.fatalError("Empty index sequence for all expressions")
        val rangeMap = toIntermediateRanges(indices, rangesAbstr)
        val errMap = toIntermediateErrors(indices, errorsAbstr, precisionMap, constPrecision)
        val (_, intermedRanges) = evalRange[MPFRInterval](x, Map(), MPFRInterval.apply, precompIntermRanges = rangeMap)
        val withIntervals = intermedRanges.map({case (k,v) => k-> Interval.fromMPFR(v)})
        val (_, intermedErrs) = evalRoundoff[MPFRInterval](x, withIntervals,
          precisionMap,
          Map(),
          zeroError = MPFRInterval.zero,
          fromError = MPFRInterval.+/-,
          interval2T = MPFRInterval.apply,
          constantsPrecision = constPrecision,
          trackRoundoffErrs,
          precomputedIntermedErrs = errMap
        )
        //val intervalIntermedErrs = intermedErrs.map({case (k,v) => k-> v.toInterval})
        val updatedRangeAbstr = updateDSWithRanges(x, indices, rangesAbstr, intermedRanges, path)
        val updatedErrorAbstr = updateDSWithRanges(x, indices, errorsAbstr, intermedErrs, path)

        (updatedRangeAbstr, updatedErrorAbstr)

      case x => throw DaisyFatalError(s"Unknown expression $x")
    }
}
    // prepare arguments for evaluation
    // values for RealType variables
    val inputForRealVars: Map[Identifier, Interval] = initValMap.filter({
      case (id, _) => id.getType match {
        case VectorType(_) => false
        case MatrixType(_) => false
        case _ => true
      }
    })
    val realRanges = inputForRealVars.map({
      case (id, interval) if id.getType.equals(RealType) =>
        val ds = DSAbstraction(Map(Set[Index]() -> MPFRInterval(interval)))
        Some(Variable(id) -> ds)
      case _ => None
    }).filter(_.nonEmpty).map(_.get).toMap
    val realErrors = inputErrorMap.map({
      case (id, err) if id.getType.equals(RealType) =>
        val ds = DSAbstraction(Map(Set[Index]() -> MPFRInterval(err)))
        Some(Variable(id) -> ds)
      case _ => None
    }).filter(_.nonEmpty).map(_.get).toMap
    val realIndicesDummy = inputForRealVars.keySet.map(k => Variable(k) -> Set[Index]()).toMap

    // values for data structures
    val dsErrors: Map[Expr, DSAbstraction] = dsas.keys.map({
      case k@Variable(id) =>
        val ds = DSAbstraction(Map(dsas(k).indices -> MPFRInterval(inputErrorMap(id))))
        k -> ds
    }).toMap
    val dsIndices = dsas.map({case (k, v) => k -> v.indices})

    val (rangeNew, errNew) = eval(expr, dsas ++ realRanges, dsErrors ++ realErrors, dsIndices ++ realIndicesDummy)

    val err = MPFRInterval.maxAbs(errNew(expr).fullInterval)
    val range = rangeNew(expr).fullInterval
    //println(rangeNew(expr))
    //(err: Rational, range: Interval)
    (Rational.fromMPFR(err), Interval.fromMPFR(range))
  }

  private def flipDSIndices(origDSRange: DSAbstraction, row: Boolean): DSAbstraction = {
    val middle = if (row) Math.ceil(origDSRange.numRows / 2).toInt else Math.ceil(origDSRange.numCols / 2).toInt
    val updMap = (0 until middle).flatMap(i => {
      val from:Seq[Index] =
        if (row)
          origDSRange.indicesAtRow(i).map(_.asInstanceOf[MatrixIndex]).toSeq
        else origDSRange.indicesAtColunm(i).map(_.asInstanceOf[MatrixIndex]).toSeq
      val to: Seq[Index] =
        if (row)
          origDSRange.indicesAtRow(origDSRange.numRows - 1 - i).map(_.asInstanceOf[MatrixIndex]).toSeq
        else origDSRange.indicesAtColunm(origDSRange.numCols - 1 - i).map(_.asInstanceOf[MatrixIndex]).toSeq
      val newMap: Seq[(Set[Index], MPFRInterval)] = (from.sorted.zip(to.sorted)).flatMap({
        case (fromI, toI) =>
          val fromVal = origDSRange.at(fromI)
          val toVal = origDSRange.at(toI)
          Set(Set(fromI) -> toVal, Set(toI) -> fromVal)
      })
      newMap
    }).toMap
    val middleRowOrColumn =
      if ((row && origDSRange.numRows % 2 > 0) || (!row && origDSRange.numCols % 2 > 0))
        if (row)
          origDSRange.getAbstractionOnlyAtIndices(origDSRange.indicesAtRow(middle)).indexToRange
        else origDSRange.getAbstractionOnlyAtIndices(origDSRange.indicesAtColunm(middle)).indexToRange
      else Map()

    DSAbstraction(updMap ++ middleRowOrColumn)
  }


  /**
   * Normalizes conditions to check whether a difference between left- and right-hand sides are greater than zero.
   * Examples:
   *   1) a <= 5 becomes 5 - a
   *   2) a >= 5 becomes a - 5
   * @param e conditional expression ot be normalized
   * @return normalized expression that is >= 0
   */
  private def normalizeCondition(e: Expr): Expr = e match {
    case GreaterThan(x, RealLiteral(Rational.zero)) => x
    case GreaterThan(x, y) => Minus(x, y)
    case GreaterEquals(x, RealLiteral(Rational.zero)) => x
    case GreaterEquals(x, y) => Minus(x, y)
    case LessThan(RealLiteral(Rational.zero), y) => y
    case LessThan(x, y) => Minus(y, x)
    case LessEquals(RealLiteral(Rational.zero), y) => y
    case LessEquals(x, y) => Minus(y, x)
    case Equals(x, y) => Minus(y, x)
    case And(ls) => And(ls.map(normalizeCondition))
    case Or(ls) => Or(ls.map(normalizeCondition))
  }

  /**
   * Check whether the normalized condition holds
   * @param e boolean expression in normalized form / not normalized for deciding in if-then-else
   * @param lhsRanges sequence of ranges that left-hand-sides of individual comparisons evaluated to. Length is > 1 only for conjunctions and disjunctions
   * @return true/false whether a boolean predicate holds
   */
  private def conditionHolds(e:Expr, lhsRanges: Seq[MPFRInterval]): Boolean = e match {
    case And(nc) =>
      val res = nc.zip(lhsRanges).map(x => conditionHolds(x._1, Seq(x._2)))
      res.tail.foldLeft(res.head)({case (acc, x) => acc && x })
    case Or(nc) =>
      val res = nc.zip(lhsRanges).map(x => conditionHolds(x._1, Seq(x._2)))
      res.tail.foldLeft(res.head)({case (acc, x) => acc || x })
    case LessEquals(lhs, rhs) if lhsRanges.head.isPointRange => lhsRanges.head.xhi == MPFRFloat.zero // definitely true, [0,0] >= 0
    case GreaterEquals(lhs, rhs) if lhsRanges.head.isPointRange => lhsRanges.head.xhi == MPFRFloat.zero // definitely true, [0,0] >= 0
    case Equals(lhs, rhs) if lhsRanges.head.isPointRange => lhsRanges.head.xhi == MPFRFloat.zero // definitely true, [0,0] >= 0
    case LessThan(lhs, rhs) if lhsRanges.head.isPointRange => lhsRanges.head.xlo > MPFRFloat.zero // definitely true, [1,10] >= 0
    case GreaterThan(lhs, rhs) if lhsRanges.head.isPointRange => lhsRanges.head.xlo > MPFRFloat.zero // definitely true, [2,20] >= 0 on normalized expression
    case _ => !(lhsRanges.head.xhi <= MPFRFloat.zero) // definitely false, e.g. [-10.-1] >= 0
    // todo what to do with over-approximation: when need to decide [-5,5] >= 0?
  }

  /**
   *  Returns an updated range that satisfies the conditions. Conditions must be
   *  a comparison between a variable and a constant or a conjunction of such comparisons
   * @param ds the data structure variable
   * @param dsRange the ds variable's range (current)
   * @param lambda condition according to which ds range should be improved
   * @return Option[MPFRInterval] improved range or None if conditions couldn't be satisfied
   */
  private def updateRange(ds: Expr, dsRange: MPFRInterval, lambda: Expr): Option[MPFRInterval] = lambda match {
      // var >= constant
    case GreaterThan(x, RealLiteral(r)) if x == ds =>
      val low = MPFRFloat.max(dsRange.xlo, MPFRFloat.fromRational(r)) // lambdaRanges.head.xlo)
      if (low > dsRange.xhi) None else Some(MPFRInterval(low, dsRange.xhi))
    case GreaterEquals(x, RealLiteral(r)) if x == ds =>
      val low = MPFRFloat.max(dsRange.xlo, MPFRFloat.fromRational(r)) // lambdaRanges.head.xlo)
      if (low > dsRange.xhi) None else Some(MPFRInterval(low, dsRange.xhi))
    case LessThan(RealLiteral(r), x) if x == ds =>
      val low = MPFRFloat.max(dsRange.xlo, MPFRFloat.fromRational(r)) // lambdaRanges.head.xlo)
      if (low > dsRange.xhi) None else Some(MPFRInterval(low, dsRange.xhi))
    case LessEquals(RealLiteral(r), x) if x == ds =>
      val low = MPFRFloat.max(dsRange.xlo, MPFRFloat.fromRational(r)) // lambdaRanges.head.xlo)
      if (low > dsRange.xhi) None else Some(MPFRInterval(low, dsRange.xhi))
      // var <= constant
    case GreaterThan(RealLiteral(r), x) if x == ds =>
      val hi = MPFRFloat.min(dsRange.xhi, MPFRFloat.fromRational(r)) //lambdaRanges.head.xhi)
      if (hi < dsRange.xlo) None else Some(MPFRInterval(dsRange.xlo, hi))
    case GreaterEquals(RealLiteral(r), x) if x == ds =>
      val hi = MPFRFloat.min(dsRange.xhi, MPFRFloat.fromRational(r)) //lambdaRanges.head.xhi)
      if (hi < dsRange.xlo) None else Some(MPFRInterval(dsRange.xlo, hi))
    case LessThan(x, RealLiteral(r)) if x == ds =>
      val hi = MPFRFloat.min(dsRange.xhi, MPFRFloat.fromRational(r)) //lambdaRanges.head.xhi)
      if (hi < dsRange.xlo) None else Some(MPFRInterval(dsRange.xlo, hi))
    case LessEquals(x, RealLiteral(r)) if x == ds =>
      val hi = MPFRFloat.min(dsRange.xhi, MPFRFloat.fromRational(r)) //lambdaRanges.head.xhi)
      if (hi < dsRange.xlo) None else Some(MPFRInterval(dsRange.xlo, hi))

    case And(ls) =>
      val res = ls.map(updateRange(ds, dsRange, _))
      if (res.contains(None))
        None
      else {
        // check if the intervals are overlapping, if not -> incompatible conditions
        val intervals = res.map({case Some(x) => x})
        MPFRInterval.intersect(intervals)
      }
    // todo also improve if comparing two variables
    case _ => Some(dsRange)
  }

  /**
   * Transforms the map between expressions and their DSAbstraction into an intermediate ranges map.
   * Preprocessing function: expressions, for which there is no range computed,
   * will be evaluated using [[daisy.tools.RangeEvaluators]] (at the next step of eval function)
   * @param indices Indices for which an expression will be evaluated (for each expression). If specified index is not in DSAbstraction yet,
   *                the value at this index has to be evaluated
   * @param rangesAbstr a mapping between expressions and their abstractions
   * @param path a (conditional) path for which an expression will be evaluated
   * @return intermediate ranges (necessary for range computation in [[daisy.tools.RangeEvaluators]])
   */
  private def toIntermediateRanges(indices: Map[Expr, Set[Index]],
                                   rangesAbstr: Map[Expr, DSAbstraction],
                                   path: PathCond = emptyPath): CachingMap[(Expr, PathCond), MPFRInterval] = {
    val mapDS = indices.filter({
      case (ex, list) => rangesAbstr.contains(ex) && list.diff(rangesAbstr(ex).indices).isEmpty
    }) // there is an abstraction for each index requested
    val mapDSAtIndices = mapDS.to(collection.mutable.HashMap).map({ case (ex, inds) => (ex, path) -> rangesAbstr(ex).atIndicesJoint(inds)})
    // RealType variables, that are not yet included into the mapDS and for which there is no range for the specified index
    val reals = rangesAbstr.filterNot({case (k,v) =>
      hasDsType(k) || mapDS.contains(k) || (indices.contains(k) && indices(k).intersect(v.indices).isEmpty)
    })
    val mapReals = reals.map({case (ex, abstraction) => (ex, path) -> abstraction.fullInterval})
    CachingMap.fromMap(mapDSAtIndices ++ mapReals)
  }

  /**
   * Transforms the map between expressions and their DSAbstraction into an intermediate rounding error map.
   * Preprocessing function: expressions, for which there is no rounding error computed,
   * will be evaluated using [[daisy.tools.RoundoffEvaluators]] (at the next step of eval function)
   * @param indices Indices for which an expression will be evaluated (for each expression).
   *                If specified index is not in DSAbstraction yet, the value at this index has to be evaluated
   * @param errorsAbstr a mapping between expressions and their abstractions
   * @param precisionMap precision assignment for variables
   * @param uniformPrec precision to be used for constants
   * @param path a (conditional) path for which an expression will be evaluated
   * @return intermediate rounding errors (necessary for error computation in [[daisy.tools.RoundoffEvaluators]])
   */
  private def toIntermediateErrors(indices: Map[Expr, Set[Index]],
                                   errorsAbstr: Map[Expr, DSAbstraction],
                                   precisionMap: Map[Identifier, Precision],
                                   uniformPrec: Precision,
                                   path: PathCond = emptyPath): CachingMap[(Expr, PathCond), (MPFRInterval, Precision)] = {
    // values for data structures or reals with specified indices (e.g. if it appears inside filter lambda function)
    val mapDS = indices.filter({
      case (ex, list) =>
        errorsAbstr.contains(ex) && list.diff(errorsAbstr(ex).indices).isEmpty
    })
    val mapDSAtIndices = mapDS.to(collection.mutable.HashMap).map({ case (ex, inds) =>
      val prec = ex match {
        case Variable(id) if precisionMap.contains(id) => precisionMap(id)
        case _ => uniformPrec
      }
      (ex, path) -> (errorsAbstr(ex).atIndicesJoint(inds), prec)
    })
    // values for real variables and expressions, except those that have to be evaluated (i.e., no error at the specified indices yet)
    val reals = errorsAbstr.filterNot({case (k,v) =>
      hasDsType(k) || mapDS.contains(k) || (indices.contains(k) && indices(k).intersect(v.indices).isEmpty)
    })
    val mapReals = reals.map({ case (ex, abstraction) =>
        val prec = ex match {
          case Variable(id) if precisionMap.contains(id) => precisionMap(id)
          case _ => uniformPrec
        }
        (ex, path) -> (abstraction.fullInterval, prec)
      })
    val toCache = (mapDSAtIndices ++ mapReals).map({
        case (k, v) => v match {
          case (i:MPFRInterval, prec) => k -> (MPFRInterval(i), prec)
        }})
    CachingMap.fromMap(toCache)
  }

  private def updateDSWithRanges(e: Expr, indices: Map[Expr, Set[Index]],
                         rangesAbstr: Map[Expr, DSAbstraction],
                         intermedRes: Map[(Expr, PathCond), MPFRInterval],
                         path: PathCond = emptyPath): Map[Expr, DSAbstraction] = {
    // todo also consider sub-expressions of e
    val oldDS = rangesAbstr.getOrElse(e, DSAbstraction.empty())
    val range = intermedRes(e, path)
    val index = indices.getOrElse(e, Set())
    val updatedDS = oldDS.addSpec(index, range)
    rangesAbstr + (e -> updatedDS)
  }

  def genSortedMatrixIndices(fromRow:Int, toRow: Int, fromCol: Int, toCol: Int): Seq[Index] = {
    val indices = for (ir <- (fromRow until toRow); ic <-  (fromCol until toCol)) yield { MatrixIndex(ir, ic) }
    indices.sorted
  }

  def genSortedVectorIndices(from: Int, to: Int): Seq[Index] =
    (from until to).map(i=> VectorIndex(i).asInstanceOf[Index]).sorted

}
