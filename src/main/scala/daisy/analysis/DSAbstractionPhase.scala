package daisy.analysis

import daisy._
import daisy.lang.Extractors.ArithOperator
import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.TreeOps
import daisy.lang.TreeOps._
import daisy.lang.Trees._
import daisy.lang.Types.{Int32Type, MatrixType, RealType, VectorType}
import daisy.tools.FinitePrecision._
import daisy.tools._
import daisy.utils.CachingMap

import scala.collection.mutable.ListBuffer

/**
  * This phase computes the abstraction of data structures
  * *
  * *
  * Prerequisites:
  *- SpecsProcessingPhase
  */
object DSAbstractionPhase extends DaisyPhase with tools.RoundoffEvaluators with tools.RangeEvaluators {
  override val name = "DS Abstraction"
  override val description = "Computes abstraction of data structures"
  type IntermedResults = Map[(Expr, PathCond), Interval]
  var unfoldLimit: Int = 1
  var unfoldLimitHighOcc: Int = 1
  val occurrenceLimit: Int = 1 // how often can a variable occur in the lambda body to be efficiently inlined

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption(
      "unfoldLimit",
      1, //10,
      "Number of iterations to be inlined in an unrolled fold-loop"),
    NumOption(
      "unfoldLimHO",
      1, //2,
      "Number of iterations to be inlined in an unrolled fold-loop if the accumulator occurs often"),
    FlagOption("noFoldOpt",
      "Disable optiimizations for fold-, enum-loops, unroll instead.")
  )

  override implicit val debugSection: DebugSection = DebugSectionAnalysis
  var rangeMethod = ""
  var errorMethod = ""
  var trackRoundoffErrs = true

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    rangeMethod = ctx.option[String]("rangeMethod")
    errorMethod = ctx.option[String]("errorMethod")
    trackRoundoffErrs = !ctx.hasFlag("noRoundoff")
    val optimizedFolds = !ctx.hasFlag("noFoldOpt")
    val uniformPrecision = ctx.option[Precision]("precision")
    unfoldLimit = ctx.option[Long]("unfoldLimit").toInt
    unfoldLimitHighOcc = ctx.option[Long]("unfoldLimHO").toInt

    val res = functionsToConsider(ctx, prg).map(fnc => {
      ctx.timers.get("DSPhase-"+ fnc.id.toString).start()

      assert(ctx.dsAbstractions.contains(fnc.id), s"No abstraction for data structures in the function $fnc") // todo allow functions with Real inputs only
      val inputErrorMap: Map[Identifier, Rational] = ctx.specInputErrors(fnc.id)

      val precisionMap: Map[Identifier, Precision] = ctx.specInputPrecisions(fnc.id) ++ allIDsOf(fnc.body.get).diff(ctx.specInputPrecisions(fnc.id).keySet).map(id => id -> uniformPrecision).toMap // add variables from let statements
      val (err, range) = computeResults(fnc.body.get,
        ctx.dsAbstractions(fnc.id),
        ctx.specInputRanges(fnc.id),
        inputErrorMap,
        precisionMap,
        uniformPrecision,
        fnc.id,
        optimizedFolds
      ) // todo also call with precond (for SMTRange)
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
   * @param dsas DS AbstractionS taken from the Context
   * @param initValMap input ranges for variables
   * @param inputErrorMap input errors for variables
   * @param precisionMap precision assignment on all IDs in expr
   * @param constPrecision a uniform precision to be used for constants
   * @param currentFncId ID of the currently evaluated body
   * @param optimizedFolds if set to True use optimized evaluation of fold-,enum-loops, otherwise unroll
   * @return
   */
  def computeResults(expr: Expr,
                     dsas: Map[Expr, DSAbstraction],
                     initValMap: Map[Identifier, Interval],
                     inputErrorMap: Map[Identifier, Rational],
                     precisionMap: Map[Identifier, Precision],
                     constPrecision: Precision,
                     currentFncId: Identifier,
                     optimizedFolds: Boolean
                    ): (Rational, Interval) = {
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
    @scala.annotation.nowarn
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

        // v is a variable, must already be in the DSA
      case x@VectorElement(v@VectorLiteral(_), index) =>
        assert(rangesAbstr.contains(v) && errorsAbstr.contains(v), s"Trying to take an element of an unknown vector $v")
        val (indexRanges, indexErrs) = eval(index, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val indexDS = indexRanges(index)
        assert(indexDS.hasSingleRange, s"Vector element index should have a single range, $index has ${indexDS.ranges}")
        val indRange = MPFRInterval.integersIn(indexDS.fullInterval).map(x => VectorIndex(x).asInstanceOf[Index])
        val updIndices = indices + (v -> indRange)
        val xDSRanges = rangesAbstr(v).getAbstractionOnlyAtIndices(indRange)
        val xDSErrors = errorsAbstr(v).getAbstractionOnlyAtIndices(indRange)
        (rangesAbstr + (x -> xDSRanges), errorsAbstr + (x -> xDSErrors))

        // v is an expression
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
        val newIndValues = (0 until padSize).toList ++ ((origVRanges.dsSize + padSize) until (origVRanges.dsSize + 2*padSize)).toSet
        val newInds = newIndValues.map(x => VectorIndex(x).asInstanceOf[Index]).toSet

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
        val xRangeDS = DSAbstraction(newRangesM.flatten.toMap).regroup()
        val xErrorDS = DSAbstraction(newErrsM.flatten.toMap).regroup()
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
        val xRangeDS = origDSRange.addSpec(Set(newIndex), eltRanges(el).fullInterval).regroup()
        val xErrorDS = origDSErr.addSpec(Set(newIndex), eltErrors(el).fullInterval).regroup()
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
        val xRangeDS = origDSRange.updateWith(newRanges).regroup()
        val xErrorDS = origDSErr.updateWith(newErrs).regroup()
        (eltRanges + (x->xRangeDS), eltErrors + (x->xErrorDS))

      case x@PrependElement(v, el) if isVector(v) =>
        val (vRanges, vErrors) = eval(v, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val origDSRange = vRanges(v)
        val origDSErr = vErrors(v)

        val (eltRanges, eltErrors) = eval(el, vRanges, vErrors, indices, unrollingScope = unrollingScope)
        val newRange = Set(VectorIndex(0).asInstanceOf[Index]) -> eltRanges(el).fullInterval
        val updIndRanges = origDSRange.indexGroups.map(ig => {
          val newInds = ig.map({case VectorIndex(intI) => VectorIndex(intI + 1).asInstanceOf[Index]})
          newInds -> origDSRange.indexToRange(ig)
        }).toMap
        val xRangeDS = DSAbstraction(updIndRanges + newRange).regroup()

        val newErr = Set(VectorIndex(0).asInstanceOf[Index]) -> eltErrors(el).fullInterval
        val updIndErrs = origDSErr.indexGroups.map(ig => {
          val newInds = ig.map({case VectorIndex(intI) => VectorIndex(intI + 1).asInstanceOf[Index]})
          newInds -> origDSErr.indexToRange(ig)
        }).toMap
        val xErrorDS = DSAbstraction(updIndErrs + newErr).regroup()
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
        val updIndRanges = origDSRange.indexGroups.map(ig => {
          val newInds = ig.map({case MatrixIndex(intI, j) => MatrixIndex(intI + 1, j).asInstanceOf[Index]})
          newInds -> origDSRange.indexToRange(ig)
        }).toMap
        val xRangeDS = DSAbstraction(updIndRanges ++ newRanges).regroup()

        val newErrs = (0 until origDSRange.numCols).map(i => {
          val eltError = eltErrors(el).at(i)
          Set(MatrixIndex(0, i).asInstanceOf[Index]) -> eltError
        }).toMap
        val updIndErrs = origDSErr.indexGroups.map(ig => {
          val newInds = ig.map({case MatrixIndex(intI, j) => MatrixIndex(intI + 1, j).asInstanceOf[Index]})
          newInds -> origDSErr.indexToRange(ig)
        }).toMap
        val xErrorDS = DSAbstraction(updIndErrs ++ newErrs).regroup()
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
        val xDSRange = rhsRanges(lhs).updateWith(rhsNewRanges).regroup()
        val xDSErrs = rhsErrors(lhs).updateWith(rhsNewErrs).regroup()
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
        // special case for zeroVector
        if (list.head == RealLiteral(Rational.zero) && list.size == 1) {
          val inds = (0 until length).map(x => VectorIndex(x).asInstanceOf[Index]).toSet
          val rng = DSAbstraction(Map(inds -> MPFRInterval.zero))
          (rangesAbstr + (x -> rng), errorsAbstr + (x -> rng))
        } else {
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
        }

      case x@MatrixFromLists(listOflists, numRows, numCols) =>
        // todo mk a special case for zeroMatrix
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
        val xDSRange = DSAbstraction(selected._1.toMap).regroup()
        val xDSErr = DSAbstraction(selected._2.toMap).regroup()
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
        val xDSRange = DSAbstraction(selected._1.flatten.toMap).regroup()
        val xDSErr = DSAbstraction(selected._2.flatten.toMap).regroup()
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
            val (updRanges, updErrors) = dsRanges(ds).indexGroups.foldLeft((dsRanges,dsErrors))({
              case (acc, ind) =>
                eval(cond, acc._1 + (lambda -> dsRanges(ds)), acc._2 + (lambda -> dsErrors(ds)), indices ++ Map(lambda -> ind, cond -> ind, ds -> ind), unrollingScope = unrollingScope)
            })
            val condIndices = updRanges(cond).indexGroups.filter(ind =>
              conditionHolds(cond, Seq(updRanges(cond).atIndicesJoint(ind)))
            ).flatten

            (condIndices, updRanges, updErrors)
        }

        val (satIndices, condRanges, condErrs) = evalCondition(normalizedLambda)
        val affectedIndices = condRanges(ds).indexGroupsFor(satIndices)
        val improvedRanges = affectedIndices.map(ind => {
          val range = updateRange(ds, condRanges(ds).atIndicesJoint(ind), replace{ case va@Variable(_) if va == lambda => ds }(body))
          if (range.isEmpty)
            None
          else Some(ind -> range.get)
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

        val (bodyRanges, bodyErrs) = dsRanges(ds).indexGroups.map({
           ind =>
             val (rng, err) = eval(body, dsRanges + (lambda -> dsRanges(ds)), dsErrors + (lambda -> dsErrors(ds)), indices ++ Map(lambda -> ind, body -> ind), unrollingScope = unrollingScope)
             (ind -> rng(body).fullInterval, ind -> err(body).fullInterval)
        }).unzip

        val xDSRange = DSAbstraction(bodyRanges.toMap)
        val xDSErr = DSAbstraction(bodyErrs.toMap)

        (dsRanges + (x -> xDSRange), dsErrors + (x -> xDSErr))


      // sum: special fold on vectors
      case x@Sum(ds, init) if optimizedFolds && !forceEval && isVector(ds) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (initEvalRange, initEvalErrs) = eval(init, dsRange, dsErrs, indices, unrollingScope = unrollingScope)
        val rangeGroups = initEvalRange(ds).countToRangeWithRepresentative()
        var iterValues = new ListBuffer[MPFRInterval]()
        var iterErrs = new ListBuffer[MPFRInterval]()
        iterValues += initEvalRange(init).fullInterval
        iterErrs += initEvalErrs(init).fullInterval
        val sum = rangeGroups.foldLeft((initEvalRange(init).fullInterval, initEvalErrs(init).fullInterval))({
          case (acc, ((count, ind), range)) =>
            val initErr = acc._2
            val xErr = initEvalErrs(ds).at(ind) // todo is this always true that error on x is the same if the range is the same?
            val xPrecision = constPrecision // todo are there cases when precision is different?
            // compute the new range
            val accRange = acc._1 + range * (count-1)
            val iterRange = (acc._1 + range * count)// .union(acc._1) // subsume previous iterations
            iterValues += iterRange
            val xRange = range.toInterval

            val accId = FreshIdentifier(s"accAt${ind.i}", RealType)
            val accVar = Variable(accId)
            val xId = FreshIdentifier(s"$ds${ind.i}", RealType)
            val xVar = Variable(xId)
            val bodyExpr = Plus(xVar, accVar)

            // compute new error on each iteration, but use the same range for acc=iterRange
            val rangeMap = toIntermediateRangesWIntervals(indices, initEvalRange) ++
              Map((accVar, emptyPath) -> accRange.toInterval, (xVar,emptyPath) -> xRange, (bodyExpr, emptyPath) -> iterRange.toInterval) // upd rangeAbstr
            val errMap = toIntermediateErrors(indices, initEvalErrs, precisionMap, constPrecision)
            errMap.update((xVar, emptyPath), (xErr, xPrecision)) // add error for VectorElement (x)

            val newError = (ind.i until (ind.i + count)).foldLeft(initErr)({
              case (propagatedErr, _) =>
                errMap.update((accVar, emptyPath), (propagatedErr, xPrecision)) // update the error for acc to correspond to the iter
                errMap.remove((bodyExpr, emptyPath)) // make sure it is recomputed every iteration
                val (_, intermedErrs) = evalRoundoff[MPFRInterval](
                  bodyExpr, rangeMap,
                  precisionMap,
                  Map(),
                  zeroError = MPFRInterval.zero,
                  fromError = MPFRInterval.+/-,
                  interval2T = MPFRInterval.apply,
                  constantsPrecision = constPrecision,
                  trackRoundoffErrs,
                  precomputedIntermedErrs = errMap
              )
                intermedErrs(bodyExpr, emptyPath)
            })

            iterErrs+= newError
            //println(s"OPTIMIZED SUM\n$count elements with range $range; representative index $ind;\n eval to $iterRange;\n propagated err becomes $newError")
            (iterRange, newError)
        })
        val endRange = MPFRInterval.union(iterValues.toSet)
        val endErr = MPFRInterval.union(iterErrs.toSet)
        (initEvalRange + (x -> DSAbstraction(endRange)), initEvalErrs + (x -> DSAbstraction(endErr)))


      // non-opimized sum on vectors
      case x@Sum(ds, init) if !forceEval && isVector(ds) =>
        val (initRange, initErrs) = eval(init, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (dsRange, dsErrs) = eval(ds, initRange, initErrs, indices, unrollingScope = unrollingScope)
        val unrollChunks = (0 until dsRange(ds).dsSize).grouped(unfoldLimit)
        val accId = FreshIdentifier("acc", RealType)
        val xId = FreshIdentifier("x", RealType)
        val args = Seq(ValDef(accId), ValDef(xId))
        val body = Plus(Variable(accId), Variable(xId))
        val chunkedUnrollRes = unrollChunks.foldLeft((dsRange, dsErrs, init))({
          case (acc, inds) =>
            val unrolledFold = TreeOps.unrollFoldOnVector(ds, fromInd = inds.head, toInd = inds.last, acc._3, args, body)
            val (foldRanges, foldErrs) = eval(unrolledFold, acc._1, acc._2, indices, unrollingScope = true)

            // union with previous iterations (all previous iterations are subsumed by the follow-up)
            val foldRangeSubsumes = DSAbstraction(foldRanges(unrolledFold).fullInterval.union(acc._1(init).fullInterval))
            val foldErrSubsumes = DSAbstraction(foldErrs(unrolledFold).fullInterval.union(acc._2(init).fullInterval))
            val newId = FreshIdentifier(s"acc${inds.head}to${inds.last}", RealType)
            // the new iterator gets the latest unrolled value, not the one that subsumes the previous iterations
            val additionalRanges = Map(unrolledFold -> foldRangeSubsumes, Variable(newId) -> foldRanges(unrolledFold))
            val additionalErrs = Map(unrolledFold -> foldErrSubsumes, Variable(newId) -> foldErrs(unrolledFold))
            // passing only the end result to the next iter - otherwise if the body contains let statements, all exprs that use would take old cached values
            (dsRange ++ additionalRanges, dsErrs ++ additionalErrs, Variable(newId))
        })
        val (newRng, newErrs, unrolledEndExpr) = chunkedUnrollRes
        (newRng + (x -> newRng(unrolledEndExpr)), newErrs + (x -> newErrs(unrolledEndExpr)))

      // linear bodies: special fold on vectors
      case x@FoldIter(ds, init, Lambda(args, body)) if optimizedFolds && !forceEval && isVector(ds) && isLinear(body) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (initEvalRange, initEvalErrs) = eval(init, dsRange, dsErrs, indices, unrollingScope = unrollingScope)
        // determine which coefficient is which (detect by args id)
        val coefs = getLinearCoefficients(e=body, xId=args(1).id, accId=args.head.id, coefs=Map(), Sign.Positive)
        val coefRanges = coefs.map(cc => {
          val rng = initEvalRange.getOrElse(cc._2, {
            cc._2 match {
              case RealLiteral(r) => DSAbstraction(r)
              case UMinus(v) => DSAbstraction(initEvalRange(v).fullInterval*MPFRFloat.fromDouble(-1))
              case _ => throw new DaisyFatalError(Some("Coefficient was not available. Would need to reevaluate the expression. (Giving up for now)."))
            }
          }).fullInterval
          cc._1 -> rng
        })
        val a = coefRanges.getOrElse("a", MPFRInterval.zero).union(-coefRanges.getOrElse("a", MPFRInterval.zero))
        val b = coefRanges.getOrElse("b", MPFRInterval.zero).union(-coefRanges.getOrElse("b", MPFRInterval.zero))
        val c = coefRanges.getOrElse("c", MPFRInterval.zero).union(-coefRanges.getOrElse("c", MPFRInterval.zero))
        //println(s"a=$a, b=$b, c=$c")
        // sort the groups, since order makes a difference here
        val rangeGroups = initEvalRange(ds).conseqGroupsWRepresentative()
        var iterValues = new ListBuffer[MPFRInterval]()
        var iterErrs = new ListBuffer[MPFRInterval]()
        iterValues += initEvalRange(init).fullInterval
        iterErrs += initEvalErrs(init).fullInterval
        val sum = rangeGroups.foldLeft((initEvalRange(init).fullInterval, initEvalErrs(init).fullInterval))({
          case (acc, ((count, ind), xRange)) =>
            // copy errors into vars with meaningful names
            val initErr = acc._2
            val xErr = initEvalErrs(ds).at(ind) // todo is this always true that error on x is the same if the range is the same?
            val xPrecision = constPrecision // todo are there cases when precision is different?
            // compute the new range
            val sumOfBs = sumOfPows(b, count)
            val accRange =  a * xRange * sumOfBs + b.^(count-1) * acc._1 + c * sumOfBs
            val iterRange = (a * xRange * sumOfBs + b.^(count) * acc._1 + c * sumOfBs) //  a*x*sumOfPows(b,n) + b.^(n)*init + sumOfPows(b,n)*c
            iterValues += iterRange
            // create an expression to analyze
            val accId = FreshIdentifier(s"accAt${ind.i}", RealType)
            val accVar = Variable(accId)
            val xId = FreshIdentifier(s"$ds${ind.i}", RealType)
            val xVar = Variable(xId)
            val bodyExpr = replace({
              case Variable(id) if id == args.head.id => accVar
              case Variable(id) if id == args(1).id => xVar
            })(body)
            // compute intermediate ranges for all sub-expressions of bodyExpr
            val iterRangeMap = toIntermediateRanges(indices, initEvalRange ++ Map(xVar -> DSAbstraction(xRange), accVar -> DSAbstraction(accRange)))
            val (_, intermedRanges) = evalRange[MPFRInterval](bodyExpr, Map(), MPFRInterval.apply, precompIntermRanges = iterRangeMap)
            val rangeMap = intermedRanges.map({case (k,v) => k-> Interval.fromMPFR(v)})

            // compute new error on each iteration, but use the same range for acc=iterRange
            val errMap = toIntermediateErrors(indices, initEvalErrs, precisionMap, constPrecision)
            errMap.update((xVar, emptyPath), (xErr, xPrecision)) // add error for VectorElement (x)

            val newError = (ind.i until (ind.i + count)).foldLeft(initErr)({
              case (propagatedErr, _) =>
                errMap.update((accVar, emptyPath), (propagatedErr, xPrecision)) // update the error for acc to correspond to the iter
                errMap.remove((bodyExpr, emptyPath)) // make sure it is recomputed every iteration
                val (_, intermedErrs) = evalRoundoff[MPFRInterval]( //AffineForm](
                  bodyExpr, rangeMap,
                  precisionMap,
                  Map(),
                  zeroError = MPFRInterval.zero, // AffineForm.zero, //
                  fromError = MPFRInterval.+/-, //AffineForm.+/-, //
                  interval2T = MPFRInterval.apply, // AffineForm.apply, //
                  constantsPrecision = constPrecision,
                  trackRoundoffErrs,
                  precomputedIntermedErrs = errMap
                )
                intermedErrs(bodyExpr, emptyPath)
            })
            iterErrs += newError

            //println(s"OPTIMIZED SUM\n$count elements with range $range; representative index $ind;\n eval to $iterRange;\n propagated err becomes $newError")
            (iterRange, newError)
        })
        // union with previous iterations' results
        val endRange = MPFRInterval.union(iterValues.toSet)
        val endErr = MPFRInterval.union(iterErrs.toSet)
        (initEvalRange + (x -> DSAbstraction(endRange)), initEvalErrs + (x -> DSAbstraction(endErr)))

      // multiplication: special fold on vectors
      case x@FoldIter(ds, init, Lambda(args, Times(Variable(_), Variable(_)))) if optimizedFolds && !forceEval && isVector(ds) =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (initEvalRange, initEvalErrs) = eval(init, dsRange, dsErrs, indices, unrollingScope = unrollingScope)
        val rangeGroups = initEvalRange(ds).countToRangeWithRepresentative()
        var iterValues = new ListBuffer[MPFRInterval]()
        var iterErrs = new ListBuffer[MPFRInterval]()
        iterValues += initEvalRange(init).fullInterval
        iterErrs += initEvalErrs(init).fullInterval
        val sum = rangeGroups.foldLeft((initEvalRange(init).fullInterval, initEvalErrs(init).fullInterval))({
          case (acc, ((count, ind), range)) =>
            val lookupErr = initEvalErrs(ds).at(ind)
            // compute the new range and error
            val newRange = range^count
            val timesRange = (acc._1 * newRange)
            // error term over-approximation
            // x^n*e_init + (init + e_init)*(2^n * (x^{n-1}*e_x + x*e_x^{n-1}))
            val errTerm = newRange * acc._2 + (MPFRInterval(2)^count)*(acc._1 + acc._2)*((range^(count-1))*lookupErr + range*(lookupErr^(count-1)))
            //println(s"OPTIMIZED Times\n$count elements with range $range; representative index $ind;\n eval to $newRange;\n propagated err ${acc._2} becomes $errTerm; sum=$timesRange")
            iterValues += timesRange
            iterErrs += errTerm
            (timesRange, errTerm)
        })
        // union with previous iterations' results
        val endRange = MPFRInterval.union(iterValues.toSet)
        val endErr = MPFRInterval.union(iterErrs.toSet)
        (initEvalRange + (x -> DSAbstraction(endRange)), initEvalErrs + (x -> DSAbstraction(endErr)))


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

            // union with previous iterations (all previous iterations are subsumed by the follow-up)
            val foldRangeSubsumes = DSAbstraction(foldRanges(unrolledFold).fullInterval.union(acc._1(init).fullInterval))
            val foldErrSubsumes = DSAbstraction(foldErrs(unrolledFold).fullInterval.union(acc._2(init).fullInterval))

            val newId = FreshIdentifier(s"acc${inds.head}to${inds.last}", RealType)
            val additionalRanges = Map(unrolledFold -> foldRangeSubsumes, Variable(newId) -> foldRanges(unrolledFold))
            val additionalErrs = Map(unrolledFold -> foldErrSubsumes, Variable(newId) -> foldErrs(unrolledFold))
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

          // union with previous iterations (all previous iterations are subsumed by the follow-up)
          val foldRangeSubsumes = DSAbstraction(foldRanges(unrolledFold).fullInterval.union(acc._1(init).fullInterval))
          val foldErrSubsumes = DSAbstraction(foldErrs(unrolledFold).fullInterval.union(acc._2(init).fullInterval))

          val newId = FreshIdentifier(s"acc${rowInds.head}to${rowInds.last}", VectorType(Seq(RealType)))
          val additionalRanges = Map(unrolledFold -> foldRangeSubsumes, VectorLiteral(newId) -> foldRanges(unrolledFold))
          val additionalErrs = Map(unrolledFold -> foldErrSubsumes, VectorLiteral(newId) -> foldErrs(unrolledFold))

          // passing only the end result to the next iter - otherwise if the body contains let statements, all exprs that use would take old cached values
          (initRange ++ additionalRanges, initErrs ++ additionalErrs, VectorLiteral(newId), acc._4 + (VectorLiteral(newId) -> foldRanges(unrolledFold).indices, unrolledFold -> foldRanges(unrolledFold).indices))
        })
        val unrolledEndExpr = chunkedUnrollRes._3
        (chunkedUnrollRes._1 + (x -> chunkedUnrollRes._1(unrolledEndExpr)), chunkedUnrollRes._2 + (x -> chunkedUnrollRes._2(unrolledEndExpr)))

      // sum: special fold on matrices
      case x@FoldElemsIter(ds, init, Lambda(args, bodyExpr@Plus(Variable(_), Variable(_)))) if optimizedFolds && !forceEval =>
        val (dsRange, dsErrs) = eval(ds, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (initEvalRange, initEvalErrs) = eval(init, dsRange, dsErrs, indices, unrollingScope = unrollingScope)
        val rangeGroups = initEvalRange(ds).countToRangeWithRepresentative()
        var iterValues = new ListBuffer[MPFRInterval]()
        var iterErrs = new ListBuffer[MPFRInterval]()
        iterValues += initEvalRange(init).fullInterval
        iterErrs += initEvalErrs(init).fullInterval
        val sum = rangeGroups.foldLeft((initEvalRange(init).fullInterval, initEvalErrs(init).fullInterval))({
          case (acc, ((count, ind), range)) =>
            val initErr = acc._2
            val xErr = initEvalErrs(ds).at(ind) // todo is this always true that error on x is the same if the range is the same?
            val xPrecision = constPrecision // todo are there cases when precision is different?
            // compute the new range
            val accRange = acc._1 + range * (count-1)
            val iterRange = (acc._1 + range * count) //.union(acc._1) // union with previous iterations' result
            iterValues += iterRange
            val xRange = range.toInterval

            //val accId = FreshIdentifier(s"accAt${ind.i}", RealType)
            val accVar = Variable(args.head.id)
            //val xId = FreshIdentifier(s"$ds${ind.i}", RealType)
            val xVar = Variable(args(1).id)
            //val bodyExpr = Plus(xVar, accVar)

            // compute new error on each iteration, but use the same range for acc=iterRange
            val rangeMap = toIntermediateRangesWIntervals(indices, initEvalRange) ++
              Map((accVar, emptyPath) -> accRange.toInterval, (xVar,emptyPath) -> xRange, (bodyExpr, emptyPath) -> iterRange.toInterval) // upd rangeAbstr
            val errMap = toIntermediateErrors(indices, initEvalErrs, precisionMap, constPrecision)
            errMap.update((xVar, emptyPath), (xErr, xPrecision)) // add error for MatrixElement (x)

            val newError = (ind.i until (ind.i + count)).foldLeft(initErr)({
              case (propagatedErr, _) =>
                errMap.update((accVar, emptyPath), (propagatedErr, xPrecision)) // update the error for acc to correspond to the iter
                errMap.remove((bodyExpr, emptyPath)) // make sure it is recomputed every iteration
                val (_, intermedErrs) = evalRoundoff[MPFRInterval](
                  bodyExpr, rangeMap,
                  precisionMap,
                  Map(),
                  zeroError = MPFRInterval.zero,
                  fromError = MPFRInterval.+/-,
                  interval2T = MPFRInterval.apply,
                  constantsPrecision = constPrecision,
                  trackRoundoffErrs,
                  precomputedIntermedErrs = errMap
                )
                intermedErrs(bodyExpr, emptyPath)
            })
            iterErrs += newError
            //println(s"OPTIMIZED SUM\n$count elements with range $range; representative index $ind;\n eval to $iterRange;\n propagated err becomes $newError")
            (iterRange, newError)
        })
        val endRange = MPFRInterval.union(iterValues.toSet)
        val endErr = MPFRInterval.union(iterErrs.toSet)
        (initEvalRange + (x -> DSAbstraction(endRange)), initEvalErrs + (x -> DSAbstraction(endErr)))

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

            // union with previous iterations (all previous iterations are subsumed by the follow-up)
            val foldRangeSubsumes = DSAbstraction(foldRanges(unrolledFold).fullInterval.union(acc._1(init).fullInterval))
            val foldErrSubsumes = DSAbstraction(foldErrs(unrolledFold).fullInterval.union(acc._2(init).fullInterval))

            val newId = FreshIdentifier(s"acc${fromInd.i}c${fromInd.j}to${toInd.i}c${toInd.j}", RealType)
            val additionalRanges = Map(unrolledFold -> foldRangeSubsumes, Variable(newId) -> foldRanges(unrolledFold))
            val additionalErrs = Map(unrolledFold -> foldErrSubsumes, Variable(newId) -> foldErrs(unrolledFold))

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
        //// todo do we need intermediate values? e.g. after sliding the window once/twice/...?
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

            val updRangesDS = dsRange + (iterM -> DSAbstraction(mappedRanges).regroup())
            val updErrsDS = dsErrs + (iterM -> DSAbstraction(mappedErrs).regroup())
            val (iterRanges, iterErrs) = eval(body, updRangesDS, updErrsDS, indices + (iterM -> iterDSIndices.toSet), unrollingScope = unrollingScope)

            val newEltRangeMap = Set(index.asInstanceOf[Index]) -> iterRanges(body).fullInterval // adjust if the result is a DS (not a Real type)
            val newEltErrMap = Set(index.asInstanceOf[Index]) -> iterErrs(body).fullInterval // adjust if the result is a DS (not a Real type)
            (newEltRangeMap, newEltErrMap)
        }).unzip
        val xRangeDS = DSAbstraction(xRangeMap.toMap).regroup()
        val xErrDS = DSAbstraction(xErrMap.toMap).regroup()
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
              case vi@VectorIndex(j) => Set(MatrixIndex(i,j).asInstanceOf[Index]) -> iterRanges(body).at(vi) // resulting vector has the same size as original row
            })
            val newEltErrMap = vectorIndices.map({
              case vi@VectorIndex(j) => Set(MatrixIndex(i,j).asInstanceOf[Index]) -> iterErrs(body).at(vi)
            })
            (newEltRangeMap, newEltErrMap)
        }).unzip
        val xRangeDS = DSAbstraction(xRangeMap.flatten.toMap).regroup()
        val xErrDS = DSAbstraction(xErrMap.flatten.toMap).regroup()
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
            val origDSIndices = (i*size until (i+1)*size).map(VectorIndex)
            val sliceIndices = (0 until size).map(VectorIndex)
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
              case vi@VectorIndex(j) => Set(VectorIndex(j+i*size).asInstanceOf[Index]) -> iterRanges(body).at(vi) // resulting vector has the same size as original slice
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
        //println(id + " " + rangesAbstr(dsas.head._1).dsSize) // debugging heat1d
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
          Set(VectorIndex(1).asInstanceOf[Index]) -> sndRanges(secondElt).fullInterval)).regroup()
        val xDSErr = DSAbstraction(Map(Set(VectorIndex(0).asInstanceOf[Index]) -> fstErrs(firstElt).fullInterval,
          Set(VectorIndex(1).asInstanceOf[Index]) -> sndErrs(secondElt).fullInterval)).regroup()
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErr))

      case x@CrossProduct(lhs, rhs) if isMatrix(lhs) && isVector(rhs) =>
        /// i-th row x vector
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        assert(rhsRanges(lhs).numCols == rhsRanges(rhs).dsSize,
          s"Wrong dimensions for multiplication of matrix with vector $lhs:${rhsRanges(lhs).dsSize} and $rhs:${rhsRanges(rhs).dsSize}")
        //val allValidRows = (0 until rhsRanges(lhs).numRows).map(i => genMatrixIndices(i,i+1, 0, rhsRanges(lhs).numCols))
        //val (preGroupRows, singleElts) = rhsRanges(lhs).indexGroups.partition({ gr => allValidRows.exists(f=> f.diff(gr).isEmpty)})
        val cols = rhsRanges(lhs).numCols
        val (preGroupRows, _) = rhsRanges(lhs).indexGroups.partition({ gr =>
          (0 until rhsRanges(lhs).numRows).exists(f=> gr.count(g => g.i == f) == cols)})
        val sameRangeRows = preGroupRows.map(gr => {
          val rowInds = getAllRowIndices(gr)
          rowInds.filter(same => gr.count(g => g.i == same) == cols) // only take the rows for which all elements are in the group
        })
        // compute the range once per group with the same range
        val rowsOptimal: (Set[(Set[Index], MPFRInterval)], Set[(Set[Index], MPFRInterval)]) = sameRangeRows.map(rowIndices => {
          // vector part
          val origDSIndices = rhsRanges(lhs).indicesAtRow(rowIndices.head) // take a representative
          val vectorDS = rhsRanges(lhs).getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()
          val vectorErr = rhsErrors(lhs).getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()

          val toSum = (0 until vectorDS.dsSize).map(j => {
            val mElt = vectorDS.at(j)
            val vElt = rhsRanges(rhs).at(j)
            val mEltErr = vectorErr.at(j)
            val vEltErr = rhsErrors(rhs).at(j)
            val mID = FreshIdentifier(s"mElt${rowIndices.head}_$j", RealType)
            val vID = FreshIdentifier(s"vElt${rowIndices.head}_$j", RealType)
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

          val outIndices = rowIndices.map(vi => VectorIndex(vi).asInstanceOf[Index])
          (outIndices -> sumExpr._2(endExpr).fullInterval, outIndices -> sumExpr._3(endExpr).fullInterval)
        }).unzip

        // compute separately for rows where not all elements are the same
        val splitGroupRows = (0 until rhsRanges(lhs).numRows).toSet.diff(sameRangeRows.flatten) // todo see if we can preserve groups
        val rowsSeparate: (Set[(Set[Index], MPFRInterval)], Set[(Set[Index], MPFRInterval)]) = splitGroupRows.map(rowIndices => {
          // vector part
          val origDSIndices = rhsRanges(lhs).indicesAtRow(rowIndices)
          val vectorDS = rhsRanges(lhs).getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()
          val vectorErr = rhsErrors(lhs).getAbstractionOnlyAtIndices(origDSIndices).convertMatrixIndToRowInd()

          val toSum = (0 until vectorDS.dsSize).map(j => {
            val mElt = vectorDS.at(j)
            val vElt = rhsRanges(rhs).at(j)
            val mEltErr = vectorErr.at(j)
            val vEltErr = rhsErrors(rhs).at(j)
            val mID = FreshIdentifier(s"mElt${rowIndices}_$j", RealType)
            val vID = FreshIdentifier(s"vElt${rowIndices}_$j", RealType)
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

          val outIndices = Set(VectorIndex(rowIndices).asInstanceOf[Index])
          (outIndices -> sumExpr._2(endExpr).fullInterval, outIndices -> sumExpr._3(endExpr).fullInterval)
        }).unzip

        val xDSRange = DSAbstraction((rowsOptimal._1 ++ rowsSeparate._1).toMap).regroup()
        val xDSErr = DSAbstraction((rowsOptimal._2 ++ rowsSeparate._2).toMap).regroup()

        //println(s"$lhs x $rhs = \n ${xDSRange.regroup()}")
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErr))

        // matrix multiplication (matrix x matrix)
      case x@CrossProduct(lhs, rhs) if isMatrix(lhs) && isMatrix(rhs) =>
        /// i-th row x vector
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, indices, unrollingScope = unrollingScope)
        assert(rhsRanges(lhs).numCols == rhsRanges(rhs).numRows,
          s"Wrong dimensions for multiplication of matrices $lhs:[${rhsRanges(lhs).numRows}x${rhsRanges(lhs).numCols}] and [$rhs:${rhsRanges(rhs).numRows}x${rhsRanges(rhs).numRows}]")
        val matchingDimension = rhsRanges(lhs).numCols
        val allValidLHSRows = (0 until rhsRanges(lhs).numRows).map(i => genMatrixIndices(i,i+1, 0, rhsRanges(lhs).numCols)).toSet
        val (preGroupRows, singleEltRows) = rhsRanges(lhs).indexGroups.partition({ gr =>
          allValidLHSRows.exists(f=>
            f.diff(gr).isEmpty)})
        val presameRangeRows = preGroupRows.map(g => {
          val rowInds = getAllRowIndices(g)
          rowInds
        })
        val diffRangeRows = singleEltRows.map(g => {
          val rowInds = getAllRowIndices(g)
          rowInds
        })
        // remove all row indices that contain individual specs
        val tmpdiffRows = diffRangeRows.flatten
        val sameRangeRows = presameRangeRows.map(r => r.diff(tmpdiffRows))
        val allValidRHSCols = (0 until rhsRanges(rhs).numCols).map(i => genMatrixIndices(0, rhsRanges(rhs).numRows, i, i+1).toSet)
        val (preGroupCols, singleEltCols) = rhsRanges(rhs).indexGroups.partition({ gr =>
          allValidRHSCols.exists(f =>
            f.diff(gr).isEmpty)})
        val presameRangeCols = preGroupCols.map(g => {
          val colInds = g.map({case MatrixIndex(_,j) => j})
          colInds
        })
        val diffRangeCols = singleEltCols.map(g => {
          val colInds = g.map({case MatrixIndex(_,j) => j})
          colInds
        })
        val tmpdiffCols = diffRangeCols.flatten
        val sameRangeCols = presameRangeCols.map(r => r.diff(tmpdiffCols))
        // compute which rows and columns go together
        val optimalCombo = for (r <- sameRangeRows; c <- sameRangeCols) yield { (r, c)}
        val indivRange1 = for (r <- sameRangeRows; c <- diffRangeCols) yield { (r, c) }
        val indivRange2 = for (r <- diffRangeRows; c <- sameRangeCols) yield { (r, c) }
        val indivRange3 = for (r <- diffRangeRows; c <- diffRangeCols) yield { (r, c) }
        val separateEvalCombo = indivRange1 ++ indivRange2 ++ indivRange3

        // compute the range once per group with the same range
        //println("Optimal combo")
        val chunkOptimal: (Set[(Set[Index], MPFRInterval)], Set[(Set[Index], MPFRInterval)]) = optimalCombo.flatMap(combo => {
          val (rowIndices, colIndices) = combo
          val i = rowIndices.head
          // row from left-hand side
          val lhsRng = rhsRanges(lhs).at(MatrixIndex(i, 0)) // the whole row is the same, so column index is arbitrary
          val lhsErr = rhsErrors(lhs).at(MatrixIndex(i, 0))
          val rhsRng = rhsRanges(rhs).at(MatrixIndex(0, colIndices.head)) // the whole column is the same, so column index is arbitrary
          val rhsErr = rhsErrors(rhs).at(MatrixIndex(0, colIndices.head))

          val newMatrixElts = colIndices.map(j => {
            //var sumStr: String = ""
            val toSum = (0 until matchingDimension).map(k => {
              // i - fixed row, j - fixed column, k varies
              val lhsID = FreshIdentifier(s"lhsElt${i}_$j", RealType)
              val rhsID = FreshIdentifier(s"rhsElt${j}_$k", RealType)
              val timesExpr = Times(Variable(lhsID), Variable(rhsID))
              //sumStr+= s"$lhs[$i,$k] * $rhs[$k, $j] +"

              (timesExpr,
                Map(Variable(lhsID) -> DSAbstraction(lhsRng), Variable(rhsID) -> DSAbstraction(rhsRng)),
                Map(Variable(lhsID) -> DSAbstraction(lhsErr), Variable(rhsID) -> DSAbstraction(rhsErr)))
            })
            //_reporter.info(sumStr)
            // todo optimize further by approximating the sum( a_ij * b_ji) = n*a_ij * b_ji and iterate only for errors
            val sumExpr = toSum.tail.foldLeft((toSum.head._1.asInstanceOf[Expr], rhsRanges ++ toSum.head._2, rhsErrors ++ toSum.head._3))({
              case (acc, x) =>
                val sumE = Plus(acc._1, x._1)
                val tmp = eval(sumE, acc._2 ++ x._2, acc._3 ++ x._3, indices, unrollingScope = unrollingScope)
                (sumE, tmp._1, tmp._2)
            })
            val endExpr = sumExpr._1
            val inds = for (ir <- rowIndices; ic <- colIndices) yield { MatrixIndex(ir, ic).asInstanceOf[Index] }
            //_reporter.info(s"computed ranges valid for $inds")
            (inds -> sumExpr._2(endExpr).fullInterval, inds -> sumExpr._3(endExpr).fullInterval)
          })
          newMatrixElts
        }).unzip

        // compute separately for rows and columns where not all elements are the same
        _reporter.info("Different ranges:")
        val rowsSeparate: (Set[Set[(Set[Index], MPFRInterval)]], Set[Set[(Set[Index], MPFRInterval)]]) = separateEvalCombo.flatMap(combo => {
          val (rowIndices, colIndices) = combo
          val newMatrixElts = rowIndices.map(i => {
            val tmp = colIndices.map(j => {
              //var sumStr: String = ""
              val toSum = (0 until rhsRanges(lhs).numCols).map(k => {
                // i - fixed row, j - fixed column, k varies
                // lhs
                val lhsRng = rhsRanges(lhs).at(MatrixIndex(i, k))
                val lhsErr = rhsErrors(lhs).at(MatrixIndex(i, k))
                // rhs
                val rhsRng = rhsRanges(rhs).at(MatrixIndex(k, j))
                val rhsErr = rhsErrors(rhs).at(MatrixIndex(k, j))
                val lhsID = FreshIdentifier(s"lhsElt${i}_$k", RealType)
                val rhsID = FreshIdentifier(s"rhsElt${k}_$j", RealType)
                val timesExpr = Times(Variable(lhsID), Variable(rhsID))
                // sumStr+= s"$lhs[$i,$k] * $rhs[$k, $j] +"

                (timesExpr,
                  Map(Variable(lhsID) -> DSAbstraction(lhsRng), Variable(rhsID) -> DSAbstraction(rhsRng)),
                  Map(Variable(lhsID) -> DSAbstraction(lhsErr), Variable(rhsID) -> DSAbstraction(rhsErr)))
              })
              //_reporter.info(s"$sumStr -> res[$i][$j]")
              val sumExpr = toSum.tail.foldLeft((toSum.head._1.asInstanceOf[Expr], rhsRanges ++ toSum.head._2, rhsErrors ++ toSum.head._3))({
                case (acc, x) =>
                  val sumE = Plus(acc._1, x._1)
                  val tmp = eval(sumE, acc._2 ++ x._2, acc._3 ++ x._3, indices, unrollingScope = unrollingScope)
                  (sumE, tmp._1, tmp._2)
              })
              val endExpr = sumExpr._1
              (Set(MatrixIndex(i, j).asInstanceOf[Index]) -> sumExpr._2(endExpr).fullInterval,
                Set(MatrixIndex(i, j).asInstanceOf[Index]) -> sumExpr._3(endExpr).fullInterval)
          }).unzip
            //_reporter.info(s"COMBO: $combo\n")
          tmp
          })
          newMatrixElts
        }).unzip

        val xDSRange = DSAbstraction((chunkOptimal._1 ++ rowsSeparate._1.flatten).toMap)
        val xDSErr = DSAbstraction((chunkOptimal._2 ++ rowsSeparate._2.flatten).toMap)
        //println(xDSRange.regroup())
        (rhsRanges + (x->xDSRange), rhsErrors + (x->xDSErr))

      // unary arithmetic operations on DS operations with Real results
      case ArithOperator(es, fnc) if !forceEval && es.length == 1 && isDsExpr(es.head) && es.head.getType == RealType =>
        val arg = es.head
        val (argRanges, argErrors) = eval(arg, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope) // todo indices: for input DS is already there, others?
        val operatorApplied = fnc(es)
        eval(operatorApplied, argRanges, argErrors, indices, path, forceEval = true, unrollingScope = unrollingScope) // todo indices ? + (arg -> Set())

      // unary arithmetic operations with on DS (includes ElemFnc)
      case ArithOperator(es, fnc) if !forceEval && es.length == 1 && hasDsType(es.head) =>
        val arg = es.head
        val (argRanges, argErrors) = eval(arg, rangesAbstr, errorsAbstr, indices, unrollingScope = unrollingScope) // todo indices: for input DS is already there, others?
        val operatorApplied = fnc(es)
        val (opRanges, opErrs) = argRanges(arg).indexGroups.map({ partialIndices =>
          val (partRanges, partErrs) = eval(operatorApplied, argRanges, argErrors, indices + (arg -> partialIndices), path, forceEval = true, unrollingScope = unrollingScope)
          val newRangeMap = partialIndices -> partRanges(operatorApplied).fullInterval
          val newErrMap = partialIndices -> partErrs(operatorApplied).fullInterval
          (newRangeMap, newErrMap)
        }).unzip

        (argRanges + (operatorApplied -> DSAbstraction(opRanges.toMap).regroup()),
          argErrors + (operatorApplied -> DSAbstraction(opErrs.toMap).regroup()))

        // arithmetic operations on DS (both operands are DS)
      // todo dstype is sufficient? && es.forall(isDsExpr)
      case ArithOperator(es, fnc) if !forceEval && es.forall(hasDsType) =>
        assert(es.length == 2, "Arithmetic operations on data structures with more than two operands are currently not supported.")

        val lhs = es.head
        val rhs = es(1)
        val lhsIndices = if (rangesAbstr.contains(lhs)) indices + (lhs -> rangesAbstr(lhs).indices) else indices
        val rhsIndices = if (rangesAbstr.contains(rhs)) indices + (rhs -> rangesAbstr(rhs).indices) else indices
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, lhsIndices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, rhsIndices, unrollingScope = unrollingScope) // calling with the latest results of lhs eval

        val lhsAbstr = lhsRanges(lhs).indexToRange
        val rhsAbstr = rhsRanges(rhs).indexToRange
        val operatorApplied = fnc(es)
        // compute ranges that have to be evaluated together
        val overlap = lhsAbstr.flatMap({
          case (lIndex, _) =>
            val overlapping = rhsAbstr.keys.map(rhsKey => {
              val intersect = rhsKey.intersect(lIndex)
              if (intersect.isEmpty)
                None
              else
                Some((rhsKey, intersect))
            })
            val allMatchingRhs = overlapping.filter(_.nonEmpty)
            // todo figure out what has to be updated in lhs
            allMatchingRhs.map(r => {
              val (rhsIndex, resultIndex) = r.get
              val indConfig = Map(lhs -> lIndex, rhs -> rhsIndex, operatorApplied -> resultIndex)
              val newRanges = eval(operatorApplied, rhsRanges, rhsErrors, indices ++ indConfig, path, forceEval = true, unrollingScope = unrollingScope)
              val range = newRanges._1(operatorApplied).atIndicesJoint(resultIndex)
              val error = newRanges._2(operatorApplied).atIndicesJoint(resultIndex)
             (resultIndex -> range, resultIndex -> error)
            })
        }).toSeq
        assert(overlap.nonEmpty, s"Seems like there are no common indices at $lhs and $rhs. Check the inputs.")
        val (opAppliedRanges, opAppliedErrors) = overlap.unzip
        val dsRangesOpApplied = DSAbstraction(opAppliedRanges.toMap).regroup()
        val dsErrorsOpApplied = DSAbstraction(opAppliedErrors.toMap).regroup()
        (rhsRanges + (operatorApplied -> dsRangesOpApplied), rhsErrors + (operatorApplied -> dsErrorsOpApplied))

      // arithmetic operations on DS with constants/real variables
      case ArithOperator(es, fnc) if !forceEval && hasDsType(es.head) && (es(1).getType == RealType || es(1).getType == Int32Type) =>
        assert(es.length == 2, "Arithmetic operations on data structures with more than two operands are currently not supported.")
        val lhs = es.head
        val rhs = es(1)
        val lhsIndices = if (rangesAbstr.contains(lhs)) indices + (lhs -> rangesAbstr(lhs).indices) else indices
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, lhsIndices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, lhsIndices, unrollingScope = unrollingScope) // calling with the latest results of lhs eval

        val operatorApplied = fnc(es)
        val (opRanges, opErrs) = rhsRanges(lhs).indexGroups.map({ partialIndices =>
            val (partRanges, partErrs) = eval(operatorApplied, rhsRanges, rhsErrors, indices + (lhs -> partialIndices), path, forceEval = true, unrollingScope = unrollingScope)
            val newRangeMap = partialIndices -> partRanges(operatorApplied).fullInterval
            val newErrMap = partialIndices -> partErrs(operatorApplied).fullInterval
            (newRangeMap, newErrMap)
        }).unzip

        (rhsRanges + (operatorApplied -> DSAbstraction(opRanges.toMap).regroup()),
          rhsErrors + (operatorApplied -> DSAbstraction(opErrs.toMap).regroup()))

      // todo does this ever happen? (seems like const * ds doesn't type check
      //case ArithOperator(es, fnc) if !forceEval && es.length == 2 && dsType(es(1)) && es.head.getType == RealType =>
      //  assert(es.length == 2, "Arithmetic operations on data structures with more than two operands are currently not supported.")
      //  val lhs = es.head
      //  val rhs = es(1) // DS type
      //  val rhsIndices = if (rangesAbstr.contains(rhs)) indices + (rhs -> rangesAbstr(rhs).indices) else indices
      //  val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, indices)
      //  val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, rhsIndices) // todo is it ok to call with results of lhs eval?
      //
      //  val operatorApplied = fnc(es)
      //  val (opRanges, opErrs) = rhsRanges(rhs).indexGroups.map({ partialIndices =>
      //    val (partRanges, partErrs) = eval(operatorApplied, rhsRanges, rhsErrors, indices + (rhs -> partialIndices), path, forceEval = true)
      //    val newRangeMap = partialIndices -> partRanges(operatorApplied).fullInterval
      //    val newErrMap = partialIndices -> partErrs(operatorApplied).fullInterval
      //    (newRangeMap, newErrMap)
      //  }).unzip
      //
      //  (rhsRanges + (operatorApplied -> DSAbstraction(opRanges.toMap).regroup()),
      //    rhsErrors + (operatorApplied -> DSAbstraction(opErrs.toMap).regroup()))

      // arithmetic operations on Reals (operands may still include DS expressions, e.g. x.at(i)
      case ArithOperator(es, fnc) if !forceEval && es.exists(isDsExpr) =>
        assert(es.length == 2, "Arithmetic operations on data structures with more than two operands are currently not supported.")

        val lhs = es.head
        val rhs = es(1)
        val lhsIndices = if (rangesAbstr.contains(lhs)) indices + (lhs -> rangesAbstr(lhs).indices) else indices
        val rhsIndices = if (rangesAbstr.contains(rhs)) indices + (rhs -> rangesAbstr(rhs).indices) else indices
        val (lhsRanges, lhsErrors) = eval(lhs, rangesAbstr, errorsAbstr, lhsIndices, unrollingScope = unrollingScope)
        val (rhsRanges, rhsErrors) = eval(rhs, lhsRanges, lhsErrors, rhsIndices, unrollingScope = unrollingScope) // calling with the latest results of lhs eval

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
    (Rational.fromMPFR(err), Interval.fromMPFR(range)) // todo save dsAbstractions into the context
  }

  private def flipDSIndices(origDSRange: DSAbstraction, row: Boolean): DSAbstraction = {
    val middle = if (row) Math.ceil(origDSRange.numRows / 2).toInt else Math.ceil(origDSRange.numCols / 2).toInt
    val updMap = (0 until middle).flatMap(i => {
      val from:Seq[Index] =
        if (row)
          origDSRange.indicesAtRow(i).map(_.asInstanceOf[MatrixIndex]).toSeq
        else origDSRange.indicesAtColunm(i).map(_.asInstanceOf[MatrixIndex]).toSeq
      val to:Seq[Index] =
        if (row)
          origDSRange.indicesAtRow(origDSRange.numRows - 1 - i).map(_.asInstanceOf[MatrixIndex]).toSeq
        else origDSRange.indicesAtColunm(origDSRange.numCols - 1 - i).map(_.asInstanceOf[MatrixIndex]).toSeq
      val newMap: Seq[(Set[Index], MPFRInterval)] = (from.sorted.zip(to.sorted)).flatMap({
        case (fromI: Index, toI: Index) =>
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

    DSAbstraction(updMap ++ middleRowOrColumn).regroup()
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
    case _ => lhsRanges.head.xlo < MPFRFloat.zero // definitely false, e.g. [-10.-1] >= 0, and potentially false, e.g. [-5,5] >= 0 (returns false)
  }

  /**
   *  Returns an updated range that satisfies the conditions. Conditions must be
   *  a comparison between a variable and a constant or a conjunction of such comparisons
   * @param ds the data structure variable
   * @param dsRange the ds variable's range (current)
   * @param lambda condition according to which ds range should be improved
   * @return Option[Interval] improved range or None if conditions couldn't be satisfied
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
    val mapDSAtIndices = mapDS.to(collection.mutable.HashMap).map({ case (ex, inds) =>
      (ex, path) -> rangesAbstr(ex).atIndicesJoint(inds)})
    // RealType variables, that are not yet included into the mapDS and for which there is no range for the specified index
    val reals = rangesAbstr.filterNot({case (k,v) =>
      hasDsType(k) || mapDS.contains(k) || (indices.contains(k) && indices(k).intersect(v.indices).isEmpty)
    })
    val mapReals = reals.map({case (ex, abstraction) => (ex, path) -> abstraction.fullInterval})
    CachingMap.fromMap(mapDSAtIndices ++ mapReals)
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
  private def toIntermediateRangesWIntervals(indices: Map[Expr, Set[Index]],
                                   rangesAbstr: Map[Expr, DSAbstraction],
                                   path: PathCond = emptyPath): Map[(Expr, PathCond), Interval] = {
    val mapDS = indices.filter({
      case (ex, list) => rangesAbstr.contains(ex) && list.diff(rangesAbstr(ex).indices).isEmpty
    }) // there is an abstraction for each index requested
    val mapDSAtIndices = mapDS.to(collection.mutable.HashMap).map({ case (ex, inds) =>
      (ex, path) -> rangesAbstr(ex).atIndicesJoint(inds).toInterval})
    // RealType variables, that are not yet included into the mapDS and for which there is no range for the specified index
    val reals = rangesAbstr.filterNot({case (k,v) =>
      hasDsType(k) || mapDS.contains(k) || (indices.contains(k) && indices(k).intersect(v.indices).isEmpty)
    })
    val mapReals = reals.map({case (ex, abstraction) => (ex, path) -> abstraction.fullInterval.toInterval})
    mapDSAtIndices.toMap ++ mapReals
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
  def genMatrixIndices(fromRow:Int, toRow: Int, fromCol: Int, toCol: Int): Set[Index] = {
    val indices = for (ir <- (fromRow until toRow); ic <-  (fromCol until toCol)) yield { MatrixIndex(ir, ic) }
    indices.toSet
  }

  def genSortedVectorIndices(from: Int, to: Int): Seq[Index] =
    (from until to).map(i=> VectorIndex(i).asInstanceOf[Index]).sorted

  def sumOfPows(b:MPFRInterval, n: Int): MPFRInterval = {
    val pows = List.fill(n)(b).zipWithIndex.map({ case (x, i) => x.^(i)})
    pows.foldLeft(MPFRInterval.zero)({case (acc, x) => acc+x})
  }

  def matrixIndicesAtRow(inds: Set[Index], i: Int): Set[Index] = inds.filter({
    case MatrixIndex(ii, _) if ii == i => true
    case _ => false
  })

  def containsAllRowElements(where: Set[Index], numRow: Int, numCols: Int): Boolean = {
    val ref = (0 until numCols).map(j => MatrixIndex(numRow, j).asInstanceOf[Index]).toSet
    ref.diff(where).isEmpty
  }

  def getAllRowIndices(in: Set[Index]): Set[Int] = {
    in.map({case MatrixIndex(i,_) => i}).toSet
  }
}
