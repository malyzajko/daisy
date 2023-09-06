package daisy.tools

import daisy.DaisyFatalError
//import scala.language.implicitConversions

trait Index extends Ordered[Index] {
    val i: Int
    def diff(that: Index): Int
    override def compare(that: Index): Int = (this, that) match {
        case (VectorIndex(i1), VectorIndex(i2)) => i1.compare(i2)
        case (MatrixIndex(i1, j1), MatrixIndex(i2, j2)) =>
            // 1 this > that
            // 0 this == that
            //-1 this < that
            if (i1==i2 && j1==j2) 0
            else if (i1 > i2) 1
            else if (i1 == i2 && j1 > j2) 1
            else -1
    }
    //implicit def vectorIndToInt(x: VectorIndex): Int = x.i
    //implicit def matrixIndToInt(x: MatrixIndex): (Int, Int) = (x.i, x.j)
}

case class VectorIndex(i: Int) extends Index {
    override def diff(that: Index): Int = Math.abs(this.i - that.i) // todo abs needed?
}

case class MatrixIndex(i: Int, j: Int) extends Index {
    override def toString: String = (i,j).toString
    val rowNr: Int = i
    override def diff(that: Index): Int = that match {
        case MatrixIndex(i2, j2) if i == i2 => Math.abs(j - j2)
        case MatrixIndex(i2, j2) if j2 == 0 => Math.abs(i - i2)
        case _ => -1 // todo cannot know the real diff without total number of columns
    }
}

object DSAbstraction {
    def apply(range: MPFRInterval): DSAbstraction = DSAbstraction(Map(Set[Index]() -> range))
    def apply(r: Rational): DSAbstraction = DSAbstraction(Map(Set[Index]() -> MPFRInterval(r)))
    def empty(): DSAbstraction = DSAbstraction(Map[Set[Index],MPFRInterval]())
}
/**
 *
 * @param indexToRange mapping from indices of elements to their range (elements with the same range are grouped in one set)
 */
case class DSAbstraction(indexToRange: Map[Set[Index], MPFRInterval]) {
    override def toString: String = "{\n " + indexToRange.map({ case (k, v) => f"$k -> $v\n" }) + " }\n"

    lazy val fullInterval: MPFRInterval = MPFRInterval.union(indexToRange.values.toSet)
    lazy val dsSize: Int = indexToRange.keySet.flatten.size // count the indices for which there is a range specified

    // only applicable to Matrices
    lazy val numRows: Int = {
        val allIndices = indexToRange.keySet.toSeq.flatten
        allIndices.map({ case MatrixIndex(i, _) => i }).distinct.size
    } // get the number of rows
    lazy val numCols: Int = {
        val allIndices = indexToRange.keySet.toSeq.flatten
        allIndices.map({ case MatrixIndex(_, j) => j }).distinct.size
    } // get the number of columns
    // end - only applicable to Matrices

    lazy val indices: Set[Index] = indexToRange.keySet.flatten
    lazy val indexGroups: Set[Set[Index]] = indexToRange.keySet
    lazy val hasSingleRange: Boolean = indexToRange.values.size == 1

    def ranges: Seq[MPFRInterval] = indexToRange.values.toSeq

    /**
     * Get a subset of the abstraction on specified indices and other indices that have the same ranges
     *
     * @param indices set of indices for which the abstraction is returned
     * @return
     */
    def getAbstractionAtIndices(indices: Set[Index]): DSAbstraction = {
        val tmp = indexToRange.filter({ case (ind, _) => ind.intersect(indices).nonEmpty })
        DSAbstraction(tmp)
    }

    /**
     * Get a subset of the abstraction ONLY on specified indices
     *
     * @param indices set of indices for which the abstraction is returned
     * @return
     */
    def getAbstractionOnlyAtIndices(indices: Set[Index]): DSAbstraction = {
        val overlapMap = indexToRange.flatMap({ case (ind, range) =>
            val overlap = ind.intersect(indices)
            if (overlap.nonEmpty)
                Some(overlap -> range)
            else
                None
        })
        assert(overlapMap.nonEmpty, s"No ranges exist for $indices inside an abstraction $indexToRange")
        DSAbstraction(overlapMap)
    }

    def indexGroupsFor(indices: Set[Index]): Set[Set[Index]] = {
        val tmp = indexToRange.filter({ case (ind, _) => ind.intersect(indices).nonEmpty }).keySet
        tmp
    }

    def at(i: Index): MPFRInterval = {
        val corrMap = indexToRange.find({ case (seq, _) => seq.contains(i) })
        assert(corrMap.isDefined, s"Trying to obtain the range for index $i that does not exist in the abstraction $this")
        corrMap.get._2
    }

    def at(i: Int): MPFRInterval = {
        val corrMap = indexToRange.find({ case (seq, _) => seq.contains(VectorIndex(i)) })
        assert(corrMap.isDefined, s"Trying to obtain the range for index $i that does not exist in the abstraction $this")
        corrMap.get._2
    }

    def at(i: Int, j: Int): MPFRInterval = {
        val corrMap = indexToRange.find({ case (seq, _) => seq.contains(MatrixIndex(i, j)) })
        assert(corrMap.isDefined, s"Trying to obtain the range for index $i that does not exist in the abstraction $this")
        corrMap.get._2
    }

    /**
     * Returns one joint interval for all requested indices
     *
     * @param indices sequence of data structure indices
     * @return one interval that covers all ranges at indices
     */
    def atIndicesJoint(indices: Set[Index]): MPFRInterval = {
        if (indices.isEmpty)
            if (indexToRange.contains(indices))
                indexToRange(indices)
            else
                throw new DaisyFatalError(Some(s"No value in the data structure abstraction at $indices: $indexToRange"))
        else {
            val allVals = atIndicesAll(indices)
            MPFRInterval.union(allVals)
        }
    }

    /**
     * Looks up elements' ranges for requested indices, returns a set of unique ranges
     *
     * @param indices sequence of data structure indices
     * @return a set of intervals for the groups that contain at least one element of [indices]
     */
    def atIndicesAll(indices: Set[Index]): Set[MPFRInterval] = {
        val relevantGroups = indexToRange.keySet.filter(gr => gr.intersect(indices).nonEmpty)
        assert(relevantGroups.nonEmpty, s"Trying to obtain the range for indices $indices that do not exist in the abstraction $this")
        relevantGroups.map(g => indexToRange(g))
    }


    def updateWith(newMap: Map[Set[Index], MPFRInterval]): DSAbstraction = {
        val newKeys = newMap.keySet.flatten
        val interleaving = this.indexToRange.keys.flatten.toSet.intersect(newKeys)
        if (interleaving.nonEmpty) {
            // remove new indices from old specs
            val updated = indexToRange.map({ case (inds, ints) =>
                val keep = inds.diff(interleaving)
                if (keep.isEmpty)
                    None
                else Some(keep -> ints)
            }).filter(_.isDefined).map(_.get).toMap
            DSAbstraction(updated ++ newMap)
        } else
            DSAbstraction(indexToRange ++ newMap)
    }

    /**
     * Updates the abstraction by overwriting the range for given indices
     *
     * @param indices  list of indices, for which the abstraction is updated
     * @param interval the new range specification (for the given indices)
     * @return updated abstraction
     */
    def addSpec(indices: Set[Index], interval: MPFRInterval, debug: Boolean = false): DSAbstraction = {
        val interleaving = this.indexToRange.keySet.flatten.intersect(indices)
        if (interleaving.nonEmpty) {
            if (debug)
                println(s"[ DEBUG ]  Overlapping specification for indices $interleaving")
            // update the spec -> remove from old specs current indices
            val updated = indexToRange.map({ case (inds, ints) =>
                val keep = inds.diff(interleaving)
                if (keep.isEmpty)
                    None
                else Some(keep -> ints)
            }).filter(_.isDefined).map(_.get).toMap
            val updIndexToRange = updated + (indices -> interval)
            val grouped = updIndexToRange.groupBy({case (_, interval) => interval})
            val merged = grouped.map({case (k,v) => v.keySet.flatten -> k})
            DSAbstraction(merged)
        } else {
            val updIndexToRange = indexToRange + (indices -> interval)
            val grouped = updIndexToRange.groupBy({case (_, interval) => interval})
            val merged = grouped.map({case (k,v) => v.keySet.flatten -> k})
            DSAbstraction(merged)
        }
    }

    /**
     * Updates the abstraction by adding the range for given indices.
     * Does not overwrite existing spec if already exists for (some) indices.
     *
     * @param indices  list of indices, for which the abstraction may be updated
     * @param interval the new range specification (for the given indices)
     * @return
     */
    def addSpecNoUpdate(indices: Set[Index], interval: MPFRInterval, debug: Boolean = false): DSAbstraction = {
        val interleaving = this.indexToRange.keySet.flatten.intersect(indices)
        if (interleaving.nonEmpty && debug) {
            println(s"[ DEBUG ]  Overlapping specification for indices $interleaving. New range $interval will be ignored for these indices.")
        }
        // ignore the latest spec for overlapping indices
        val uniqueIndicies = indices.diff(interleaving)
        if (uniqueIndicies.nonEmpty)
            DSAbstraction(indexToRange + (uniqueIndicies -> interval))
        else
            this
    }

    def indicesAtRow(row: Int): Set[Index] = {
        val allIndices = indexToRange.keySet.flatten
        allIndices.filter({ case MatrixIndex(i, _) => i == row })
    }

    def indicesAtColunm(col: Int): Set[Index] = {
        val allIndices = indexToRange.keySet.flatten
        allIndices.filter({ case MatrixIndex(_, j) => j == col })
    }

    //def getAbstractionForRow(row: Int): DSAbstraction = {
    //    val indices = indicesAtRow(row)
    //    getAbstractionOnlyAtIndices(indices)
    //}

    def convertMatrixIndToRowInd(): DSAbstraction = {
        val allIndices = indexToRange.keySet.flatten
        val rows = allIndices.map({case MatrixIndex(i,_) => i})
        val cols = allIndices.map({case MatrixIndex(_, j) => j})
         if (rows.size == 1) {
             // just remove the row index
             val Row = rows.head
             val mapWVectorIndices = indexToRange.map({case (indices, interval) =>
                 val updIndices = indices.map({ case MatrixIndex(Row, j) => VectorIndex(j).asInstanceOf[Index]})
                 updIndices -> interval
             })
             DSAbstraction(mapWVectorIndices)
         } else {
             // merge intervals per column index [ e.g., at (0,1), (1,1) and (2,1)]
             val eachIndexToRange = cols.map(c => {
                 val toMerge = rows.map(r => MatrixIndex(r, c).asInstanceOf[Index])
                 val range = getAbstractionOnlyAtIndices(toMerge).fullInterval
                 VectorIndex(c).asInstanceOf[Index] -> range
             })
             val grouped = eachIndexToRange.groupBy({case (_, interval) => interval})
             val merged = grouped.map({case (k,v) => v.toMap.keySet -> k})
             DSAbstraction(merged)
         }
    }

    def regroup(): DSAbstraction = {
        val grouped = indexToRange.groupBy({case (_, interval) => interval})
        val merged = grouped.map({case (k,v) => v.keySet.flatten -> k})
        DSAbstraction(merged)
    }

    def countToRangeWithRepresentative(): Map[(Int, Index), MPFRInterval] =
        indexToRange.map({case (inds, range) => (inds.size, inds.head) -> range })

    def conseqGroupsWRepresentative(): Seq[((Int, Index), MPFRInterval)] = {
        val conseqGroups = indexToRange.flatMap({
            case (inds, range) =>
                val sorted = inds.toSeq.sorted
                if (sorted.size > 1) {
                    val disordered = sorted.sliding(2).toSeq.partition({ case Seq(x, y, _*) => y.diff(x) == 1 })._2.sortBy(_.head)
                    val parts = disordered.foldLeft((Set[Seq[Index]](), 0))({ case (acc, x) =>
                        val origInd = sorted.indexOf(x.head)
                        val tmp: Seq[Index] = sorted.slice(acc._2, origInd + 1)
                        //val a = acc._1
                        (acc._1 + tmp, sorted.indexOf(x(1)))
                    })
                    val allParts = (parts._1 + sorted.splitAt(parts._2)._2).filter(_.nonEmpty)
                    allParts.map(newGroups =>
                        (newGroups.size, newGroups.head) -> range
                    )
                } else if (sorted.nonEmpty) {
                    Set((sorted.size, sorted.head) -> range)
                } else {
                    throw new DaisyFatalError(Some("Empty sequence of indices in a DS"))
                }
        })
        conseqGroups.toSeq.sortBy({case ((_, ind),_) => ind})
    }
}