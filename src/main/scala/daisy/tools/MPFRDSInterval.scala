package daisy
package tools


import scala.collection.mutable.ListBuffer
import org.gnu.glpk.GLPK
import org.gnu.glpk.GLPKConstants
import org.gnu.glpk.SWIGTYPE_p_double
import org.gnu.glpk.SWIGTYPE_p_int
import org.gnu.glpk.glp_smcp

import MPFRFloat.{zero => fzero, one => fone, _}


object MPFRDSInterval {
  val roundOff = MPFRFloat.fromDouble(1e-7)
  def apply(i: MPFRInterval, w: MPFRFloat): MPFRDSInterval = MPFRDSInterval(List((i,w))) // construct DS structure with one focal element
  def time[R](block: => R): (R, Long) = {
    val t0 = System.currentTimeMillis
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis
    (result, (t1 - t0))
  }
  def isOverlapping (dsInterval1: MPFRInterval, dsInterval2: MPFRInterval): Boolean = {
    (dsInterval1.xlo <= dsInterval2.xhi && dsInterval2.xlo <= dsInterval1.xhi)
  }
  def reduce(d: List[(MPFRInterval, MPFRFloat)]): MPFRDSInterval = {
    val lengthDS = d.length
    val maxLength = 20

    val dSorted = d.sortWith(_._1.xlo < _._1.xlo)

    var res = dSorted
    var i = 0
    while (res.length > maxLength) {
        //println(s"reduction iteration #$i")
      i = i + 1
      val tmpRes = reduction(res, maxLength)
      if (tmpRes.length == res.length) {
        println(" !!! Reducing non-overlapping !!!")
        res = reduceNonOverlapping(res)
      } else {
        res = tmpRes
      }
    }
    MPFRDSInterval(res)
  }

  // if all else fails, we have to reduce non-overlapping intervals
  def reduceNonOverlapping (d: List[(MPFRInterval, MPFRFloat)]): List[(MPFRInterval, MPFRFloat)] = {
    val tmp: (List[(MPFRInterval, MPFRFloat)], Boolean) = d.tail.foldLeft((List(d.head), false)) {
      // previous element was merged, don't merge again
      case ((acc, true), (intNext, r2)) =>
        ((intNext, r2) :: acc, false)

      case ((acc, false), (intNext, r2)) =>
        val (intBefore, r1) = acc.head
        ((MPFRInterval(intBefore.xlo, intNext.xhi), r1 + r2) :: acc.tail, true)
    }
    tmp._1.reverse
  }

  def reduction(d: List[(MPFRInterval, MPFRFloat)], maxLength: Int): List[(MPFRInterval, MPFRFloat)] = {
    if (d.length > maxLength) {
      val origLen = d.length
      // if there are small weights, merge those
      val minWeight = MPFRFloat.fromDouble(1e-5)
      var currentDS: List[(MPFRInterval, MPFRFloat)] = d.tail.foldLeft(List(d.head)) {
        case (acc, (intNext, r2)) =>
          val (intBefore, r1) = acc.head
          if ((r2 <= minWeight) && isOverlapping(intBefore, intNext)) {
            (MPFRInterval(intBefore.xlo, intNext.xhi), r1 + r2) :: acc.tail
          }
          else {
            (intNext, r2) :: acc
          }
      }.reverse
      val minWeightLen = currentDS.length
      // if (minWeightLen < origLen)
      //   println(s"size after min-Weight reduction: ${minWeightLen}")

      // compute the std. deviation of the interval widths
      val allWidths = currentDS.map({ case (i, w) => i.width.doubleValue() })
      val sumWidths = allWidths.fold(0.0)((sum, next) => sum + next)
      val avrgWidth = sumWidths / allWidths.length
      val stdDevWidth = math.sqrt(allWidths.fold(0.0)((sum, next) =>
        sum + (next - avrgWidth)*(next - avrgWidth)) / (allWidths.length - 1))
      //println(s"std deviation widths: $stdDevWidth")

      val stdDevWidthThreshold = 1e-3
      if (stdDevWidth > stdDevWidthThreshold) {
        // there is a substantial difference in interval widths, merge the smallest (overlapping) ones
        val minWidth = MPFRFloat.fromDouble(avrgWidth - stdDevWidth)
        currentDS = currentDS.tail.foldLeft(List(currentDS.head)) {
          case (acc, (intNext, r2)) =>
            val (intBefore, r1) = acc.head
            if ((intNext.width < minWidth) && isOverlapping(intBefore, intNext)) {
              //println(" !!! Reducing elements with small width !!!")

              (MPFRInterval(intBefore.xlo, intNext.xhi), r1 + r2) :: acc.tail
            }
            else {
              (intNext, r2) :: acc
            }
        }.reverse
      }
      val widthReduceLen = currentDS.length
      // if (widthReduceLen < minWeightLen)
      //   println(s"size after width reduction: ${currentDS.length}")

      // compute the std. deviation of the weights
      val allWeights = currentDS.map({ case (i, w) => w.doubleValue() })
      val sumWeights = allWeights.fold(0.0)((sum, next) => sum + next) // should be one...
      val avrgWeight = sumWeights / allWeights.length
      val stdDevWeight = math.sqrt(allWeights.fold(0.0)((sum, next) =>
        sum + (next - avrgWeight)*(next - avrgWeight)) / (allWeights.length - 1))

      // if all the interval widths are the same, but there is a difference in weights,
      // merge those which are smaller
      val stdDevWeightThreshold = 1e-4
      // TODO: make this depend on the std dev of the interval width?
      if (stdDevWeight > stdDevWeightThreshold) {
        // there is a substantial difference in interval widths, merge the smallest (overlapping) ones
        val minWeight = MPFRFloat.fromDouble(avrgWeight - stdDevWeight)
        currentDS = currentDS.tail.foldLeft(List(currentDS.head)) {
          case (acc, (intNext, r2)) =>
            val (intBefore, r1) = acc.head
            if ((r2 < minWeight) && isOverlapping(intBefore, intNext)) {
              (MPFRInterval(intBefore.xlo, intNext.xhi), r1 + r2) :: acc.tail
            }
            else {
              (intNext, r2) :: acc
            }
        }.reverse
      }
      val weightReduceLen = currentDS.length
      // if (weightReduceLen < widthReduceLen)
      //   println(s"size after weight reduction: ${currentDS.length}")

      //else (varianceWeights <= minWeight && varianceWidths <= varianceWidthThreshold) {
      // if the above were not sufficient to reduce the length enough, just merge every second overlapping interval
      if (currentDS.length > maxLength) {
        // if the second argument is true, the previous element was merged
        val tmp: (List[(MPFRInterval, MPFRFloat)], Boolean) = currentDS.tail.foldLeft((List(currentDS.head), false)) {

          // previous element was merged, don't merge again
          case ((acc, true), (intNext, r2)) =>
            ((intNext, r2) :: acc, false)

          case ((acc, false), (intNext, r2)) =>
            val (intBefore, r1) = acc.head
            if (isOverlapping(intBefore, intNext)) {
              ((MPFRInterval(intBefore.xlo, intNext.xhi), r1 + r2) :: acc.tail, true)
            }
            else {
              ((intNext, r2) :: acc, false)
            }
        }
        currentDS = tmp._1.reverse
      }
      //println(s"size after all reductions: ${currentDS.length}")

      // val finalWeights = currentDS.map({ case (i, w) => w })
      // val finalSumWeights = finalWeights.fold(rzero)((sum, next) => sum + next) // should be one...
      // println(s"sum weights: ${finalSumWeights}")
      currentDS
    }
    else d
  }

  def initializeIntervalMatrix(d1: MPFRDSInterval, d2: MPFRDSInterval): Array[Array[MPFRInterval]] = {       //Initializing the interval matrix
    //println("\nEntering Initialize Matrix")

    val row = d2.dsi.size + 1
    val col = d1.dsi.size + 1
    val intervalMatrix = Array.ofDim[MPFRInterval](row,col)
    d1.dsi.zipWithIndex.foreach {
      case (focalElem, iter) => intervalMatrix(row-1)(iter) = focalElem._1
    }
    d2.dsi.zipWithIndex.foreach {
      case (focalElem, iter) => intervalMatrix(iter)(col-1) = focalElem._1
    }
    intervalMatrix(row-1)(col-1) = MPFRInterval(fzero)
    //println("\nExiting Initialize Matrix")
    intervalMatrix
  }

  def initializeWeight(ds: MPFRDSInterval): Array[MPFRFloat] = {
    //Initializing the weight matrix
    val (_, weights) = ds.dsi.unzip
    weights.toArray
  }
  // ======================================================== Generating the p_box ==============================================================
  def generatePBox(intervalMatrix: Array[Array[MPFRInterval]], d1Weight: Array[MPFRFloat], d2Weight: Array[MPFRFloat]):
   (List[(MPFRFloat, MPFRFloat)], List[(MPFRFloat, MPFRFloat)]) = {
    val row = d2Weight.size
    val col = d1Weight.size
    val nrow = row + col
    val ncol = row * col
    //Initializing the linear program in GLPK Simplex Solver
    val lpp = GLPK.glp_create_prob()
    GLPK.glp_set_prob_name(lpp, "pl")
    GLPK.glp_term_out(GLPKConstants.GLP_OFF)
    // Initializing the ncol unknowns
    GLPK.glp_add_cols(lpp, ncol)
    for (i <- 1 to ncol) {
      GLPK.glp_set_col_kind(lpp, i, GLPKConstants.GLP_CV) // The variables are continuous
      // Constraints stating that each weight is greater than equal to 0 and less than equal to 1
      GLPK.glp_set_col_bnds(lpp, i, GLPKConstants.GLP_DB, 0.0, 1.0)
    }
    // Allocate memory to pass on information about each constraint
    // the +1 is needed, no idea why
    val ind1: SWIGTYPE_p_int = GLPK.new_intArray(col+1)
    val value1: SWIGTYPE_p_double = GLPK.new_doubleArray(col+1)
    val ind2: SWIGTYPE_p_int = GLPK.new_intArray(row+1)
    val value2: SWIGTYPE_p_double = GLPK.new_doubleArray(row+1)

    // There will be nx+ny constraints
    GLPK.glp_add_rows(lpp, nrow)
    // Upper bounds on each constraint, e.g. x1 + x2 = upper_bound
    for (i <- 1 to row) {
      GLPK.glp_set_row_bnds(lpp, i, GLPKConstants.GLP_FX, d2Weight(i-1).doubleValue(), d2Weight(i-1).doubleValue())
    }
    for (i <- row+1 to nrow) {
      GLPK.glp_set_row_bnds(lpp, i, GLPKConstants.GLP_FX, d1Weight(i-1-row).doubleValue(), d1Weight(i-1-row).doubleValue())
    }
    // All coefficients are 1.0
    for (j <- 1 to col) {
      GLPK.doubleArray_setitem(value1, j, 1.0)
    }
    //Adding row constraints
    for (i <- 1 to row) {
      for (j <- 1 to col) {
        GLPK.intArray_setitem(ind1, j, (i-1)*col+j) // Writing the constraints row wise
      }
      //Populate the row of the matrix
      GLPK.glp_set_mat_row(lpp, i, col, ind1, value1)
    }
    //Free the memory
    GLPK.delete_intArray(ind1)
    GLPK.delete_doubleArray(value1)

    for (j <- 1 to row) {
      GLPK.doubleArray_setitem(value2, j, 1.0)
    }
    for (i <- 1 to col) { // Writing the constraints column wise
      for (j <- 1 to row) {
        GLPK.intArray_setitem(ind2, j, i+(j-1)*col)
      }
      GLPK.glp_set_mat_row(lpp, i+row, row, ind2, value2)
    }
    //Free the memory
    GLPK.delete_intArray(ind2)
    GLPK.delete_doubleArray(value2)
    // Find the lower and upper bounds of the intervals
    var hiSetTraversed: Set[MPFRFloat] = Set()
    var lowSetTraversed: Set[MPFRFloat] = Set()
    for (x <- 0 until d2Weight.length ; y <- 0 until d1Weight.length ) {
      val i = intervalMatrix(x)(y)
      lowSetTraversed += i.xlo
      hiSetTraversed += i.xhi
    }
    val hiPbox = new ListBuffer[(MPFRFloat, MPFRFloat)]
    for (hiBnd <- hiSetTraversed.toList.sorted) {
      var constraintHi: Set[Int] = Set()
      //check upper bounds which are less than or equal to interval_hi
      for (z <- 0 until d2Weight.length; k <- 0 until d1Weight.length) {
        if (intervalMatrix(z)(k).xhi <= hiBnd) {
          constraintHi += z * col + k + 1
        }
      }
      if (!(constraintHi.isEmpty)) {
        // If not empty, generate the objective function for the upper bound
        GLPK.glp_set_obj_dir(lpp, GLPKConstants.GLP_MIN)
        for(i <- 0 to ncol) {
          if (constraintHi.contains(i)) {
            GLPK.glp_set_obj_coef(lpp, i, 1.0)
          }
          else{
            GLPK.glp_set_obj_coef(lpp, i, 0.0)
          }
        }
        // Solve model
        val parm: glp_smcp = new glp_smcp()
        GLPK.glp_init_smcp(parm)

        //Calling the GLPK simplex solver for the upper bound of p box
        val retHi = GLPK.glp_simplex(lpp, parm)
        //println("\n Need to get the minimized value")

        if (retHi != 0) {
          throw NoSolutionException("The problem could not be solved " + toString)
        }
        // Retrieve solution
        val resultHi = GLPK.glp_get_obj_val(lpp)

        // Store the upper bound of the p box
        hiPbox += ((hiBnd, fromDouble(resultHi)))
      }
      else {
        // If empty, weight is 0
        hiPbox += ((hiBnd, fzero))
      }
    }

    var loPbox = new ListBuffer[(MPFRFloat, MPFRFloat)]
    for (lowBnd <- lowSetTraversed.toList.sorted) {
      var constraintLo: Set[Int] = Set()

      for (z <- 0 until d2Weight.length; k <- 0 until d1Weight.length) {
        if (intervalMatrix(z)(k).xlo <= lowBnd) {
          constraintLo += z * col + k + 1
        }
      }

      if (!(constraintLo.isEmpty)) {
        GLPK.glp_set_obj_dir(lpp, GLPKConstants.GLP_MAX)

        for(i <- 0 to ncol) {
          if (constraintLo.contains(i)) {
            GLPK.glp_set_obj_coef(lpp, i, 1.0)
          }
          else {
            GLPK.glp_set_obj_coef(lpp, i, 0.0)
          }
        }
        // Solve model
        val parm: glp_smcp = new glp_smcp()
        GLPK.glp_init_smcp(parm)

        val retLo = GLPK.glp_simplex(lpp, parm) //Calling the GLPK simplex solver for the lower bound of p box
        if (retLo != 0)
          throw NoSolutionException("The problem could not be solved " + toString)
            // Retrieve solution
        val resultLo = GLPK.glp_get_obj_val(lpp)
        loPbox += ((lowBnd, fromDouble(resultLo))) //Building the lower bound of the p box
      }
      else {
        loPbox += ((lowBnd, fzero)) //If empty, weight is 0
      }
    }

    GLPK.glp_delete_prob(lpp)

    val lowerPbox = (loPbox.toList).sortBy(_._1)
    val upperPbox = (hiPbox.toList).sortBy(_._1)

    (MPFRDSInterval.cleanPBox(lowerPbox), MPFRDSInterval.cleanPBox(upperPbox)) //Returns the lower and upper P-Box without non-staircase if any

  }

  def pBoxToDS(edgeOfLoPbox: List[(MPFRFloat, MPFRFloat)], edgeOfHiPbox: List[(MPFRFloat, MPFRFloat)]) : MPFRDSInterval = {
    var z = new ListBuffer[(MPFRInterval, MPFRFloat)]()
    var i = 0
    var j = 0
    var accumulatedWeight = fzero //Accumulated weight
    var computedInterval = MPFRInterval(fzero)
    while (i < edgeOfLoPbox.length && j < edgeOfHiPbox.length) {
      val intervalLo = edgeOfLoPbox(i)._1
      val intervalHi = edgeOfHiPbox(j)._1
      if (intervalLo < intervalHi) computedInterval = MPFRInterval(intervalLo, intervalHi) //Generates the interval for the focal element
      else computedInterval = MPFRInterval(intervalHi, intervalLo)
       //Generates the interval for the focal element
      if (edgeOfLoPbox(i)._2 > edgeOfHiPbox(j)._2) { //In case the weight of the lower P-box is greater than the higher P-Box
        val weight = edgeOfHiPbox(j)._2 - accumulatedWeight //Gets the weight for the interval
        z += ((computedInterval, weight)) // Stores the focal elements
        accumulatedWeight = accumulatedWeight + weight
        if (j == (edgeOfHiPbox.length-1)) {
          if (i == (edgeOfLoPbox.length - 1)) {
            j = j + 1
          }
          else {
            i = i + 1
          }
        }
        else {
          j = j + 1
        }
      }
      else { //In case the weight of the higher P-Box is greater than equal to the lower P-box
        val weight = edgeOfLoPbox(i)._2 - accumulatedWeight
        z += ((computedInterval, weight))
        accumulatedWeight += weight
        if (i == (edgeOfLoPbox.length-1)) {
          if (j == (edgeOfHiPbox.length - 1)) {
            i = i + 1
          }
          else {
            j = j + 1
          }
        }
        else{
          i = i + 1
        }
      }

    }
    val res = (z.toList).filter(_._2 >= roundOff) //Deleting the weights less than equal to lowWeight
    MPFRDSInterval.reduce(res) //Returns the resultant DS structure
  }

  //TODO: Soundness of the method needs to be checked.
  def cleanPBox(pBox:List[(MPFRFloat, MPFRFloat)]): List[(MPFRFloat, MPFRFloat)] = { //Merges two P-Boxes
    //val period = Rational.fromReal(1e-7)
    val errorThreshold = MPFRFloat.fromDouble(1.0) + roundOff
    pBox.foldLeft(List.empty[(MPFRFloat, MPFRFloat)]) {
      case (a, b) if a.isEmpty => List(b)
      case (acc, (e2, w2)) => {
        val (e1, w1) = acc.head
        if (e1 > e2) { //Deleting if the Pbox is not staircase
          (e1, w2) :: acc
        }
        else if (w1 > w2) { //Deleting if the Pbox is not staircase
          assert ((w1 - w2) < roundOff, "difference: " + (w1 - w2))  // Asserting to make sure that the difference is very small
          acc
        }
        else if (w1 == w2) acc
        else if (w2 > fone) {
          assert (w2 < errorThreshold)
          (e2, fone) :: acc
        }
        else (e2, w2) :: acc
      }
    }.reverse
  }

  def cleanPBoxForGraph(pBox:List[(MPFRFloat, MPFRFloat)]): List[(MPFRFloat, MPFRFloat)] = { //Merges two P-Boxes
    //val period = Rational.fromReal(1e-7)
    val errorThreshold = MPFRFloat.fromDouble(1.0) + roundOff
    pBox.foldLeft(List.empty[(MPFRFloat, MPFRFloat)]) {
      case (a, b) if a.isEmpty => List(b)
      case (acc, (e2, w2)) => {
        val (e1, w1) = acc.head
        if (w1 > w2) { //Deleting if the Pbox is not staircase
          // assert ((w1 - w2) < roundOff, "difference: " + (w1 - w2))  // Asserting to make sure that the difference is very small
          if ((w1-w2) > roundOff) {
            println("Difference: " + (w1 - w2))
          }
          acc
        }
        else if (w1 == w2) acc
        else if (w2 > fone) {
          assert (w2 < errorThreshold)
          (e2, fone) :: acc
        }
        else (e2, w2) :: acc
      }
    }.reverse
  }

}
case class MPFRDSInterval(dsi: List[(MPFRInterval, MPFRFloat)]) {
  import MPFRDSInterval._
  dsi.foreach(x => assert(x._2 >= fzero, "weights are smaller than 0"))
  val (intervals, weights) = dsi.unzip
  val sumOfWeights = weights.foldLeft(fzero)(_+_)
  val diff = sumOfWeights - fone
  assert(diff <= roundOff, s"!!! sum of weights is greater than 1 !! weight computed: $sumOfWeights")
  if (sumOfWeights > fone) {
    println(" !!! Round-off error in the DSI structure !!!")
  }

  def toMPFRInterval() = {
    var lo = dsi.head._1.xlo
    var hi = dsi.head._1.xhi
    for (x <- dsi.tail) {
      lo = min(lo, x._1.xlo)
      hi = max(hi, x._1.xhi)
    }
    MPFRInterval(lo, hi)
  }
  //============================================ Unary operations ========================================================

  def unary_-(): MPFRDSInterval = {
    val res = dsi.map { case (i, w) => (i.unary_-, w)}
    reduce(res)
  }

  // multiplication by a constant factor
  def *(r: MPFRFloat): MPFRDSInterval = {
    val res = dsi.map { case (i, w) => (MPFRInterval(r) * i, w)}
    reduce(res)
  }

  def *(r: MPFRInterval): MPFRDSInterval = {
    val res = dsi.map { case (i, w) => (r * i, w)}
    reduce(res)
  }

  def square: MPFRDSInterval = {
    val res = dsi.map { case (i, w) => (i.square, w)}
    reduce(res)
  }

  def sqrt: MPFRDSInterval  = {
    val res = dsi.map { case (i, w) => (i.squareRoot, w)}
    reduce(res)
  }

  def log: MPFRDSInterval = {
    val res = dsi.map { case (i, w) => (i.log, w)}
    reduce(res)
  }


  //========================================== Arithmetic for Independent Variables =====================================

  def i_+(other: MPFRDSInterval): MPFRDSInterval = {
    val res = for ( x <- dsi; y <- other.dsi) //iterating over all possible non zero intervals
      yield (x._1 + y._1, x._2 * y._2)
    reduce(res)
  }
  def i_-(other: MPFRDSInterval): MPFRDSInterval = {
    val res = for ( x <- dsi; y <- other.dsi) //iterating over all possible non zero intervals
      yield (x._1 - y._1, x._2 * y._2)
    reduce(res)
  }
  def i_*(other: MPFRDSInterval): MPFRDSInterval = {
    val res = for ( x <- dsi; y <- other.dsi) //iterating over all possible non zero intervals
      yield (x._1 * y._1, x._2 * y._2)
    reduce(res)
  }
  def i_/(other: MPFRDSInterval): MPFRDSInterval = {
    val res = for ( x <- dsi; y <- other.dsi) //iterating over all possible non zero intervals
      yield (x._1 / y._1, x._2 * y._2)
    reduce(res)
  }

//========================================== Arithmetic for Dependent Variables =====================================


  def d_+(other: MPFRDSInterval, verbose: Boolean = false): MPFRDSInterval = {
    val row = other.dsi.size
    val col = dsi.size
    var intervalMatrix = initializeIntervalMatrix(this, other)
    val d1Weight = initializeWeight(this)
    val d2Weight = initializeWeight(other)
    for (x <- 0 until other.dsi.length ; y <- 0 until dsi.length) {
      intervalMatrix(x)(y) = intervalMatrix(row)(y) + intervalMatrix(x)(col) //Intervals are generated
    }
    val (pBox, timeTakenGenPBox) = time {generatePBox(intervalMatrix, d1Weight, d2Weight)}
    if (verbose) println(s"dep_+: Time for generating P-Box: $timeTakenGenPBox ms")
    val (result, timeTaken) = time {pBoxToDS(pBox._1, pBox._2)}
    if (verbose) println(s"dep_+: Time to convert P-Box to DS: $timeTaken ms")
    result
  }

  def d_-(other: MPFRDSInterval, verbose: Boolean = false): MPFRDSInterval = {
    val row = other.dsi.size
    val col = dsi.size
    var intervalMatrix = initializeIntervalMatrix(this, other)
    val d1Weight = initializeWeight(this)
    val d2Weight = initializeWeight(other)
    for (x <- 0 until other.dsi.length ; y <- 0 until dsi.length) {
      intervalMatrix(x)(y) = intervalMatrix(row)(y) - intervalMatrix(x)(col) //Intervals are generated
    }
    val (pBox, timeTakenGenPBox) = time {generatePBox(intervalMatrix, d1Weight, d2Weight)}
    if (verbose) println(s"dep_+: Time for generating P-Box: $timeTakenGenPBox ms")
    val (result, timeTaken) = time {pBoxToDS(pBox._1, pBox._2)}
    if (verbose) println(s"dep_+: Time to convert P-Box to DS: $timeTaken ms")
    result
  }

  def d_*(other: MPFRDSInterval, verbose: Boolean = false): MPFRDSInterval = {
    val row = other.dsi.size
    val col = dsi.size
    var intervalMatrix = initializeIntervalMatrix(this, other)
    val d1Weight = initializeWeight(this)
    val d2Weight = initializeWeight(other)
    for (x <- 0 until other.dsi.length ; y <- 0 until dsi.length) {
      intervalMatrix(x)(y) = intervalMatrix(row)(y) * intervalMatrix(x)(col) //Intervals are generated
    }
    val (pBox, timeTakenGenPBox) = time {generatePBox(intervalMatrix, d1Weight, d2Weight)}
    if (verbose) println(s"dep_+: Time for generating P-Box: $timeTakenGenPBox ms")
    val (result, timeTaken) = time {pBoxToDS(pBox._1, pBox._2)}
    if (verbose) println(s"dep_+: Time to convert P-Box to DS: $timeTaken ms")
    result
  }

  def d_/(other: MPFRDSInterval, verbose: Boolean = false): MPFRDSInterval = {
    val row = other.dsi.size
    val col = dsi.size
    var intervalMatrix = initializeIntervalMatrix(this, other)
    val d1Weight = initializeWeight(this)
    val d2Weight = initializeWeight(other)
    for (x <- 0 until other.dsi.length ; y <- 0 until dsi.length) {
      intervalMatrix(x)(y) = intervalMatrix(row)(y) / intervalMatrix(x)(col) //Intervals are generated
    }
    val (pBox, timeTakenGenPBox) = time {generatePBox(intervalMatrix, d1Weight, d2Weight)}
    if (verbose) println(s"dep_+: Time for generating P-Box: $timeTakenGenPBox ms")
    val (result, timeTaken) = time {pBoxToDS(pBox._1, pBox._2)}
    if (verbose) println(s"dep_+: Time to convert P-Box to DS: $timeTaken ms")
    result
  }

  def power(n: Integer): MPFRDSInterval = {
    assert(n >= 0, "No negative power allowed") //Works with only poasitive integer power
    val res = dsi.map { case (i, w) => (i.^(n), w)}
    reduce(res)
  }
}
