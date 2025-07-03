package daisy.lang
//import Real._

object Vector {
    def zeroVector(i:Int): Vector = ???
    def flatVector(l: List[List[Real]]): Vector = ???
    def zip(v1: Vector, v2: Vector): Matrix = ???
}

// @ignore
case class Vector(data: List[Real]) {
    // Uncertainty on this vector
    def +/-(x: Real): Boolean = ???
    // specify one range for the whole vector
    def <=(x: Real): Boolean = ???
    def >=(x: Real): Boolean = ???
    // specify range for subset of elements
    def range(i:Int, j: Int)(low: Real, up: Real): Boolean = ???
    def specV(ranges: Set[((Int, Int), (Real, Real))]): Boolean = ???
    def size(i: Int): Boolean = ???

    // element-wise operations
    def +(v: Vector): Vector = ??? // this.zip(v).map(tpl=> tpl._1 + tpl._2)
    def -(v: Vector): Vector = ???
    def *(v: Vector): Vector = ???
    def /(v: Vector): Vector = ???
    // element-wise elementary functions
    def log(): Vector = ??? // base 2   // this.map(x => Math.log(x))
    //def ln(): Vector = ???
    def sin(): Vector = ???
    def cos(): Vector = ???
    def tan(): Vector = ???
    def asin(): Vector = ??? // if x> 1 or x< 1 result is NaN
    def acos(): Vector = ??? // if x> 1 or x< 1 result is NaN
    def atan(): Vector = ???
    def exp(): Vector = ???
    def sqrt(): Vector = ??? // if x < 0 result is NaN
    // cross-product
    def x(v: Vector): Vector = ??? // spell out? (see eval)
    // operations with constants
    def +(c: Real): Vector = ??? // this.map(x => x + c)
    def *(c: Real): Vector = ???
    def /(c: Real): Vector = ???

    // just for type checking
    def toList(): List[Real] = data

    // non-arithmetic operations
    def length(): Int = ??? // Real or INT?
    // TODO does head make sense?
    val head: Real = ???    // this.head
    def at(i: Int): Real = ???      // this(i)
    // subset
    def slice(i: Int, j: Int): Vector = ???   // slice(i, j)
    def everyNth(i: Int, from: Int): Vector = ??? // this.drop(from).grouped(i).map(_.head).toList
    // group and enumerate
    // mapPartitionsWithIndex -> mapSlideWithIndex
    def enumSlideFlatMap(n: Int)(fnc: (Int, Vector) => Vector): Vector = ??? // use if the index is needed, o.w. slide
    //def slide(size:Int, step: Int): List[Vector] = ??? // apply sliding window to the vector, list resulting sub-vectors left to right
    def slideReduce(size:Int, step: Int)(fnc: (Vector) => Real): Vector = ??? // apply sliding window to the vector, map over sub-vectors, record the reduced value
    def enumMap(fnc: (Int, Real) => Real): Vector = ??? // this.zipWithIndex.map(tpl => {val (el, index) = tpl; \lambda })
    def map(fnc: (Real) => Real): Vector = ??? // this.map(arg => f(arg))
    def fold(init: Real)(fnc: (Real,Real) => Real):Real = ???
    def sum():Real = ??? // sum all elements
    def filter(fnc: (Real) => Boolean): Vector = ???

    def pad(i: Int): Vector = ??? // add zeros padding around the vector
    def max(): Real = ???
    def min(): Real = ???

    // concatenate
    def ++(v: Vector): Vector = ???
    def :+(r: Real): Vector = ??? // append
    def +:(r: Real): Vector = ??? // prepend
}


object Matrix {
    def zeroMatrix(i:Int, j:Int): Matrix = ???
}

// @ignore
case class Matrix(data: List[List[Real]]) {
    // Uncertainty on this matrix
    def +/-(x: Real): Boolean = ???
    // specify one range for the whole vector
    def <=(x: Real): Boolean = ???
    def >=(x: Real): Boolean = ???
    // TODO think about a good way to specify subranges for a matrix
    def range(indices: Set[(Int, Int)])(low: Real, up: Real): Boolean = ???
    def specM(ranges: Set[(Set[(Int, Int)], (Real, Real))]): Boolean = ???
    // specify range for an individual matrix element
    def el(i:Int, j: Int)(low: Real, up: Real): Boolean = ???
    def size(i: Int, j: Int): Boolean = ???

    // element-wise operations
    def +(m: Matrix): Matrix = ???
    def -(m: Matrix): Matrix = ???
    def *(m: Matrix): Matrix = ???
    def /(m: Matrix): Matrix = ???
    // element-wise elementary functions
    def log(): Matrix = ??? // base 2
    //def ln(): Matrix = ???
    def sin(): Matrix = ???
    def cos(): Matrix = ???
    def tan(): Matrix = ???
    def asin(): Matrix = ???
    def acos(): Matrix = ???
    def atan(): Matrix = ???
    def ctan(): Matrix = ???
    def exp(): Matrix = ???
    def sqrt(): Matrix = ???

    // operations with constants
    def +(c: Real): Matrix = ???
    def *(c: Real): Matrix = ???
    def /(c: Real): Matrix = ???

    // matrix operations
    def x(v: Vector): Vector = ???
    def x(m: Matrix): Matrix = ???
    // todo: can we do these?
    def determinant(): Real = ???
    def inverse(): Matrix = ???

    // non-arithmetic operations
    def row(i: Int): Vector = ???
    def slice(fromI: Int, fromJ: Int)(toI: Int, toJ: Int): Matrix = ???
    def at(i:Int, j: Int): Real = ???
    def numRows(): Int = ??? // Real?
    def numCols(): Int = ??? // Real?
    def length(): Int = ??? // Real?
    def everyNth(i: Int, from: Int): Matrix = ???
    def enumRowsMap(fnc: (Int, Vector) => Vector): Matrix = ???
    // TODO: figure out whether we need just the slide w/o applying the function afterwards
    //def slide(size:Int, step: Int): List[List[Matrix]] = ??? // apply sliding window to the matrix, list resulting sub-matrices left to right, top to bottom
    def slideReduce(size:Int, step: Int)(fnc: (Matrix) => Real): Matrix = ??? // apply sliding window to the matrix and map over resulting sub-matrices, list resulting sub-matrices left to right, top to bottom

    // operations on rows (vectors)
    def map(fnc: (Vector) => Vector): Matrix = ???
    def fold(init: Vector)(fnc: (Vector,Vector) => Vector): Vector = ???
    def filter(fnc: (Vector) => Boolean): Matrix = ???
    // operations on individual elements
    def mapElements(fnc: (Real) => Real): Matrix = ???
    def foldElements(init: Real)(fnc: (Real,Real) => Real): Real = ???

    def flatten(): Vector = ???
    def transpose(): Matrix = ???
    def flipud(): Matrix = ???
    def fliplr(): Matrix = ???
    def pad(i: Int): Matrix = ??? // add zeros padding around the matrix i x i
    def pad(i: Int, j: Int): Matrix = ??? // add zeros padding around the matrix
    def max(): Real = ???
    def min(): Real = ???

    // concatenate
    def ++(m: Matrix): Matrix = ???
    def :+(v: Vector): Matrix = ??? // append
    def +:(v: Vector): Matrix = ??? // prepend
}
