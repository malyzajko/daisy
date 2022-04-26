// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.tools

import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees._


object FloatInterval {

  def maxAbs(i: FloatInterval): Float = (i.xlo).abs.max((i.xhi).abs)

  def minAbs(i: FloatInterval): Float = {
    assert(!i.includes(0.0F))
    (i.xlo).abs.min((i.xhi).abs)
  }

  def apply(r: Float): FloatInterval = FloatInterval(r, r)
  def apply(i: FloatInterval): FloatInterval = i

  def +/-(r: Rational) : FloatInterval = ???
  def *(r: Rational): FloatInterval = ???

  val zero: FloatInterval = FloatInterval(0F)

  // TODO good precision?
  val pi = FloatInterval(Math.PI.toFloat , Math.PI.toFloat)

  def floatEvalRange(expr: Expr, _valMap: Map[Identifier, FloatInterval]): FloatInterval = {
    var valMap = _valMap

    def eval(e: Expr): FloatInterval = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case f: FloatLiteral => FloatInterval(f.value)
      case Plus(x, y) => eval(x) + eval(y)
      case Minus(x, y) => eval(x) - eval(y)
      case Times(x, y) => eval(x) * eval(y)
      case Division(x, y) => eval(x) / eval(y)
      case IntPow(x, y) => eval(x) ^ y //TODO correct?
      case UMinus(x) => - eval(x)
      case Sqrt(x) => eval(x).squareRoot
      case Sin(x) => eval(x).sine
      case Cos(x) => eval(x).cosine
      case Tan(x) => eval(x).tangent
      case Asin(x) => eval(x).arcsine
      case Acos(x) => eval(x).arccosine
      case Atan(x) => eval(x).arctangent
      case Exp(x) => eval(x).exp
      case Log(x) => eval(x).log
      case Let(id, v, b) =>
        valMap += (id -> eval(v))
        eval(b)

    }
    eval(expr)
  }
}

case class PartialFloatInterval(xlo: Option[Float], xhi: Option[Float])

case class FloatInterval(xlo: Float, xhi: Float) extends RangeArithmetic[FloatInterval] {
  assert(xlo <= xhi, "interval lower bound cannot be bigger than upper bound")

  import FloatInterval.pi

  // private val zero = Rational(0.0)

  // def this(aa: RationalForm) = this(aa.interval._1, aa.interval._2)
  // def this(aa: FixedForm) = this(aa.qInterval._1, aa.qInterval._2)
  // def this(i: Interval) = this(Rational(i.xlo), Rational(i.xhi))


  val mid: Float = xlo/2.0F + xhi/2.0F
  val width: Float = (xhi - xlo).abs
  val radius: Float = width / 2.0F

  def isPointRange: Boolean = xlo == xhi

  // for compatibility with RangeArithmetic
  def toInterval: Interval = ???

  /* Adds an error of magnitude r */
  def +/-(r: Rational): FloatInterval = ???

  def addConstraint(e: Set[daisy.lang.Trees.Expr]): FloatInterval = this

  def unary_- = FloatInterval(-xhi, -xlo)

  def +(other: FloatInterval): FloatInterval = {
    FloatInterval(xlo + other.xlo, xhi + other.xhi)
  }

  def -(other: FloatInterval): FloatInterval = {
    FloatInterval(xlo - other.xhi, xhi - other.xlo)
  }

  def *(y: FloatInterval): FloatInterval = y match {
    case FloatInterval(ylo, yhi) =>
      // TODO: test via unit test that this works and that we need this
      if (xlo == 0.0F && xhi == 0.0F) {
        FloatInterval(0.0F, 0.0F)
      } else if (xlo >= 0.0F) {
        if (ylo >= 0.0F) {
          FloatInterval(xlo * ylo, xhi * yhi)
        } else if (yhi <= 0.0F) {
          FloatInterval(xhi * ylo, xlo * yhi)
        } else {
          FloatInterval(xhi * ylo, xhi * yhi)
        }
      }
      else if (xhi <= 0.0F) {
        if (ylo >= 0.0F) {
          FloatInterval(xlo * yhi, xhi * ylo)
        } else if (yhi <= 0.0F) {
          FloatInterval(xhi * yhi, xlo * ylo)
        } else {
          FloatInterval(xlo * yhi, xlo * ylo)
        }
      }
      else {
        if (ylo >= 0.0F) {
          FloatInterval(xlo * yhi, xhi * yhi)
        } else if (yhi <= 0.0F) {
          FloatInterval(xhi * ylo, xlo * ylo)
        } else {
          val a = (xlo * yhi).min(xhi * ylo)
          val b = (xlo * ylo).max(xhi * yhi)
          FloatInterval(a, b)
        }
      }
  }

  // the lazy version
  def *(r: Rational): FloatInterval = ???

  // 0/ 0 is undefined, and will also throw a DivisionByZeroException
  def /(y: FloatInterval): FloatInterval = y match {
    case FloatInterval(ylo, yhi) =>

      // if(xlo == zero && ylo == zero) Interval(zero, zero)

      if (ylo >= 0.0F) {
        if (xlo >= 0.0F) {
          FloatInterval(xlo / yhi, xhi/ ylo)
        } else if (xhi <= 0.0F) {
          FloatInterval(xlo / ylo, xhi / yhi)
        } else {
          FloatInterval(xlo / ylo, xhi / ylo)
        }
      }
      else if (yhi <= 0.0F) {
        if (xlo >= 0.0F) {
          FloatInterval(xhi / yhi, xlo / ylo)
        } else if (xhi <= 0.0F) {
          FloatInterval(xhi / ylo, xlo / yhi)
        } else {
          FloatInterval(xhi / yhi, xlo / yhi)
        }
      } else {
        throw DivisionByZeroException("trying to divide by interval containing 0")
      }

  }

  def ^(n: Int): FloatInterval = {
    if (n % 2 == 0 && includes(0.0F)) {
      // even powers non-monotonic over - / +
      FloatInterval(0.0F, Math.pow(FloatInterval.maxAbs(this), n).toFloat)
    } else if (n % 2 == 0 && xhi < 0.0F) {
      // even powers monotonically falling over - / -
      FloatInterval(Math.pow(xhi, n).toFloat, Math.pow(xlo, n).toFloat)
    } else {
      // odd powers, and even powers over + / + monotonically rising
      FloatInterval(Math.pow(xlo, n).toFloat, Math.pow(xhi, n).toFloat)
    }
  }

  /*
   * Compares 2 intervals by width
   * @param i2 the second interval to compare with
   * @return
   */
  def >(i2: FloatInterval): Boolean = {
    this.width > i2.width
  }

  /*
   * Checks whether 2 intervals are equal
   * @param y - interval to compare with
   * @return true/ false
   */
  def equals(y: FloatInterval): Boolean = {
    this.xlo == y.xlo && this.xhi == y.xhi
  }

  def inverse: FloatInterval = FloatInterval(1.0F, 1.0F) / this

  def squareRoot: FloatInterval = {
    if (xlo < 0.0F) {
      throw NegativeSqrtException("Trying to take the sqrt of a negative number!")
    }
    FloatInterval(sqrtDown(xlo), sqrtUp(xhi))
  }

  // This computes an approximation of the square root (only), since sqrt(x)
  // is not necessarily a rational number.
  def sqrtUp(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.sqrtUp(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def sqrtDown(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.sqrtDown(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }


  def union(y: FloatInterval): FloatInterval = {
    FloatInterval(this.xlo.min(y.xlo), this.xhi.max(y.xhi))
  }

  def toBigString: String = "[%.18g, %.18g]".format(xlo.toDouble, xhi.toDouble)
  override def toString: String = "[%s, %s]".format(xlo.toDouble, xhi.toDouble)

  // divide the initial interval for two and return the tuple
  def getBinaryDivision: (FloatInterval, FloatInterval) = (FloatInterval(this.xlo, this.mid), FloatInterval(this.mid, this.xhi))

   /* Divides the interval to predefined number of subintervals
    * @param limit - amount of subintervals to get
    * @return list of subintervals
    */
  def divide(limit: Integer): List[FloatInterval] = {
    val step = this.width./(limit.toFloat)
    List.tabulate(limit)(n => FloatInterval(this.xlo +
      (step * (n.toFloat)), this.xlo + (step * ((n + 1).toFloat))))
  }

   /* Extends interval by predefined parameter.
    * E.g. [0].extendBy(0.1)
    * will return interval [-0.1, 0.1]
    * @param parameter positive value for which the bounds of original interval will be changed
    * @return extended interval
    */
  def extendBy(parameter: Float): FloatInterval =
    FloatInterval(this.xlo - (parameter), this.xhi + (parameter))

   /** Reduces the Interval bounds to [-pi, pi]
    *
    * Note that the resulting values form not necessarily an interval.
    *
    * It also returns the indices of the quarters of the sine wave in which the
    * end points are.
    */
   //TODO return type changed to Float ok?
  def reduceRange() : ((Float, Float), (Float, Float)) = {
    // lower bound >= 0 -> subtract over-approximation of pi...
    // lower bound < 0 -> subtract under-approximation of pi...
    // ... and for the widened interval add the opposite one
    val piForLo = if (xlo >= 0.0) (pi.xhi, pi.xlo) else (pi.xlo, pi.xhi)

    // upper bound >= 0 -> subtract under-approximation of pi...
    // upper bound < 0 -> subtract over-approximation of pi...
    // ... and for the widened interval add the opposite one
    val piForHi = if (xhi >= 0.0) (pi.xlo, pi.xhi) else (pi.xhi, pi.xlo)

    // Returns the representative y of x in [-b, b] and the number of times 2*b
    // has been subtracted from x to obtain y.
    // TODO this becomes inaccurate for intervals that are far from 0 because of
    // the imprecision of the rational pi
    def reduceTo(x: Float, b: Float) = {
      val k = x / b
      val sign = if (k >= 0) 1 else -1
      val l = (k + sign) / 2
      val res = x - l * 2 * b
      (res, l)
    }

    val (rLo, rLoFact) = reduceTo(xlo, piForLo._1)
    val (rHi, rHiFact) = reduceTo(xhi, piForHi._1)

    // find an over-approximation of any interval that could lead to the
    // resulting values with the given uncertainty of pi
    val rounded = FloatInterval(rLo + rLoFact * 2 * piForLo._2, rHi + rHiFact * 2 * piForHi._2)

    val piLo = if (rounded.xlo >= 0.0) pi.xhi else pi.xlo
    val piHi = if (rounded.xhi >= 0.0) pi.xlo else pi.xhi

    // computes n such that n*(pi/2) <= rounded.x__ <= (n+1)*(pi/2)
    var posLo = (rounded.xlo * 2) / piLo
    if (rounded.xlo < 0) posLo -= 1
    var posHi = (rounded.xhi * 2) / piHi
    if (rounded.xhi < 0) posHi -= 1
    // has to be based on rounded (instead of this) as the imprecision of pi can
    // make the bounded values be in different quarters of the sine wave as they
    // should be, resulting in unsound results if not properly handled

    ((rLo, rHi), (posLo, posHi))
  }

  //Over-approximating implementation of the trigonometric sine function
  def sine: FloatInterval = {
    def mod(a: Float, b: Float): Float = {a % b}

    val ((yloBounded, yhiBounded), (posLo, posHi)) = this.reduceRange()
    if (posHi - posLo >= 4) {
      // the interval is larger than 2 pi -> no information
      FloatInterval(-1, 1)
    } else {
      // In which quarters of the sine wave are we?
      //
      // |  _|_  |   |   |
      // | / | \ |   |   |
      // |/  |  \|   |   |
      // |   |   |   |   |
      // |   |   |\  |  /|
      // |   |   | \_|_/ |
      // |   |   |   |   |
      //   0   1   2   3
      //
      (mod(posLo, 4).toInt, mod(posHi, 4).toInt) match {
        case (0, 3) | (2, 1) => FloatInterval(-1, 1) // both -1 and 1 contained
        case (0, 1) | (0, 2) | (3, 1) | (3, 2) => { // 1, but not -1 contained
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          FloatInterval(yll.min(ylh).min(yhl.min(yhh)), 1F)
        }
        case (1, 0) | (1, 3) | (2, 0) | (2, 3) => { // -1, but not 1 contained
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          FloatInterval(-1,(yll.max(ylh)).max(yhl.max(yhh)))
        }
        case _ => { // monotone part
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          FloatInterval(((yll.min(ylh)).min(yhl.min(yhh))), (yll.max(ylh)).max(yhl.max(yhh)))
        }
      }
    }
  }

  //Over-approximating implementation of the trigonometric cosine function
  def cosine: FloatInterval = {
    val conv = (pi / FloatInterval(2F, 2F)) - this
    conv.sine
  }

  def tangent: FloatInterval = {
    this.sine / this.cosine
  }

  def arcsine: FloatInterval = {
    if (xlo < -1.0F || xhi > 1.0F) {
      throw new ArcOutOfBoundsException("Trying to compute arcsine of: " + this)
    }
    FloatInterval(arcsineDown(xlo), arcsineUp(xhi))
  }

  def arccosine: FloatInterval = {
    if (xlo < -1.0F || xhi > 1.0F) {
      throw new ArcOutOfBoundsException("Trying to compute arccosine of: " + this)
    }
    FloatInterval(arccosineDown(xhi), arccosineUp(xlo))   // negative derivative
  }

  def arctangent: FloatInterval = {
    FloatInterval(arctanDown(xlo), arctanUp(xhi))
  }

  def exp: FloatInterval = {
    FloatInterval(expDown(xlo), expUp(xhi))
  }

  def log: FloatInterval = {
    if (xlo <= 0.0F) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }
    FloatInterval(logDown(xlo), logUp(xhi))
  }

  def arccosineUp(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.acosUp(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def arccosineDown(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.acosDown(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def arctanUp(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.atanUp(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def arctanDown(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.atanDown(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def expUp(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.expUp(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }
  def expDown(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.expDown(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def arcsineUp(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.asinUp(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def arcsineDown(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.asinDown(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }

  def logUp(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.logUp(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }
  def logDown(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.logDown(MPFRFloat.fromDouble(dblValue)).floatValue())
    mpfrRes
  }


  @inline
  def includes(that: FloatInterval): Boolean = this.xlo <= that.xlo && that.xhi <= this.xhi

  @inline
  def includes(r: Float): Boolean = xlo <= r && r <= xhi

  def isNonNegative: Boolean = this.xlo >= 0.0F

  def isPowerOf2: Boolean = ???

  def sineBounded(x: Float): (Float, Float) = {
    (sineDown(x), sineUp(x))
  }

  def sineUp(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.sinUp(MPFRFloat.fromDouble(dblValue)).doubleValue)
    mpfrRes.toFloat
  }

  def sineDown(x: Float): Float = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.sinDown(MPFRFloat.fromDouble(dblValue)).doubleValue)
    mpfrRes.toFloat
  }

}

