package daisy.tools

import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees._

object DoubleInterval {

  def maxAbs(i: DoubleInterval): Double = (i.xlo).abs.max((i.xhi).abs)

  def minAbs(i: DoubleInterval): Double = {
    assert(!i.includes(0.0D))
    (i.xlo).abs.min((i.xhi).abs)
  }

  def apply(r: Double): DoubleInterval = DoubleInterval(r, r)
  def apply(i: DoubleInterval): DoubleInterval = i

  def +/-(r: Rational) : DoubleInterval = ???
  def *(r: Rational): DoubleInterval = ???

  val zero: DoubleInterval = DoubleInterval(0D)

  // TODO good precision?
  val pi = DoubleInterval(Math.PI , Math.PI)

  def doubleEvalRange(expr: Expr, _valMap: Map[Identifier, DoubleInterval]): DoubleInterval = {
    var valMap = _valMap

    def eval(e: Expr): DoubleInterval = (e: @unchecked) match {

      case Variable(id) => valMap(id)
      case d: DoubleLiteral => DoubleInterval(d.value)
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

case class PartialDoubleInterval(xlo: Option[Double], xhi: Option[Double])

case class DoubleInterval(xlo: Double, xhi: Double) extends RangeArithmetic[DoubleInterval] {
  assert(xlo <= xhi, "interval lower bound cannot be bigger than upper bound")

  import DoubleInterval.pi

  // private val zero = Rational(0.0)

  // def this(aa: RationalForm) = this(aa.interval._1, aa.interval._2)
  // def this(aa: FixedForm) = this(aa.qInterval._1, aa.qInterval._2)
  // def this(i: Interval) = this(Rational(i.xlo), Rational(i.xhi))


  val mid: Double = xlo/2.0D + xhi/2.0D
  val width: Double = (xhi - xlo).abs
  val radius: Double = width / 2.0D

  def isPointRange: Boolean = xlo == xhi

  // for compatibility with RangeArithmetic
  def toInterval: Interval = ???

  /* Adds an error of magnitude r */
  def +/-(r: Rational): DoubleInterval = ???

  def addConstraint(e: Set[daisy.lang.Trees.Expr]): DoubleInterval = this

  def unary_- = DoubleInterval(-xhi, -xlo)

  def +(other: DoubleInterval): DoubleInterval = {
    DoubleInterval(xlo + other.xlo, xhi + other.xhi)
  }

  def -(other: DoubleInterval): DoubleInterval = {
    DoubleInterval(xlo - other.xhi, xhi - other.xlo)
  }

  def *(y: DoubleInterval): DoubleInterval = y match {
    case DoubleInterval(ylo, yhi) =>
      // TODO: test via unit test that this works and that we need this
      if (xlo == 0.0D && xhi == 0.0D) {
        DoubleInterval(0.0D, 0.0D)
      } else if (xlo >= 0.0D) {
        if (ylo >= 0.0D) {
          DoubleInterval(xlo * ylo, xhi * yhi)
        } else if (yhi <= 0.0D) {
          DoubleInterval(xhi * ylo, xlo * yhi)
        } else {
          DoubleInterval(xhi * ylo, xhi * yhi)
        }
      }
      else if (xhi <= 0.0D) {
        if (ylo >= 0.0D) {
          DoubleInterval(xlo * yhi, xhi * ylo)
        } else if (yhi <= 0.0D) {
          DoubleInterval(xhi * yhi, xlo * ylo)
        } else {
          DoubleInterval(xlo * yhi, xlo * ylo)
        }
      }
      else {
        if (ylo >= 0.0D) {
          DoubleInterval(xlo * yhi, xhi * yhi)
        } else if (yhi <= 0.0D) {
          DoubleInterval(xhi * ylo, xlo * ylo)
        } else {
          val a = (xlo * yhi).min(xhi * ylo)
          val b = (xlo * ylo).max(xhi * yhi)
          DoubleInterval(a, b)
        }
      }
  }

  // the lazy version
  def *(r: Rational): DoubleInterval = ???

  // 0/ 0 is undefined, and will also throw a DivisionByZeroException
  def /(y: DoubleInterval): DoubleInterval = y match {
    case DoubleInterval(ylo, yhi) =>

      // if(xlo == zero && ylo == zero) Interval(zero, zero)

      if (ylo >= 0.0D) {
        if (xlo >= 0.0D) {
          DoubleInterval(xlo / yhi, xhi/ ylo)
        } else if (xhi <= 0.0D) {
          DoubleInterval(xlo / ylo, xhi / yhi)
        } else {
          DoubleInterval(xlo / ylo, xhi / ylo)
        }
      }
      else if (yhi <= 0.0D) {
        if (xlo >= 0.0D) {
          DoubleInterval(xhi / yhi, xlo / ylo)
        } else if (xhi <= 0.0D) {
          DoubleInterval(xhi / ylo, xlo / yhi)
        } else {
          DoubleInterval(xhi / yhi, xlo / yhi)
        }
      } else {
        throw DivisionByZeroException("trying to divide by interval containing 0")
      }

  }

  def ^(n: Int): DoubleInterval = {
    if (n % 2 == 0 && includes(0.0D)) {
      // even powers non-monotonic over - / +
      DoubleInterval(0.0D, Math.pow(DoubleInterval.maxAbs(this), n))
    } else if (n % 2 == 0 && xhi < 0.0D) {
      // even powers monotonically falling over - / -
      DoubleInterval(Math.pow(xhi, n), Math.pow(xlo, n))
    } else {
      // odd powers, and even powers over + / + monotonically rising
      DoubleInterval(Math.pow(xlo, n), Math.pow(xhi, n))
    }
  }

  /*
   * Compares 2 intervals by width
   * @param i2 the second interval to compare with
   * @return
   */
  def >(i2: DoubleInterval): Boolean = {
    this.width > i2.width
  }

  /*
   * Checks whether 2 intervals are equal
   * @param y - interval to compare with
   * @return true/ false
   */
  def equals(y: DoubleInterval): Boolean = {
    this.xlo == y.xlo && this.xhi == y.xhi
  }

  def inverse: DoubleInterval = DoubleInterval(1.0D, 1.0D) / this

  def squareRoot: DoubleInterval = {
    if (xlo < 0.0D) {
      throw NegativeSqrtException("Trying to take the sqrt of a negative number!")
    }
    DoubleInterval(sqrtDown(xlo), sqrtUp(xhi))
  }

  // This computes an approximation of the square root (only), since sqrt(x)
  // is not necessarily a rational number.
  def sqrtUp(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.sqrtUp(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def sqrtDown(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.sqrtDown(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }


  def union(y: DoubleInterval): DoubleInterval = {
    DoubleInterval(this.xlo.min(y.xlo), this.xhi.max(y.xhi))
  }

  def toBigString: String = "[%.18g, %.18g]".format(xlo.toDouble, xhi.toDouble)
  override def toString: String = "[%s, %s]".format(xlo.toDouble, xhi.toDouble)

  // divide the initial interval for two and return the tuple
  def getBinaryDivision: (DoubleInterval, DoubleInterval) = (DoubleInterval(this.xlo, this.mid), DoubleInterval(this.mid, this.xhi))

  /* Divides the interval to predefined number of subintervals
   * @param limit - amount of subintervals to get
   * @return list of subintervals
   */
  def divide(limit: Integer): List[DoubleInterval] = {
    val step = this.width./(limit.toDouble)
    List.tabulate(limit)(n => DoubleInterval(this.xlo +
      (step * (n.toDouble)), this.xlo + (step * ((n + 1).toDouble))))
  }

  /* Extends interval by predefined parameter.
   * E.g. [0].extendBy(0.1)
   * will return interval [-0.1, 0.1]
   * @param parameter positive value for which the bounds of original interval will be changed
   * @return extended interval
   */
  def extendBy(parameter: Double): DoubleInterval =
    DoubleInterval(this.xlo - (parameter), this.xhi + (parameter))

  /** Reduces the Interval bounds to [-pi, pi]
   *
   * Note that the resulting values form not necessarily an interval.
   *
   * It also returns the indices of the quarters of the sine wave in which the
   * end points are.
   */
  //TODO return type changed to Double ok?
  def reduceRange() : ((Double, Double), (Double, Double)) = {
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
    def reduceTo(x: Double, b: Double) = {
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
    val rounded = DoubleInterval(rLo + rLoFact * 2 * piForLo._2, rHi + rHiFact * 2 * piForHi._2)

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
  def sine: DoubleInterval = {
    def mod(a: Double, b: Double): Double = {a % b}

    val ((yloBounded, yhiBounded), (posLo, posHi)) = this.reduceRange()
    if (posHi - posLo >= 4) {
      // the interval is larger than 2 pi -> no information
      DoubleInterval(-1, 1)
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
        case (0, 3) | (2, 1) => DoubleInterval(-1, 1) // both -1 and 1 contained
        case (0, 1) | (0, 2) | (3, 1) | (3, 2) => { // 1, but not -1 contained
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          DoubleInterval(yll.min(ylh).min(yhl.min(yhh)), 1F)
        }
        case (1, 0) | (1, 3) | (2, 0) | (2, 3) => { // -1, but not 1 contained
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          DoubleInterval(-1,(yll.max(ylh)).max(yhl.max(yhh)))
        }
        case _ => { // monotone part
          val (yll, ylh) = sineBounded(yloBounded)
          val (yhl, yhh) = sineBounded(yhiBounded)
          DoubleInterval(((yll.min(ylh)).min(yhl.min(yhh))), (yll.max(ylh)).max(yhl.max(yhh)))
        }
      }
    }
  }

  //Over-approximating implementation of the trigonometric cosine function
  def cosine: DoubleInterval = {
    val conv = (pi / DoubleInterval(2F, 2F)) - this
    conv.sine
  }

  def tangent: DoubleInterval = {
    this.sine / this.cosine
  }

  def arcsine: DoubleInterval = {
    if (xlo < -1.0F || xhi > 1.0F) {
      throw new ArcOutOfBoundsException("Trying to compute arcsine of: " + this)
    }
    DoubleInterval(arcsineDown(xlo), arcsineUp(xhi))
  }

  def arccosine: DoubleInterval = {
    if (xlo < -1.0F || xhi > 1.0F) {
      throw new ArcOutOfBoundsException("Trying to compute arccosine of: " + this)
    }
    DoubleInterval(arccosineDown(xhi), arccosineUp(xlo))   // negative derivative
  }

  def arctangent: DoubleInterval = {
    DoubleInterval(arctanDown(xlo), arctanUp(xhi))
  }

  def exp: DoubleInterval = {
    DoubleInterval(expDown(xlo), expUp(xhi))
  }

  def log: DoubleInterval = {
    if (xlo <= 0.0D) {
      throw NonPositiveLogException("Trying to take the log of a non-positive number!")
    }
    DoubleInterval(logDown(xlo), logUp(xhi))
  }

  def arccosineUp(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.acosUp(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def arccosineDown(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.acosDown(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def arctanUp(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.atanUp(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def arctanDown(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.atanDown(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def expUp(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.expUp(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }
  def expDown(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.expDown(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def arcsineUp(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.asinUp(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def arcsineDown(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.asinDown(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }

  def logUp(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.logUp(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }
  def logDown(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.logDown(MPFRFloat.fromDouble(dblValue)).doubleValue())
    mpfrRes
  }


  @inline
  def includes(that: DoubleInterval): Boolean = this.xlo <= that.xlo && that.xhi <= this.xhi

  @inline
  def includes(r: Double): Boolean = xlo <= r && r <= xhi

  def isNonNegative: Boolean = this.xlo >= 0.0D

  def isPowerOf2: Boolean = ???

  def sineBounded(x: Double): (Double, Double) = {
    (sineDown(x), sineUp(x))
  }

  def sineUp(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextUp(MPFRFloat.sinUp(MPFRFloat.fromDouble(dblValue)).doubleValue)
    mpfrRes
  }

  def sineDown(x: Double): Double = {
    val dblValue = x.doubleValue
    val mpfrRes = java.lang.Math.nextDown(MPFRFloat.sinDown(MPFRFloat.fromDouble(dblValue)).doubleValue)
    mpfrRes
  }
}
