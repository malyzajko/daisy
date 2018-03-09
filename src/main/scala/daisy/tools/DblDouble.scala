// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.math.ScalaNumber

import scala.language.implicitConversions
import scala.annotation.strictfp

@strictfp
object DblDouble {

  implicit def int2DblDouble(i: Int): DblDouble = new DblDouble(i.toDouble)
  implicit def double2DblDouble(d: Double): DblDouble = new DblDouble(d)

  def apply(d: Double): DblDouble = new DblDouble(d)

  val zero = new DblDouble(0.0, 0.0)

  def sqrt(x: DblDouble): DblDouble = x.squareRoot

  def abs(x: DblDouble): DblDouble = if (x.hi < 0.0) -x else x
  def max(x: DblDouble, y: DblDouble): DblDouble = if (x > y) x else y
  def min(x: DblDouble, y: DblDouble): DblDouble = if (x < y) x else y


}

/**
 * Implements a floating-point data type with approximately double the
 * precision of a regular double-precision float. The name is DblDouble
 * to avoid clashes with the DoubleDouble precision in FinitePrecision.
 *
 * This is a translation from the Java version by M. Davis. DblDouble.java.
 * http://tsusiatsoftware.net/dd/main.html.
 * The algorithms used here are from D. M. Priest.
 * Algorithms for Arbitrary Precision Floating
 * Point Arithmetic. In Proceedings of the 10th Symposium on
 * Computer Arithmetic, 1991.
 * Implementation is according to M. Davis. DblDouble.java.
 * http://tsusiatsoftware.net/dd/main.html.
 */
@strictfp
class DblDouble(val hi: Double, val lo: Double) extends ScalaNumber with Ordered[DblDouble] {
  import DblDouble._

  def this(d: Double) = this(d, 0.0)

  def +(y: DblDouble): DblDouble = { // new DblDouble(DDouble.add(x0, x1, y.x0, y.x1))
    val xhi = this.hi; val xlo = this.lo; val yhi = y.hi; val ylo = y.lo
    var H, h, T, t, S, s, e, f = 0.0;
    S = xhi + yhi;
    T = xlo + ylo;
    e = S - xhi;
    f = T - xlo;
    s = S - e;
    t = T - f;
    s = (yhi - e) + (xhi - s);
    t = (ylo - f) + (xlo - t);
    e = s + T; H = S + e; h = e + (S - H); e = t + h;

    val zhi = H + e;
    new DblDouble(H + e, e + (H - zhi))

  }

  def -(y: DblDouble): DblDouble = { // new DblDouble(DDouble.sub(x0, x1, y.x0, y.x1))
    val xhi = this.hi; val xlo = this.lo;; val yhi = -y.hi; val ylo = -y.lo
    var H, h, T, t, S, s, e, f = 0.0;
    S = xhi + yhi;
    T = xlo + ylo;
    e = S - xhi;
    f = T - xlo;
    s = S - e;
    t = T - f;
    s = (yhi - e) + (xhi - s);
    t = (ylo - f) + (xlo - t);
    e = s + T; H = S + e; h = e + (S - H); e = t + h;

    val zhi = H + e;
    new DblDouble(H + e, e + (H - zhi))
  }

  /**
   * The value to split a double-precision value on during multiplication
   */
  private val SPLIT = 134217729.0D; // 2^27+1, for IEEE double

  def *(y: DblDouble): DblDouble = {// new DblDouble(DDouble.mult(x0, x1, y.x0, y.x1))
    val xhi = this.hi; val xlo = this.lo; val yhi = y.hi; val ylo = y.lo
    var hx, tx, hy, ty, C, c = 0.0;
    C = SPLIT * xhi;
    hx = C - xhi;
    c = SPLIT * yhi;
    hx = C - hx;
    tx = xhi - hx;
    hy = c - yhi;
    C = xhi * yhi;
    hy = c - hy;
    ty = yhi - hy;
    c = ((((hx * hy - C) + hx * ty) + tx * hy) + tx * ty) + (xhi * ylo + xlo * yhi);
    val zhi = C + c; hx = C - zhi;
    new DblDouble(zhi, c + hx)
  }

  def /(y: DblDouble): DblDouble = { // new DblDouble(DDouble.div(x0, x1, y.x0, y.x1))
    val xhi = this.hi; val xlo = this.lo; val yhi = y.hi; val ylo = y.lo
    var hc, tc, hy, ty, C, c, U, u = 0.0;
    C = xhi / yhi;
    c = SPLIT * C;
    hc =c - C;
    u = SPLIT * yhi;
    hc = c - hc;
    tc = C - hc;
    hy = u - yhi;
    U = C * yhi;
    hy = u - hy;
    ty = yhi - hy;
    u = (((hc * hy - U) + hc * ty) + tc * hy) + tc * ty;
    c = ((((xhi - U) - u) + xlo) - C * ylo) / yhi;
    u = C + c;

    new DblDouble(u, (C - u) + c)

  }

  def squareRoot: DblDouble = {
    // new DblDouble(DDouble.sqrt(x.x0, x.x1))
    /* [comment from DblDouble.java] Strategy:  Use Karp's trick:  if x is an approximation
    to sqrt(a), then sqrt(a) = a*x + [a - (a*x)^2] * x / 2   (approx)
    The approximation is accurate to twice the accuracy of x.
    Also, the multiplication (a*x) and [-]*x can be done with only half the precision.
     */

    if (hi == 0.0 && lo == 0.0) return zero;

    if (hi < 0.0 || (hi == 0.0 && lo < 0.0)) return new DblDouble(Double.NaN, Double.NaN);

    val x = 1.0 / scala.math.sqrt(hi);
    val ax = hi * x;

    val axdd = DblDouble(ax)

    val axddSqr = axdd * axdd
    val diffSq = x - axddSqr
    val d2 = diffSq.hi * (x * 0.5)

    axdd + DblDouble(d2)
  }

  def %(y: DblDouble): DblDouble = this - (y * (this/y).toInt)

  def unary_-(): DblDouble = new DblDouble(-hi, -lo)

  def isNaN: Boolean = hi != hi || lo != lo

  override def equals(other: Any): Boolean = other match {
    case x: DblDouble => this.compare(x) == 0
    case x: Double => this.compare(x) == 0
    case x: Short => this.compare(x) == 0
    case x: Char => this.compare(x) == 0
    case x: Byte => this.compare(x) == 0
    case x: Int => this.compare(x) == 0
    case x: Float => this.compare(x) == 0
    case x: Long => this.compare(x) == 0
    case _ => false
  }

  override def hashCode(): Int = hi.hashCode

  override def toString: String = (hi + lo).toString

  // TODO: def toLongString(digits: Int = 30): String = DDouble.toString(digits, x0, x1, x2, x3)

  def compare(y: DblDouble): Int = {
    if (hi < y.hi) {
      -1
    } else if (hi > y.hi) {
      1
    } else {
      if (lo < y.lo) {
        -1
      } else if (lo > y.lo) {
        1
      } else {
        0
      }
    }
  }

  override def byteValue(): Byte = Predef.double2Double(hi).byteValue
  override def doubleValue(): Double = (hi + lo)
  override def floatValue(): Float = Predef.double2Double(hi).floatValue
  override def intValue(): Int = Predef.double2Double(hi).intValue
  override def longValue(): Long = Predef.double2Double(hi).longValue
  override def shortValue(): Short = Predef.double2Double(hi).shortValue
  def toByte: Byte = hi.toByte
  def toChar: Char = hi.toChar
  def toDouble: Double = (hi + lo)
  def toFloat: Float = hi.toFloat
  def toInt: Int = hi.toInt
  def toLong: Long = hi.toLong
  def toShort: Short = hi.toShort

  def underlying(): AnyRef = this
  override def isWhole(): Boolean = hi % 1.0 == 0.0 && lo == 0.0

}

