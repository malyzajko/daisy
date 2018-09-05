package daisy
package tools

import org.apache.commons.math3.special.Erf._
import scala.collection.mutable.ListBuffer
import Rational._
import scala.math._


object DistributionGenerators {

  def generateUniform (lowerBound: Rational, upperBound: Rational, prec: Int): DSInterval = {
    assert (prec > 0)
    val step = (upperBound - lowerBound) / prec
    val weight = Rational.fromReal(1.0) / prec
    var z = new ListBuffer[(Interval, Rational)]()
    for (i <- 0 to prec-1) {
      val intervalLo = lowerBound + i * step
      val intervalHi = lowerBound + (i + 1) * step
      z += ((Interval(intervalLo, intervalHi), weight))
    }
    DSInterval(z.toList)
  }

  def generateStandardNormalCase1 (inf: Rational, sup: Rational, prec: Int) : DSInterval = {
    assert (prec > 0)

    var z = new ListBuffer[(Interval, Rational)]()
    val xx = Array.ofDim[Rational](prec+1)
    val w = Array.ofDim[Rational](prec+1)
    val boundOfI = Array.ofDim[Rational](prec)
    val boundOfS = Array.ofDim[Rational](prec)
    val prob = Array.ofDim[Rational](prec)
    var sum = Rational.zero
    val step = (sup - inf) / prec
    //val sqrtOfTwoPi = Rational.fromDouble(sqrt(2 * 3.14159265 * 1))
    val sqrtOfTwoPi = Rational.fromDouble(sqrt(2 * Pi * 1))
    val power = Rational.fromReal(-0.5)
    var totalWeight = Rational.zero
    for (i <- 0 until prec+1) {
      xx(i) = inf + i * step
    }
    for (i <- 0 until prec+1) {
      w(i) = 1/sqrtOfTwoPi * Rational.fromReal(exp ((power * xx(i).square).toDouble)) // Too many conversions!!
    }
    for (i <- 0 until prec) {
      boundOfI(i) = xx(i)
      boundOfS(i) = xx(i+1)
      prob(i) = (w(i) + w(i+1)) / 2
      sum += prob(i)
    }
    for (i <- 0 until prec) {
      prob(i) = prob(i) / sum
    }
    for (i <- 0 until prec) {
      totalWeight += prob(i)
      z += ((Interval(boundOfI(i), boundOfS(i)), prob(i)))
    }
    DSInterval(z.toList)
  }

  def generateStandardNormalCase2 (inf: Rational, sup: Rational, prec: Int) : DSInterval = {
    assert (prec > 0)

    var z = new ListBuffer[(Interval, Rational)]()
    val xx = Array.ofDim[Rational](prec+1)
    val w = Array.ofDim[Rational](prec+1)
    val boundOfI = Array.ofDim[Rational](prec)
    val mean = Array.ofDim[Rational](prec)
    val boundOfS = Array.ofDim[Rational](prec)
    val prob = Array.ofDim[Rational](prec)
    var sum = Rational.zero
    val step = (sup - inf) / prec
    //val sqrtOfTwoPi = Rational.fromDouble(sqrt(2 * 3.14159265 * 1))
    val sqrtOfTwoPi = Rational.fromDouble(sqrt(2 * Pi * 1))
    val power = Rational.fromReal(-0.5)
    var totalWeight = Rational.zero
    for (i <- 0 until prec+1) {
      xx(i) = inf + i * step
    }
    for (i <- 0 until prec) {
      mean(i) = (xx(i) + xx(i+1)) / 2
      w(i) = 1/sqrtOfTwoPi * Rational.fromReal(exp ((power * mean(i).square).toDouble)) // Too many conversions!!
      sum += w(i)
    }
    for (i <- 0 until prec) {
      w(i) = w(i) / sum
    }
    for (i <- 0 until prec) {
      boundOfI(i) = xx(i)
      boundOfS(i) = xx(i+1)
    }
    for (i <- 0 until prec) {
      totalWeight += w(i)
      z += ((Interval(boundOfI(i), boundOfS(i)), w(i)))
    }
    DSInterval(z.toList)
  }



  def generateNormal (inf: Rational, sup: Rational, mean: Interval, variance: Interval, prec: Int) : DSInterval = {
    assert (prec > 0)
    val xxPl = Array.ofDim[Rational](prec)
    val pl = Array.ofDim[Rational](prec)
    val xxBel = Array.ofDim[Rational](prec)
    val bel = Array.ofDim[Rational](prec)
    val xx = Array.ofDim[Rational](prec+1)
    val sqrtOfTwo = Rational.fromDouble(sqrt(2))
    val step = (sup - inf) / prec
    for (i <- 0 until prec+1) {
      xx(i) = inf + i * step
    }
    for (i <- 0 until prec) {
      val ix = (-mean + Interval(xx(i))) / (variance * Interval(sqrtOfTwo)) // TODO: Can we use [sqrtDown, sqrtUp] as an interval here??
      if (i < prec) {
        bel(i) = Rational.fromDouble(1.0/2 * (1 + erf((ix.xlo).toDouble)))
        xxBel(i) = xx(i+1)
      }
      if (i > 0) {
        pl(i-1) = Rational.fromDouble(1.0/2 * (1 + erf((ix.xhi).toDouble)))
        xxPl(i-1) = xx(i-1)
      }
    }
    val (belFunc, plFunc) = generateCDF(xxPl, pl, xxBel, bel, prec)
    generateIDF(belFunc, plFunc)
  }

  def generateCDF (xxPl: Array[Rational], pl: Array[Rational], xxBel: Array[Rational], bel: Array[Rational], prec: Int ) : (Belief, Plausibility) = {
    var belFunc = Array.ofDim[(Rational, Rational)](prec)
    var plFunc = Array.ofDim[(Rational, Rational)](prec)
    for (i <- 0 until prec) {
      plFunc(i) = ((xxPl(i), pl(i)))
      belFunc(i) = ((xxBel(i), bel(i)))
    }
    (belFunc, plFunc)
  }

  def generateIDF (bel: Belief, pl: Plausibility) : DSInterval = {
    var z = new ListBuffer[(Interval, Rational)]()
    var iBel = 0
    var iPl = 0
    var prob = Rational.zero
    var inf = Rational.zero
    var sup = Rational.zero
    var p = Rational.zero
    bel.sortWith(_._1 < _._1)
    pl.sortWith(_._1 < _._1)
    val sizePl = pl.size
    val sizeBel = bel.size
    while ((iPl < sizePl) && (iBel < sizeBel)) {
      if (pl(iPl)._2 < bel(iBel)._2) {
        inf = pl(iPl)._1;
        sup = bel(iBel)._1;
        p = pl(iPl)._2 - prob;
        prob = pl(iPl)._2;
        iPl = iPl + 1;
      } else if (pl(iPl)._2 > bel(iBel)._2) {
        inf = pl(iPl)._1;
        sup = bel(iBel)._1;
        p = bel(iBel)._2 - prob;
        prob = bel(iBel)._2;
        iBel = iBel + 1;
      } else {
        inf = pl(iPl)._1;
        sup = bel(iBel)._1;
        p = bel(iBel)._2 - prob;
        prob = bel(iBel)._2;
        iBel = iBel + 1;
        iPl = iPl + 1;
      } 
      z += ((Interval(inf, sup), p))
    }
    while (iPl < sizePl) {
      inf = pl(iPl)._1;
      sup = bel(iBel-1)._1;
      p = pl(iPl)._2 - prob;
      prob = pl(iPl)._2;
      iPl = iPl + 1;
      z += ((Interval(inf, sup), p));
    }
    while (iBel < sizeBel) {
      inf = pl(iPl-1)._1;
      sup = bel(iBel)._1;
      p = bel(iBel)._2 - prob;
      prob = bel(iBel)._2;
      iBel = iBel + 1;
      z += ((Interval(inf, sup), p));
    }
    DSInterval(z.toList)
  }
}
