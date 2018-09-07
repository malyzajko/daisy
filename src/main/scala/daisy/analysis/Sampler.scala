// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.util.Random

import lang.Identifiers._
import tools.{Rational, Interval}

import collection.immutable.Map

import tools.StructDS

object Sampler {


  // trait Sample[T] {
  //   def next: Map[Identifier, T]
  // }

  class Uniform(ranges: Map[Identifier, Interval], seed: Long = System.currentTimeMillis) {

    val diameter: Map[Identifier, Double] = ranges.map({
      case (x, Interval(a, b)) => (x -> (Rational.abs(b - a).toDouble))
    })

    val lowerBounds: Map[Identifier, Double] = ranges.map({
      case (x, Interval(a, b)) => (x -> a.toDouble)
    })

    val random = new Random(seed) // TODO: System.millis

    def next: Map[Identifier, Double] = {
      ranges.map({
        case (x, Interval(a, b)) =>
          (x -> (lowerBounds(x) + random.nextDouble * diameter(x)))
      })
    }

    def nextString: Map[Identifier, String] = {
      next.mapValues(_.toString)
    }

  }

  // returns samples which are roughly gaussian with mean being in the middle of the interval
  class Gaussian(ranges: Map[Identifier, Interval], seed: Long = System.currentTimeMillis) {

    val stdDev: Map[Identifier, Double] = ranges.map({
      case (x, Interval(a, b)) => (x -> (Rational.abs(b - a).toDouble)/4.0)
    })

    val mean: Map[Identifier, Double] = ranges.map({
      case (x, Interval(a, b)) => (x -> (a + Rational.abs(b - a)/Rational(2)).toDouble)
    })

    val random = new Random(seed) // TODO: System.millis

    def next: Map[Identifier, Double] = {
      ranges.map({
        case (x, i @ Interval(a, b)) =>
          if (a == b) {
            (x -> a.toDouble)
          } else {
            var found = false
            var sample = 0.0
            while (!found) {
              val r = random.nextGaussian
              sample = mean(x) + stdDev(x) * r
              found = i.includes(Rational.fromDouble(sample))
            }
            (x -> sample)
          }
      })
    }

    def nextString: Map[Identifier, String] = {
      next.mapValues(_.toString)
    }

  }

  class DSSampler(ds: Map[Identifier, StructDS], seed: Long = System.currentTimeMillis) {

    // for each variable, transform the DS structure's weights into intervals
    // and map to the corresponding range's lower bound and diameter
    val probMap: Map[Identifier, List[(Interval, (Double, Double))]] = ds.mapValues ({ dSStruct =>

      // first element: ([0.0, w_1], (lowerBnd, diam))
      val i0 = dSStruct.head._1
      val firstElement = (Interval(Rational.zero, dSStruct.head._2),
        (i0.xlo.toDouble, Rational.abs(i0.xhi - i0.xlo).toDouble))

      dSStruct.tail.foldLeft(List(firstElement))({
        case (list, (Interval(lo, hi), w)) =>
          val currentLowerbound = list.last._1.xhi
          list :+ (Interval(currentLowerbound, currentLowerbound + w),
            (lo.toDouble, Rational.abs(hi - lo).toDouble))
      })
    })

    val random = new Random(seed)

    def nextString: Map[Identifier, String] = {
      probMap.mapValues({ list =>
        val r = Rational.fromString(random.nextDouble.toFloat.toString) // to make this faster
        val (lwrBnd, diam) = list.find(x => x._1.includes(r)).get._2

        (lwrBnd + random.nextDouble * diam).toString

        })
    }

  }
}