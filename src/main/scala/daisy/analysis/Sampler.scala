// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.util.Random

import lang.Identifiers._
import tools.{Rational, Interval}

import collection.immutable.Map

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
      ranges.map({
        case (x, Interval(a, b)) =>
          (x -> (lowerBounds(x) + random.nextDouble * diameter(x)).toString)
      })
    }

  }
}