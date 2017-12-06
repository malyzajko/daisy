// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package search

import util.Random
import scala.collection.immutable.Seq

import tools.Rational

// The MutationRules should probably not be mixed-in to the generic genetic program
trait GeneticSearch[T] {

  var maxGenerations = 30
  var populationSize = 30
  require(populationSize % 2 == 0)
  val tournamentSize = 4

  var rand: Random


  /** This is a general genetic algorithm.

    @param inputExpr expression to rewrite
    @param fitnessFnc fitness function to guide the search, assumes that smaller is better
    @return AST and fitness value of best expression found)
   */
  def runGenetic(inputExpr: T, fitnessFnc: T => Rational): (T, Rational) = {

    val initialFitness = fitnessFnc(inputExpr)

    // initialize population: expr with its fitness value
    var currentPopulation: Seq[(T, Rational)] =
      Array.fill(populationSize)((inputExpr, initialFitness)).toList

    var globalBest: (T, Rational) = (inputExpr, initialFitness)

    for (i <- 0 until maxGenerations) {
      assert(currentPopulation.size == populationSize)
      val newPopulation: Seq[(T, Rational)] = (0 until populationSize/2).flatMap { _ =>

        // select two individuals from population (2 to enable crossover later)
        val candOne = tournamentSelect(currentPopulation)
        val candTwo = tournamentSelect(currentPopulation)

        // mutate individuals
        val mutantOne = mutate(candOne)
        val mutantTwo = mutate(candTwo)

        // TODO: crossover (with a certain probability)

        // evaluate fitness of new candidates
        Seq((mutantOne, fitnessFnc(mutantOne)), (mutantTwo, fitnessFnc(mutantTwo)))

      }
      // update new population
      currentPopulation = newPopulation

      // check if we have found a better expression; compare the fitness values
      val currentBest = currentPopulation.sortWith(_._2 < _._2).head
      if (currentBest._2 < globalBest._2) {
        globalBest = currentBest
      }
    }

    globalBest
  }

  def tournamentSelect(list: Seq[(T, Rational)]): T = {

    val candidates = rand.shuffle(list).take(tournamentSize)

    // sort by fitness value - smaller is better and pick first
    candidates.sortWith(_._2 < _._2).head._1

  }

  def mutate(expr: T): T
}