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
  def runGenetic(inputExpr: T, fitnessFnc: T => Rational,
    crossoverProb: Double = 0.0): (T, Rational) = {

    val initialFitness = fitnessFnc(inputExpr)

    // initialize population: expr with its fitness value
    var currentPopulation: Seq[(T, Rational)] =
      Seq.fill(populationSize)((inputExpr, initialFitness))

    var globalBest: (T, Rational) = (inputExpr, initialFitness)

    for (i <- 0 until maxGenerations) {
      assert(currentPopulation.size == populationSize)
      //val newPopulation: Seq[(T, Rational)] = (0 until populationSize/2).par.flatMap { _ =>
      val newPopulation: Seq[(T, Rational)] = (0 until populationSize/2).flatMap { _ =>

        // select two individuals from population (2 to enable crossover later)
        val candOne = tournamentSelect(currentPopulation)
        val candTwo = tournamentSelect(currentPopulation)

        // mutate individuals
        var mutantOne = mutate(candOne)
        var mutantTwo = mutate(candTwo)

        // crossover (with a certain probability)
        if (crossoverProb != 0.0 && crossoverProb >= rand.nextDouble) {
          val (m1, m2) = crossover(mutantOne, mutantTwo)
          mutantOne = m1
          mutantTwo = m2
        }


        // evaluate fitness of new candidates
        Seq((mutantOne, fitnessFnc(mutantOne)), (mutantTwo, fitnessFnc(mutantTwo)))

      }.toList

      // update new population
      currentPopulation = newPopulation

      // check if we have found a better expression; compare the fitness values
      globalBest = (globalBest +: currentPopulation).minBy(_._2)
    }

    globalBest
  }

  def tournamentSelect(list: Seq[(T, Rational)]): T = {

    val candidates = rand.shuffle(list).take(tournamentSize)

    // sort by fitness value - smaller is better and pick first
    candidates.minBy(_._2)._1
  }

  def mutate(expr: T): T

  def crossover(expr1: T, expr2: T): (T, T) = ???
}