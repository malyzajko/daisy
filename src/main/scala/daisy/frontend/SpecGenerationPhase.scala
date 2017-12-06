// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import scala.collection.immutable.Seq

import lang.Trees._
import tools.Rational
import Rational._
import solvers.Z3Solver
import lang.TreeOps._
import lang.Identifiers._

/**
  Generates specifications (require's for now)
  This is completely ad-hoc.

  Prerequisites:
    -
 */
object SpecGenerationPhase extends DaisyPhase {
  override val name = "spec gen"
  override val shortName = "specGen"
  override val description = "generates specs"

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    for (fnc <- functionsToConsider(ctx, prg)) {

      val f = fnc.body.get
      ctx.reporter.info("---------------" + fnc.id + "---------------")

      // constraint that body needs to be bigger than 0 (less than 0 would be similar)
      // val bodyNotZeroConstraint = LessThan(RealLiteral(zero), f)

      val notZeroConstraints = Seq(
        LessThan(RealLiteral(zero), f),
        LessThan(f, RealLiteral(zero))
        )

      for (bodyNotZeroConstraint <- notZeroConstraints) {
        ctx.reporter.info("\nbodyNotZeroConstraint: " + bodyNotZeroConstraint)

        var currentModels: Map[Identifier, Seq[Rational]] = freeVariablesOf(f).map({
          case id => (id -> Seq[Rational]())
          }).toMap
        var currentConstraint: Expr = BooleanLiteral(true)

        for (i <- 0 until 5) {
          // ctx.reporter.info("currentModels: " + currentModels)
          // ctx.reporter.info("currentConstraint: " + currentConstraint)

          val solver = new Z3Solver(ctx)
          solver.assertConstraint(bodyNotZeroConstraint)
          solver.assertConstraint(currentConstraint)

          solver.checkSat match {
            case Some(true) =>  // SAT
              val model = solver.getModel
              // ctx.reporter.info("model: " + model)

              currentModels = currentModels.map({
                case (id, seq) =>
                  (model(id): @unchecked) match {
                    case x @ RealLiteral(r) =>
                      currentConstraint = And(currentConstraint,
                        Not(Equals(Variable(id), x)))
                      (id, seq :+ r)
                  }
                }).toMap

            case Some(false) =>
              ctx.reporter.info("constraint UNSAT")

            case _ =>
              // break
              // can't do anything
          }
        }

        ctx.reporter.info("currentModels: " + currentModels)
        ctx.reporter.info("currentConstraint: " + currentConstraint)

      }
      // if the above was successful, i.e. we have more than two models,
      // we can try to generalize, by taking the union
      /* if (currentModels.values.forall(_.length > 1)) {
        val require = currentModels.map({
          case (id, seq) =>
            val union = seq.tail.fold(Interval(seq.head))({
              (i, r) => i.union(Interval(r))
              })
          })
      } */

    }

    (ctx, prg)
  }


}