// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.collection.immutable.Seq


import lang.Trees._
import tools.{Rational, Interval}
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
object SpecGenerationPhase extends PhaseComponent {
  override val name = "spec gen"
  override val description = "generates specs"
  override def apply(cfg: Config) = new SpecGenerationPhase(cfg, name, "specGen")
}

class SpecGenerationPhase(val cfg: Config, val name: String, val shortName: String) extends DaisyPhase {

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    startRun()

    for (fnc <- functionsToConsider(prg)) {

      val f = fnc.body.get
      cfg.reporter.info("---------------" + fnc.id + "---------------")

      // constraint that body needs to be bigger than 0 (less than 0 would be similar)
      // val bodyNotZeroConstraint = LessThan(RealLiteral(zero), f)

      val notZeroConstraints = Seq(
        LessThan(RealLiteral(zero), f),
        LessThan(f, RealLiteral(zero))
        )

      for (bodyNotZeroConstraint <- notZeroConstraints) {
        cfg.reporter.info("\nbodyNotZeroConstraint: " + bodyNotZeroConstraint)

        var currentModels: Map[Identifier, Seq[Rational]] = freeVariablesOf(f).map({
          case id => (id -> Seq[Rational]())
          }).toMap
        var currentConstraint: Expr = BooleanLiteral(true)

        for (i <- 0 until 5) {
          // cfg.reporter.info("currentModels: " + currentModels)
          // cfg.reporter.info("currentConstraint: " + currentConstraint)

          val solver = new Z3Solver(cfg)
          solver.assertConstraint(bodyNotZeroConstraint)
          solver.assertConstraint(currentConstraint)

          solver.checkSat match {
            case Some(true) =>  // SAT
              val model = solver.getModel
              // cfg.reporter.info("model: " + model)

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
              cfg.reporter.info("constraint UNSAT")

            case _ =>
              // break
              // can't do anything
          }
        }

        cfg.reporter.info("currentModels: " + currentModels)
        cfg.reporter.info("currentConstraint: " + currentConstraint)

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

    finishRun(ctx, prg)
  }


}