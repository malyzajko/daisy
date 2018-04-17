// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.annotation.implicitNotFound

@implicitNotFound("No implicit debug section found in scope. " +
  "You need define an implicit DebugSection to use debug/ifDebug")
sealed abstract class DebugSection(val name: String, val mask: Int)

case object DebugSectionFrontend        extends DebugSection("frontend",       1 << 0)
case object DebugSectionSolver          extends DebugSection("solver",         1 << 1)
case object DebugSectionSMTRange        extends DebugSection("smt-range",      1 << 2)
case object DebugSectionAnalysis        extends DebugSection("analysis",       1 << 3)
case object DebugSectionBackend         extends DebugSection("backend",        1 << 4)
case object DebugSectionTaylor          extends DebugSection("taylor",         1 << 5)
case object DebugSectionOptimization    extends DebugSection("opt",            1 << 6)
case object DebugSectionExprOptimizer   extends DebugSection("expr-optimizer", 1 << 7)
case object DebugSectionFitnessEval     extends DebugSection("fitness-eval",   1 << 8)
case object DebugSectionSimplify        extends DebugSection("simplify",       1 << 9)
case object DebugSectionExperiment      extends DebugSection("experiment",     1 << 10)
case object DebugSectionOptimisation    extends DebugSection("optimisation",   1 << 11)
case object DebugSectionTransform       extends DebugSection("transform",      1 << 12)

object DebugSections {
  val all = Set[DebugSection](
    DebugSectionFrontend,
    DebugSectionSolver,
    DebugSectionSMTRange,
    DebugSectionAnalysis,
    DebugSectionBackend,
    DebugSectionTaylor,
    DebugSectionOptimization,
    DebugSectionExprOptimizer,
    DebugSectionFitnessEval,
    DebugSectionSimplify,
    DebugSectionExperiment,
    DebugSectionOptimisation,
    DebugSectionTransform
  )

}
