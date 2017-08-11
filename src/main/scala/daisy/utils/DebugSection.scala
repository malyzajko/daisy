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

object DebugSections {
  val all = Set[DebugSection](
    DebugSectionFrontend,
    DebugSectionSolver,
    DebugSectionSMTRange,
    DebugSectionAnalysis,
    DebugSectionBackend,
    DebugSectionTaylor,
    DebugSectionOptimization
  )

}