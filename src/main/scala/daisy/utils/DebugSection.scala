
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy

import scala.annotation.implicitNotFound

@implicitNotFound("No implicit debug section found in scope. You need define an implicit DebugSection to use debug/ifDebug")
sealed abstract class DebugSection(val name: String, val mask: Int)

case object DebugSectionFrontend        extends DebugSection("frontend",       1 << 0)
case object DebugSectionSolver          extends DebugSection("solver",         1 << 1)
case object DebugSectionSMTRange        extends DebugSection("smt-range",      1 << 2)
case object DebugSectionAnalysis        extends DebugSection("analysis",       1 << 3)
case object DebugSectionBackend         extends DebugSection("backend",        1 << 4)
case object DebugSectionTaylor          extends DebugSection("taylor",         1 << 5)
//case object DebugSection

object DebugSections {
  val all = Set[DebugSection](
    DebugSectionFrontend,
    DebugSectionSolver,
    DebugSectionSMTRange,
    DebugSectionAnalysis,
    DebugSectionBackend,
    DebugSectionTaylor
  )

}