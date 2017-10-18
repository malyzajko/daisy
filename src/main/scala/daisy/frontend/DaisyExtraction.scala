// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import scala.tools.nsc._
import lang.Trees.{Program => DaisyProgram}

trait DaisyExtraction extends SubComponent with CodeExtraction {
  import global._

  val phaseName = "daisy"

  var units: List[CompilationUnit] = Nil

  var imports: Map[RefTree,List[Import]] = Map()

  def setImports(imports: Map[RefTree,List[Import]]): Unit = {
    this.imports = imports
  }

  def compiledProgram: Option[DaisyProgram] = {
    new Extraction(units).extractProgram
  }

  def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      units ::= unit
    }
  }
}