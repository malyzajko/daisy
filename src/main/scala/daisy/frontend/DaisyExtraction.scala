/* Copyright 2009-2016 EPFL, Lausanne */

package daisy
package frontend

import scala.tools.nsc._

trait DaisyExtraction extends SubComponent with CodeExtraction {
  import global._

  val phaseName = "daisy"

  var units: List[CompilationUnit] = Nil

  implicit val ctx: Context

  var imports : Map[RefTree,List[Import]] = Map()

  def setImports( imports : Map[RefTree,List[Import]] ) {
    this.imports = imports
  }

  def compiledProgram = {
    new Extraction(units).extractProgram
  }

  def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new Phase(prev)

  class Phase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      units ::= unit
    }
  }
}