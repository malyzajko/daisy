// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import java.io.BufferedWriter
import scala.collection.immutable.HashMap

import lang.Trees._

object CodePrinter {
  val suffix: Map[String, String] = HashMap(
    "C" -> ".c",
    "Scala" -> ".scala",
    "FPCore" -> ".fpcore")

  def apply(t: Program, ctx: Context, lang: String, out: BufferedWriter): Unit = {
    val printer = lang match {
      case "apfixed" | "C" => new CPrinter(out, ctx)
      case "Scala" => new ScalaPrinter(out, ctx)
      case "FPCore" => new FPCorePrinter(out, ctx)
    }
    printer.pp(t, None)(0)
    out.close()
  }

}

abstract class CodePrinter(buffer: Appendable) extends PrettyPrinter(buffer) {

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {
      case LessEquals(l,r) => ppBinary(l, r, " <= ")
      case GreaterEquals(l,r) => ppBinary(l, r, " >= ")
      case And(exprs) => ppNary(exprs, "(", " && ", ")")
      case Or(exprs) => ppNary(exprs, "(", " || ", ")")
      case Not(Equals(l, r)) => ppBinary(l, r, " != ")
      case Not(expr) => ppUnary(expr, "!(", ")")

      // this should never be called by this printer, i.e. all RealTypes
      // should have been transformed before
      //case RealType => throw new Exception("RealType found in code generator")
      case _ => super.pp(tree, parent)
    }
  }
}
