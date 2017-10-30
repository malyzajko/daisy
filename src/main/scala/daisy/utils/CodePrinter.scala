// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.utils

import java.io.BufferedWriter

import daisy.{Config, Context}
import daisy.lang.Trees._
import daisy.lang.Types._

import scala.collection.immutable.HashMap

object CodePrinter {
  val suffix: Map[String, String] = HashMap("C" -> ".c", "Scala" -> ".scala")

  def apply(t: Program, ctx: Context, lang: String, out: BufferedWriter, cfg: Config): Unit = {
    val printer = lang match {
      case "C" => new CPrinter(out, cfg)
      case "Scala" => new ScalaPrinter(out, cfg)
    }
    printer.pp(t, None)(0, ctx)
    out.close()
  }

}

abstract class CodePrinter(buffer: Appendable) extends PrettyPrinter(buffer) {

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int, ctx: Context): Unit = {
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
      case RealType => throw new Exception("RealType found in code generator")
      case _ => super.pp(tree, parent)
    }
  }
}