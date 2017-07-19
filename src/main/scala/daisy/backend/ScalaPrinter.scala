
package daisy
package backend

import lang.Trees._
import lang.PrettyPrinter

object ScalaPrinter {

  def apply(t: Tree): String = {
    val printer = new ScalaPrinter
    printer.pp(t, None)(0)
    printer.toString
  }

}

class ScalaPrinter extends PrettyPrinter {

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case And(exprs) => ppNary(exprs, "(", " && ", ")")
      case Or(exprs) => ppNary(exprs, "(", " || ", ")")
      case Not(Equals(l, r)) => ppBinary(l, r, " != ")
      case Not(expr) => ppUnary(expr, "!(", ")")

      case LessEquals(l,r) => ppBinary(l, r, " <= ")      // \leq
      case GreaterEquals(l,r) => ppBinary(l, r, " >= ")   // \geq

      case fd: FunDef =>
        ind
        sb.append("def ")
        pp(fd.id, p)
        sb.append("(")

        val sz = fd.params.size
        var c = 0

        fd.params.foreach(arg => {
          sb.append(arg.id)
          sb.append(": ")
          pp(arg.getType, p)

          if(c < sz - 1) {
            sb.append(", ")
          }
          c = c + 1
        })

        sb.append(") : ")
        pp(fd.returnType, p)
        sb.append(" = {")
        sb.append("\n")

        // TODO: can we have several 'require's?
        fd.precondition.foreach{ prec => {
          ind
          ind
          sb.append("require(")
          pp(prec, p)(lvl)
          sb.append(")\n")
        }}

        ind
        ind
        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl + 1)

          case None =>
            sb.append("[unknown function implementation]")
        }
        sb.append("\n")
        ind
        sb.append("}")

        fd.postcondition.foreach{ post => {
          ind
          sb.append("ensuring(")
          pp(post, p)(lvl)
          sb.append(")")
        }}
        sb.append("\n")

      case _ => super.pp(tree, parent)
    }
  }

}