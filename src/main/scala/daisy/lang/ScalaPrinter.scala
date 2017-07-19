package daisy
package lang

import Trees._
import daisy.lang.Types.RealType

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

      case Let(b,d,e) =>
        sb.append("val ")
        pp(b, p)
        sb.append(" = ");
        pp(d, p)
        nl(lvl)
        pp(e, p)(lvl)

      case LessEquals(l,r) => ppBinary(l, r, " <= ")      // \leq
      case GreaterEquals(l,r) => ppBinary(l, r, " >= ")   // \geq
      case And(exprs) => ppNary(exprs, "(", " && ", ")")            // \land
      case Or(exprs) => ppNary(exprs, "(", " || ", ")")             // \lor
      case Not(Equals(l, r)) => ppBinary(l, r, " != ")    // \neq
      case Not(expr) => ppUnary(expr, "!(", ")")               // \neg

      // this should never be called by this printer, i.e. all RealTypes
      // should have been transformed before
      case RealType =>
        throw new Exception("RealType found in ScalaPrinter")



      case Program(id, defs) =>
        assert(lvl == 0)
        sb.append("object ")
        pp(id, p)
        sb.append(" {\n")
        nl(lvl)
        defs.foreach {
          m => pp(m, p)(lvl+1)
        }
        sb.append("}\n")

      case fd: FunDef =>
        fd.precondition.foreach{ prec => {
          ind
          sb.append("/*")
          sb.append("@pre : ")
          pp(prec, p)(lvl)
          sb.append("*/")
          sb.append("\n")
        }}

        fd.postcondition.foreach{ post => {
          ind
          sb.append("/*")
          sb.append("@post: ")
          pp(post, p)(lvl)
          sb.append("*/")
          sb.append("\n")
        }}

        ind
        sb.append("def ")
        pp(fd.id, p)
        sb.append("(")

        val sz = fd.params.size
        var c = 0

        fd.params.foreach(arg => {
          sb.append(arg.id)
          sb.append(" : ")
          pp(arg.getType, p)

          if(c < sz - 1) {
            sb.append(", ")
          }
          c = c + 1
        })

        sb.append(") : ")
        pp(fd.returnType, p)
        sb.append(" = {")
        nl(lvl+1)
        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl+1)

          case None =>
            sb.append("[unknown function implementation]")
        }
        nl(lvl)
        sb.append("}")
        nl(lvl-1)
        nl(lvl-1)


      case _ => super.pp(tree, parent)
    }
  }

}