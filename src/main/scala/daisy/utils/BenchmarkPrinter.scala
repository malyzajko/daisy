package daisy
package utils

import lang.Trees._
import daisy.lang.Types._

object BenchmarkPrinter {

  def apply(t: Tree): String = {
    val printer = new BenchmarkPrinter
    printer.pp(t, None)(0)
    printer.toString
  }

  // def applyCustom(t: Tree, applyAsDaisyInput: Boolean, importString: String): String = {
  //   val printer = new ScalaPrinter(generateDaisyInput = applyAsDaisyInput, importString = importString)
  //   printer.pp(t, None)(0)
  //   printer.toString
  // }

}


/*
  Generates code which can be consumed by Daisy as input.
*/
class BenchmarkPrinter extends PrettyPrinter {


  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case Sqrt(x) =>
        ppUnary(x, "sqrt(", ")")

      //case RealLiteral(r) => sb.append(r.doubleValue().toString())
      //case RealType => sb.append("Real")

      case RealType => sb.append("Real")

      case r: RealLiteral => sb.append(r.stringValue)

      case Program(id, defs) =>
        assert(lvl == 0)
        sb.append("import _root_.daisy.lang._\n" +
        "import Real._\n")
        nl(lvl)
        sb.append("object ")
        pp(id, p)
        sb.append(" {\n")
        nl(lvl)
        defs.foreach {
          m => pp(m, p)(lvl+1)
        }
        sb.append("}\n")

      case fd: FunDef =>
        nl(lvl)

        ind
        sb.append("def ")
        pp(fd.id, p)
        sb.append("(")

        val sz = fd.params.size
        var c = 0

        fd.params.foreach(arg => {
          sb.append(arg.id.toString)
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
        nl(lvl+2)
        sb.append("require(")
        fd.precondition.foreach{ prec => {
          pp(prec, p)(lvl)
        }}
        sb.append(")")
        nl(lvl+2)
        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl+2)

          case None =>
            sb.append("[unknown function implementation]")
        }
        nl(lvl+1)
        sb.append("}")
        sb.append(" ensuring(")
        fd.postcondition.foreach{ post => {
          pp(post, p)(lvl)
        }}
        sb.append(")")
        nl(lvl-1)
        nl(lvl-1)


      case _ => super.pp(tree, parent)
    }
  }


}