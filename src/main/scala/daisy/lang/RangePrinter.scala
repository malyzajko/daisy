

package daisy
package lang

import Trees._

object RangePrinter {

  def apply(t: Tree): String = {
    val printer = new RangePrinter
    printer.pp(t, None)(0)
    printer.toString
  }
}

class RangePrinter extends PrettyPrinter {


  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    def appendInterval(x: NumAnnotation): Unit = {
      if (x.hasInterval && x.hasError) {
          sb.append(s"#${x.interval},${x.absError}#")
        } else if (x.hasInterval) {
          sb.append(s"#${x.interval}#")
        } else {
          sb.append("##")
        }

    } 

    tree match {
      case x @ Variable(id) =>
        pp(id, p)
        appendInterval(x)

      case x @ Plus(l,r) => 
        ppBinary(l, r, " + ")
        appendInterval(x)

      case x @ Minus(l,r) => 
        ppBinary(l, r, " - ")
        appendInterval(x)

      case x @ Times(l,r) => 
        ppBinary(l, r, " * ")
        appendInterval(x)

      case x @ Division(l,r) => 
        ppBinary(l, r, " / ")
        appendInterval(x)
      case _ => super.pp(tree, parent)
    }
  }

}