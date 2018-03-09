package daisy
package utils

import lang.Trees._

object FPTunerPrinter {

  def apply(t: Tree): String = {
    val printer = new FPTunerPrinter
    printer.pp(t, None)(0)
    printer.toString
  }

}


// GenerateDaisyInput: If we are interested in generating a real valued program, that will later be again used as input to Daisy
// importString: import and package statements inlcuded at top of generated file
class FPTunerPrinter extends PrettyPrinter {


  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case Let(b,d,e) =>
        //sb.append("val ")
        pp(b, p)
        //sb.append(": ")
        //pp(b.getType, p)
        sb.append(" = ");
        pp(d, p)
        nl(lvl)
        pp(e, p)(lvl)

      case Plus(Variable(lhs), Variable(rhs)) =>
        sb.append(s"""IR.BE("+", ?, $lhs, $rhs)""")

      case Minus(Variable(lhs), Variable(rhs)) =>
        sb.append(s"""IR.BE("-", ?, $lhs, $rhs)""")

      case Times(Variable(lhs), Variable(rhs)) =>
        sb.append(s"""IR.BE("*", ?, $lhs, $rhs)""")

      case Division(Variable(lhs), Variable(rhs)) =>
        sb.append(s"""IR.BE("/", ?, $lhs, $rhs)""")

      case UMinus(Variable(t)) =>
        sb.append(s"""IR.UE("-",  ?, $t)""")

      // case Sqrt(x) => if (generateDaisyInput) {
      //   ppUnary(x, "sqrt(", ")")
      // } else {
      //   ppUnary(x, "math.sqrt(", ")")
      // }
      // case LessEquals(l,r) => ppBinary(l, r, " <= ")      // \leq
      // case GreaterEquals(l,r) => ppBinary(l, r, " >= ")   // \geq
      // case And(exprs) => ppNary(exprs, "(", " && ", ")")            // \land
      // case Or(exprs) => ppNary(exprs, "(", " || ", ")")             // \lor
      // case Not(Equals(l, r)) => ppBinary(l, r, " != ")    // \neq
      // case Not(expr) => ppUnary(expr, "!(", ")")               // \neg
      // case Cast(expr, FinitePrecisionType(Float32)) => ppUnary(expr, "", ".toFloat")
      // case Cast(expr, FinitePrecisionType(Float64)) => ppUnary(expr, "", ".toDouble")

      // case FinitePrecisionType(DoubleDouble) => sb.append("DblDouble")

      // // this should never be called by this printer, i.e. all RealTypes
      // // should have been transformed before
      // case RealType =>
      //   throw new Exception("RealType found in ScalaPrinter")

      // case Program(id, defs) =>
      //   assert(lvl == 0)
      //   sb.append(importString)
      //   nl(lvl)
      //   sb.append("object ")
      //   pp(id, p)
      //   sb.append(" {\n")
      //   nl(lvl)
      //   defs.foreach {
      //     m => pp(m, p)(lvl+1)
      //   }
      //   sb.append("}\n")

      // case fd: FunDef =>
      //   if (!generateDaisyInput){
      //     fd.precondition.foreach{ prec => {
      //       ind
      //       sb.append("/*")
      //       sb.append("@pre : ")
      //       pp(prec, p)(lvl)
      //       sb.append("*/")
      //       sb.append("\n")
      //     }}

      //     fd.postcondition.foreach{ post => {
      //       ind
      //       sb.append("/*")
      //       sb.append("@post: ")
      //       pp(post, p)(lvl)
      //       sb.append("*/")
      //       sb.append("\n")
      //     }}
      //     val infoString = getLastExpression(fd.body.get) match {
      //       case x: NumAnnotation if x.hasError =>
      //         val absError = x.absError
      //         val range = x.interval
      //         s"Abs error: ${x.absError}, Range: ${x.interval}"
      //       case x: NumAnnotation =>
      //         "Abs error: -, Range: -"
      //       case _ =>
      //     }
      //     ind
      //     sb.append("/*")
      //     sb.append(infoString)
      //     sb.append("*/")
      //   }





      //   nl(lvl)

      //   ind
      //   sb.append("def ")
      //   pp(fd.id, p)
      //   sb.append("(")

      //   val sz = fd.params.size
      //   var c = 0

      //   fd.params.foreach(arg => {
      //     sb.append(arg.id)
      //     sb.append(" : ")
      //     pp(arg.getType, p)

      //     if(c < sz - 1) {
      //       sb.append(", ")
      //     }
      //     c = c + 1
      //   })

      //   sb.append(") : ")
      //   pp(fd.returnType, p)
      //   sb.append(" = {")
      //   nl(lvl+1)
      //   if (generateDaisyInput){
      //     sb.append("require(")
      //     fd.precondition.foreach{ prec => {
      //       pp(prec, p)(lvl)
      //     }}
      //     sb.append(")")
      //     nl(lvl+1)
      //   }
      //   fd.body match {
      //     case Some(body) =>
      //       pp(body, p)(lvl+1)

      //     case None =>
      //       sb.append("[unknown function implementation]")
      //   }
      //   nl(lvl)
      //   sb.append("}")
      //   if (generateDaisyInput){
      //     sb.append(" ensuring(")
      //     fd.postcondition.foreach{ post => {
      //       pp(post, p)(lvl)
      //     }}
      //     sb.append(")")
      //   }
      //   nl(lvl-1)
      //   nl(lvl-1)


      case _ => super.pp(tree, parent)
    }
  }

  private def getLastExpression(e: Expr): Expr = e match {
    case Let(_, _, body) => getLastExpression(body)
    case _ => e
  }

}