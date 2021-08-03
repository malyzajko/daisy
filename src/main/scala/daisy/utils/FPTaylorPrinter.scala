//Copyright 2021 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import daisy.lang.Types.FinitePrecisionType
import daisy.tools.Rational
import lang.Trees._
import tools.FinitePrecision._

class FPTaylorPrinter(buffer: Appendable, ctx: Context) extends PrettyPrinter(buffer) {

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case fd: FunDef =>

        // define constants?

        sb.append("Variables\n")
        val paramDefs = fd.params.map(vd => s"  float64 ${vd.id.toString} in ${ctx.specInputRanges(fd.id)(vd.id)}")
        sb.append(paramDefs.mkString("", ",\n", ";\n"))

        sb.append("\nDefinitions\n")
        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl)

          case None =>
            sb.append("[unknown function implementation]")
        }

        sb.append("\nExpressions\n")
        sb.append(s"  ${fd.id.toString} = _res\n;")


      case Variable(id) =>
        sb.append(id.toString)
        // pp(id, p)

      case ValDef(id) =>
        sb.append(id.toString)
        // pp(id, p)

      case Let(b,d,e @ Let(_, _, _)) =>
        sb.append("  " + b.toString)
        sb.append(" rnd64= ")
        pp(d, p)
        sb.append(",\n")
        pp(e, p)(lvl)

      // last statements
      case Let(b,d,e) =>
        sb.append("  " + b.toString)
        sb.append(" rnd64= ")
        pp(d, p)
        sb.append(",\n")
        sb.append("  _res rnd64= ")
        pp(e, p)(lvl)
        sb.append("\n;\n")

      // case UMinus(expr) => ppUnary(expr, "-", "")
      // case Sqrt(expr) => ppMathFun(Seq(expr), "sqrt")
      // case Sin(expr) => ppMathFun(Seq(expr), "sin")
      // case Cos(expr) => ppMathFun(Seq(expr), "cos")
      // case Tan(expr) => ppMathFun(Seq(expr), "tan")
      // case Asin(expr) => ppMathFun(Seq(expr), "asin")
      // case Acos(expr) => ppMathFun(Seq(expr), "acos")
      // case Atan(expr) => ppMathFun(Seq(expr), "atan")
      // case Exp(expr) => ppMathFun(Seq(expr), "exp")
      // case Log(expr) => ppMathFun(Seq(expr), "log")
      // case Equals(l,r) => ppBinary(l, r, " == ")
      // case RealLiteral(r) => sb.append(r.toString)
      // case x @ FinitePrecisionLiteral(r, Float16, stringValue) =>
      //   sb.append(stringValue)
      // case x @ FinitePrecisionLiteral(r, Float32, stringValue) =>
      //   sb.append(stringValue)
      // case x @ FinitePrecisionLiteral(r, _, stringValue) =>
      //   sb.append(stringValue)
      // case Cast(expr, tpe) => ppUnary(expr, "downcast "+ tpe, "")

      // case Plus(l,r) => ppBinary(l, r, " + ")
      // case Minus(l,r) => ppBinary(l, r, " - ")
      // case Times(l,r) => ppBinary(l, r, " * ")
      // case FMA(l,m,r) => ppMathFun(Seq(l,m,r), "fma")
      // case Division(l,r) => ppBinary(l, r, " / ")
      // //case Pow(l,r) => ppBinary(l, r, " ^ ")
      // case IntPow(l,r) => ppMathFun(Seq(l, RealLiteral(Rational(r))), "pow")
      // case LessThan(l,r) => ppBinary(l, r, " < ")
      // case GreaterThan(l,r) => ppBinary(l, r, " > ")
      // case LessEquals(l,r) => ppBinary(l, r, " <= ")      // \leq
      // case GreaterEquals(l,r) => ppBinary(l, r, " >= ")   // \geq

      // case IfExpr(c, t, e) =>
      //   sb.append("( if (")
      //   pp(c, p)
      //   sb.append(")")
      //   nl(lvl + 1)
      //   pp(t, p)(lvl + 1)
      //   nl
      //   sb.append("else")
      //   nl(lvl + 1)
      //   pp(e, p)(lvl + 1)
      //   sb.append(")")

      // Definitions
      case Program(id, defs) =>
        assert(lvl == 0)

        defs.foreach {
          m => pp(m, p)(lvl + 1)
        }


      case NoTree(tpe) => sb.append("noTree")
      case _ => super.pp(tree, parent)
    }
  }

}
