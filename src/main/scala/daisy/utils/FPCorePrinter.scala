//Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import daisy.tools.Rational
import lang.Trees._
import tools.FinitePrecision._

class FPCorePrinter(buffer: Appendable, ctx: Context) extends CodePrinter(buffer) {

  // EXPRESSIONS
  // all expressions are printed in-line
  override def ppUnary(expr: Tree, op1: String, op2: String)(implicit parent: Option[Tree], lvl: Int): Unit = {
    sb.append("(" + op1 + " ")
    pp(expr, parent)
    sb.append(op2)
    sb.append(")")
  }

  override def ppBinary(left: Tree, right: Tree, op: String)(implicit parent: Option[Tree], lvl: Int): Unit = {
    sb.append("(")
    sb.append(op)
    pp(left, parent)
    sb.append(" ")
    pp(right, parent)
    sb.append(")")
  }

  def ppNary(exprs: Seq[Tree], op: String)(implicit parent: Option[Tree], lvl: Int): Unit = {
    sb.append("(" + op)

    exprs.foreach(ex => {
      sb.append(" ")
      pp(ex, parent)
    })

    sb.append(")")
  }

  def ppMathFun(exprs: Seq[Tree], fun: String)(implicit parent:Option[Tree], lvl: Int): Unit = {
    ppNary(exprs, fun)
  }


  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case Variable(id) =>
        sb.append(id.toString)
        // pp(id, p)

      case ValDef(id) =>
        sb.append(id.toString)
        // pp(id, p)

      case Let(b,d,e) =>
        sb.append("(let ([")
        sb.append(b.toString)
        sb.append(" ");
        pp(d, p)
        sb.append("]) ")
        nl(lvl + 1)
        pp(e, p)(lvl + 1)
        sb.append(")")

      case And(exprs) => ppNary(exprs, "and")            // \land
      case Or(exprs) => ppNary(exprs,"or")             // \lor
      case Not(Equals(l, r)) => ppBinary(l, r, " <> ")    // \neq
      case Not(expr) => ppUnary(expr, "!", "")               // \neg
      case Implies(l,r) => ppBinary(l, r, " ==> ")
      case UMinus(expr) => ppUnary(expr, "-", "")
      case Sqrt(expr) => ppMathFun(Seq(expr), "sqrt")
      case Sin(expr) => ppMathFun(Seq(expr), "sin")
      case Cos(expr) => ppMathFun(Seq(expr), "cos")
      case Tan(expr) => ppMathFun(Seq(expr), "tan")
      case Exp(expr) => ppMathFun(Seq(expr), "exp")
      case Log(expr) => ppMathFun(Seq(expr), "log")
      case Equals(l,r) => ppBinary(l, r, " == ")
      case RealLiteral(r) => sb.append(r.toString)
      case x @ FinitePrecisionLiteral(r, Float16, stringValue) =>
        sb.append(stringValue)
      case x @ FinitePrecisionLiteral(r, Float32, stringValue) =>
        sb.append(stringValue)
      case x @ FinitePrecisionLiteral(r, _, stringValue) =>
        sb.append(stringValue)
      case Cast(expr, tpe) => ppUnary(expr, "downcast "+ tpe, "")

      case Plus(l,r) => ppBinary(l, r, " + ")
      case Minus(l,r) => ppBinary(l, r, " - ")
      case Times(l,r) => ppBinary(l, r, " * ")
      case FMA(l,m,r) => ppMathFun(Seq(l,m,r), "fma")
      case Division(l,r) => ppBinary(l, r, " / ")
      //case Pow(l,r) => ppBinary(l, r, " ^ ")
      case IntPow(l,r) => ppMathFun(Seq(l, RealLiteral(Rational(r))), "pow")
      case LessThan(l,r) => ppBinary(l, r, " < ")
      case GreaterThan(l,r) => ppBinary(l, r, " > ")
      case LessEquals(l,r) => ppBinary(l, r, " <= ")      // \leq
      case GreaterEquals(l,r) => ppBinary(l, r, " <= ")   // \geq

      case IfExpr(c, t, e) =>
        sb.append("( if (")
        pp(c, p)
        sb.append(")")
        nl(lvl + 1)
        pp(t, p)(lvl + 1)
        nl
        sb.append("else")
        nl(lvl + 1)
        pp(e, p)(lvl + 1)
        sb.append(")")

      // Definitions
      case Program(id, defs) =>
        assert(lvl == 0)

        defs.foreach {
          m => pp(m, p)(lvl + 1)
        }

      case fd: FunDef =>
        sb.append("(FPCore (")
        fd.params.foreach {
          vd => sb.append (s" ${vd.id.toString} ")
        }
        sb.append (")\n")

        sb.append(":name \"" + fd.id.toString + "\"\n")

        fd.precondition.foreach{ prec => {
          ind
          sb.append(":pre ")
          pp(prec, p)(lvl)
          sb.append("\n")
        }}

        fd.postcondition.foreach{ post => {
          ind
          sb.append(":ensuring ")
          pp(post, p)(lvl)
          sb.append("\n")
        }}

        ind
        // sb.append("ef ")
        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl)

          case None =>
            sb.append("[unknown function implementation]")
        }
        sb.append("\n)\n\n")

      case NoTree(tpe) => sb.append("noTree")
      case _ => sb.append("Tree? (" + tree.getClass + ")")
    }
  }

}
