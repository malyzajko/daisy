// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import daisy.tools.Rational
import lang.Identifiers.Identifier
import lang.Trees._
import lang.Types._
import tools.FinitePrecision._

import scala.collection.immutable.Seq

object PrettyPrinter {

  def apply(t: Tree): String = {
    val printer = new PrettyPrinter
    printer.pp(t, None)(0)
    printer.toString
  }

  def withIDs(t: Tree): String = {
    val printer = new PrettyPrinter(printUniqueIds = true)
    printer.pp(t, None)(0)
    printer.toString
  }
}


class PrettyPrinter(val sb: Appendable = new StringBuffer, printUniqueIds: Boolean = false,
                    printTypes: Boolean = false, printPositions: Boolean = false) {

  override def toString: String = sb.toString

  def ind(implicit lvl: Int): Unit = {
    sb.append("  " * lvl)
  }
  def nl(implicit lvl: Int): Unit = {
    sb.append("\n")
    ind(lvl)
  }

  // EXPRESSIONS
  // all expressions are printed in-line
  def ppUnary(expr: Tree, op1: String, op2: String)(implicit parent: Option[Tree], lvl: Int): Unit = {
    sb.append(op1)
    pp(expr, parent)
    sb.append(op2)
  }

  def ppBinary(left: Tree, right: Tree, op: String)(implicit parent: Option[Tree], lvl: Int): Unit = {
    sb.append("(")
    pp(left, parent)
    sb.append(op)
    pp(right, parent)
    sb.append(")")
  }

  def ppNary(exprs: Seq[Tree], pre: String, op: String, post: String)(
    implicit  parent: Option[Tree], lvl: Int): Unit = {
    sb.append(pre)
    val sz = exprs.size
    var c = 0

    exprs.foreach(ex => {
      pp(ex, parent);
      c += 1;
      if (c < sz) {
        sb.append(op)
      }
    })

    sb.append(post)
  }

  val mathPrefix: String = ""
  def ppMathFun(exprs: Seq[Tree], fun: String)(implicit parent: Option[Tree], lvl: Int): Unit = {
    ppNary(exprs, mathPrefix + fun + "(", ", ", ")")
  }


  def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {
      case id: Identifier =>
        if (printUniqueIds) {
          sb.append(id.uniqueName)
        } else {
          sb.append(id.toString)
        }

      case Variable(id) =>
        if (printTypes) {
          sb.append("(")
          pp(id, p)
          sb.append(": ")
          pp(id.getType, p)
          sb.append(")")
        } else {
          pp(id, p)
        }

      case Delta(id) =>
        if (printTypes) {
          sb.append("(")
          pp(id, p)
          sb.append(": ")
          pp(id.getType, p)
          sb.append(")")
        } else {
          pp(id, p)
        }

      case Epsilon(id) =>
        if (printTypes) {
          sb.append("(")
          pp(id, p)
          sb.append(": ")
          pp(id.getType, p)
          sb.append(")")
        } else {
          pp(id, p)
        }

      case ValDef(id) =>
        pp(id, p)

      case Let(b,d,e) =>
        sb.append("(let (")
        pp(b, p)
        sb.append(" := ");
        pp(d, p)
        sb.append(") in")
        nl(lvl + 1)
        pp(e, p)(lvl + 1)
        sb.append(")")

      /* case Block(exprs, last) =>
        ppNary(exprs :+ last, "{", "\n", "}")

      case Assignment(id, e) =>
        sb.append("val ")
        pp(id, p)
        sb.append(" = ")
        pp(e, p)
        sb.append("\n") */

      case And(exprs) => ppNary(exprs, "(", " \u2227 ", ")")            // \land
      case Or(exprs) => ppNary(exprs, "(", " \u2228 ", ")")             // \lor
      case Not(Equals(l, r)) => ppBinary(l, r, " \u2260 ")    // \neq
      case Not(expr) => ppUnary(expr, "\u00AC(", ")")               // \neg
      case Implies(l,r) => ppBinary(l, r, " ==> ")
      case UMinus(expr) => ppUnary(expr, "-(", ")")

      case Sin(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "sinf")
      case Sqrt(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "sqrtf")
      case Cos(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "cosf")
      case Tan(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "tanf")
      case Exp(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "expf")
      case Log(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "logf")
      case Atan(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "atanf")
      case Asin(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "asinf")
      case Acos(expr) if (expr.getType == FinitePrecisionType(Float32)) => ppMathFun(Seq(expr), "acosf")

      case Sqrt(expr) => ppMathFun(Seq(expr), "sqrt")
      case Sin(expr) => ppMathFun(Seq(expr), "sin")
      case Cos(expr) => ppMathFun(Seq(expr), "cos")
      case Tan(expr) => ppMathFun(Seq(expr), "tan")
      case Exp(expr) => ppMathFun(Seq(expr), "exp")
      case Log(expr) => ppMathFun(Seq(expr), "log")
      case Atan(expr) => ppMathFun(Seq(expr), "atan")
      case Asin(expr) => ppMathFun(Seq(expr), "asin")
      case Acos(expr) => ppMathFun(Seq(expr), "acos")
      case Approx(_, expr, _, _, fName, _) => ppMathFun(Seq(expr), fName)
      case Equals(l,r) => ppBinary(l, r, " == ")
      case Int16Literal(v) => sb.append(v.toString)
      case Int32Literal(v) => sb.append(v.toString)
      case Int64Literal(v) => sb.append(v.toString)
      case IntegerLiteral(v) => sb.append(v.toString)
      case BooleanLiteral(v) => sb.append(v.toString)
      case UnitLiteral() => sb.append("()")
      case RealLiteral(r) => sb.append(r.toString)
      case x @ FinitePrecisionLiteral(r, Float16, stringValue) =>
        sb.append(stringValue + "hf")
      case x @ FinitePrecisionLiteral(r, Float32, stringValue) =>
        sb.append(stringValue + "f")
      case x @ FinitePrecisionLiteral(r, _, stringValue) =>
        sb.append(stringValue)
      case Cast(expr, tpe) => ppUnary(expr, "cast(", ", " + tpe + ")")
      case FunctionInvocation(fdId, _, args, _) =>
        pp(fdId, p)

        ppNary(args, "(", ", ", ")")

      case Lambda(args, body) =>
        ppNary(args, "(", ", ", ")")
        sb.append(" => ")
        pp(body, p)

      case Plus(l,r) => ppBinary(l, r, " + ")
      case Minus(l,r) => ppBinary(l, r, " - ")
      case Times(l,r) => ppBinary(l, r, " * ")
      case FMA(l,m,r) => ppMathFun(Seq(l,m,r), "fma")
      case Division(l,r) => ppBinary(l, r, " / ")
//      case Pow(l,r) => ppBinary(l, r, " ^ ")
      case IntPow(l,r) => ppMathFun(Seq(l, RealLiteral(Rational(r))), "pow")
      case AbsError(l, r) => ppBinary(l, r, " +/- ")
      case LessThan(l,r) => ppBinary(l, r, " < ")
      case GreaterThan(l,r) => ppBinary(l, r, " > ")
      case LessEquals(l,r) => ppBinary(l, r, " \u2264 ")      // \leq
      case GreaterEquals(l,r) => ppBinary(l, r, " \u2265 ")   // \geq

      case RightShift(t, by) => ppUnary(t, "(", " >> " + by + ")")
      case LeftShift(t, by) => ppUnary(t, "(", " << " + by + ")")

      case IfExpr(c, t, e) =>
        sb.append("if (")
        pp(c, p)
        sb.append(") {")
        nl(lvl + 1)
        pp(t, p)(lvl + 1)
        nl
        sb.append("} else {")
        nl(lvl + 1)
        pp(e, p)(lvl + 1)
        sb.append("}")

      case Tuple(args) => ppNary(args, "(", ", ", ")")

      case Error(tpe, desc) =>
        sb.append(s"""error[$tpe]("$desc""")

      // Types
      case Untyped => sb.append("<untyped>")
      case UnitType => sb.append("Unit")
      case Int16Type => sb.append("Short")
      case Int32Type => sb.append("Int")
      case Int64Type => sb.append("Long")
      case BooleanType => sb.append("Boolean")
      case RealType => sb.append("Real")
      case FinitePrecisionType(Float16) => sb.append("Float16")
      case FinitePrecisionType(Float32) => sb.append("Float")
      case FinitePrecisionType(Float64) => sb.append("Double")
      case FinitePrecisionType(DoubleDouble) => sb.append("DoubleDouble")
      case FinitePrecisionType(QuadDouble) => sb.append("QuadDouble")
      case FinitePrecisionType(FixedPrecision(b)) => sb.append(s"Fixed($b)")
      case FunctionType(fts, tt) =>
        if (fts.size > 1) {
          ppNary(fts, "(", ", ", ")")
        } else if (fts.size == 1) {
          pp(fts.head, p)
        }
        sb.append(" => ")
        pp(tt, p)
      case TupleType(args) => ppNary(args, "(", ", ", ")")

      // Definitions
      case Program(id, defs) =>
        assert(lvl == 0)
        sb.append("object ")
        pp(id, p)
        sb.append(" {\n")
        defs.foreach {
          m => pp(m, p)(lvl + 1)
        }
        sb.append("\n}\n")

      case fd: FunDef =>
        fd.precondition.foreach{ prec => {
          ind
          sb.append("@pre : ")
          pp(prec, p)(lvl)
          sb.append("\n")
        }}

        fd.postcondition.foreach{ post => {
          ind
          sb.append("@post: ")
          pp(post, p)(lvl)
          sb.append("\n")
        }}

        ind
        sb.append("def ")
        pp(fd.id, p)
        sb.append("(")

        val sz = fd.params.size
        var c = 0

        fd.params.foreach(arg => {
          sb.append(s"${arg.id}: ")
          pp(arg.getType, p)

          if(c < sz - 1) {
            sb.append(", ")
          }
          c = c + 1
        })

        sb.append(") : ")
        pp(fd.returnType, p)
        sb.append(" = ")
        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl)

          case None =>
            sb.append("[unknown function implementation]")
        }
        sb.append("\n\n")

      case NoTree(tpe) => sb.append("noTree")
      case _ => sb.append("Tree? (" + tree.getClass + ")")
    }
    if (printPositions) {
      ppos(tree.getPos)
    }
  }

  def ppos(p: utils.Position): Unit = p match {
    case op: utils.OffsetPosition =>
      sb.append("@" + op.toString)
    case rp: utils.RangePosition =>
      sb.append("@" + rp.focusBegin.toString + "--" + rp.focusEnd.toString)
    case _ =>
  }
}
