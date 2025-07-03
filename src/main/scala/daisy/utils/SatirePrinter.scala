//Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.utils

import daisy.Context
import daisy.lang.Identifiers.{Identifier, FreshIdentifier}
import daisy.lang.TreeOps
import daisy.lang.Trees._
import daisy.lang.Types.{TypeTree, FinitePrecisionType}
import daisy.tools.FinitePrecision._
import daisy.tools.Rational

import scala.collection.immutable.HashMap
import java.io.FileWriter
import java.io.BufferedWriter


class SatirePrinter(buffer: Appendable, ctx: Context) extends CodePrinter(buffer) {

  val precisionIn: Map[Precision, String] = HashMap(
    Float32 -> "fl32",
    Float64 -> "fl64"
  )

  val roundingIn: Map[Precision, String] = HashMap(
    Float32 -> "rnd32",
    Float64 -> "rnd64"
  )

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
    pp(left, parent)
    sb.append(op)
    pp(right, parent)
    sb.append(")")
  }

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)


    tree match {

      case Variable(id) =>
        sb.append(id.uniqueNameDelimited("_"))
        // pp(id, p)

      case ValDef(id) =>
        sb.append(id.uniqueNameDelimited("_"))
        // pp(id, p)

      case Let(b,d,e) =>
        // x_1_0 rnd64 = (x + 10.0*(y - x)*0.005);
        val prec = d.getType match {
          case FinitePrecisionType(pr) => roundingIn(pr)
        }
        sb.append(s"${b.uniqueNameDelimited('_'.toString)} $prec = ")
        pp(d, p)
        sb.append(";\n")
        //nl(lvl + 1)
        e match {
          case Variable(_) => ;
          case Let(_,_,_) => pp(e, p)(lvl)
          case _ => ; // everything else should be transformed in the FunDef printing case
        }

      case AbsError(_, err) => sb.append(f"${err}\n")

      //case And(exprs) => ppNary(exprs, "and", " ")            // \land
      //case Or(exprs) => ppNary(exprs,"or", " ")             // \lor
      case Not(Equals(l, r)) => ppBinary(l, r, " != ")    // \neq
      case Not(expr) => ppUnary(expr, "!", "")               // \neg
      case Implies(l,r) => ppBinary(l, r, " ==> ")
      case UMinus(expr) => ppBinary(RealLiteral(-1), expr, " * ")
      case Sqrt(expr) => ppMathFun(Seq(expr), "sqrt")
      case Sin(expr) => ppMathFun(Seq(expr), "sin")
      case Cos(expr) => ppMathFun(Seq(expr), "cos")
      case Tan(expr) => ppMathFun(Seq(expr), "tan")
      case Asin(expr) => ppMathFun(Seq(expr), "asin")
      case Acos(expr) => ppMathFun(Seq(expr), "acos")
      case Atan(expr) => ppMathFun(Seq(expr), "atan")
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
      case GreaterEquals(l,r) => ppBinary(l, r, " >= ")   // \geq

      //case IfExpr(c, t, e) =>
      //  sb.append("( if (")
      //  pp(c, p)
      //  sb.append(")")
      //  nl(lvl + 1)
      //  pp(t, p)(lvl + 1)
      //  nl
      //  sb.append("else")
      //  nl(lvl + 1)
      //  pp(e, p)(lvl + 1)
      //  sb.append(")")

      // Definitions
      case Program(id, defs) =>
        assert(lvl == 0)

        defs.foreach {
          m => pp(m, p)(lvl + 1)
        }

      case fd: FunDef =>
        // Satire allows only a single function per file, so we store the prefix
        // to be able to extract the part for this FunDef only
        val sbBefore: String = sb.toString

        // inputs
        sb.append("INPUTS { \n\n")

        // xm1_0   fl64 : (1,2) ;
        fd.params.foreach {
          vd => {
            // FIXME hardcoded Float64 as a default
            val prec = precisionIn(ctx.specInputPrecisions(fd.id).getOrElse(vd.id, ctx.uniformPrecisions.getOrElse(fd.id, Float64)))
            val range = ctx.specInputRanges(fd.id)(vd.id)
              .toBigString
              .replace('[', '(')
              .replace(']', ')')
            sb.append(s" ${vd.id.uniqueNameDelimited('_'.toString)}\t $prec:\t $range ;\n")
          }
        }
        sb.append ("}\n\n")

        val (formattedBody, returnId) = getLastVariable(fd.body.get, fd.returnType)
        sb.append(s"OUTPUTS { ${returnId.uniqueNameDelimited('_'.toString)}; }\n\n")

        sb.append("EXPRS {\n\n")
        pp(formattedBody, Some(fd))
        sb.append("\n\n}\n")

        // print each FunDef to a separate file
        val diff = sb.toString.replace(sbBefore, "")
        val origFileName = ctx.file.drop(ctx.file.lastIndexOf('/') + 1).replace(".scala", "")
        val filename = System.getProperty("user.dir") + "/output/" + origFileName + "_" + fd.id + ".txt"
        val out = new BufferedWriter(new FileWriter(filename))
        out.write(diff)
        out.close

      case NoTree(tpe) => sb.append("noTree")
      case _ => sb.append("Tree? (" + tree.getClass + ")")
    }
  }

  // Returns the variable that is returned, and adds a fresh one if the last
  // expression is not a variable
  private def getLastVariable(e: Expr, returnType: TypeTree): (Expr, Identifier) = e match {
    case Let(id, t, body) =>
      val (newBody, retId) = getLastVariable(body, returnType)
      (Let(id, t, newBody), retId)
    case x @ Variable(id) => // last expression is already a variable
      (x, id)
    case _ =>
      val tmpId = FreshIdentifier("tmp_ret", returnType)
      (Let(tmpId, e, Variable(tmpId)), tmpId)
  }

}
