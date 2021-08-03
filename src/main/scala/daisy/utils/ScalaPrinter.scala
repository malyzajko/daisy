// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees._
import daisy.lang.Types._
import daisy.tools.FinitePrecision.{Float128, Float32, Float64}

import scala.collection.immutable.Seq


class ScalaPrinter(buffer: Appendable, ctx: Context,
  val generateDaisyInput: Boolean = false, val importString: String = "") extends CodePrinter(buffer) {

  override val mathPrefix: String = if (!generateDaisyInput) {
    "Math."
  } else {
    ""
  }

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case id: Identifier =>
        if (id.toString.matches("[0-9a-zA-Z_][0-9a-zA-Z_]*")) {
          sb.append(id.toString)
        } else {
          sb.append(s"`${id.toString}`")
        }

      case Let(b,d,e) =>
        sb.append("val ")
        pp(b, p)
        sb.append(": ")
        pp(b.getType,p)
        sb.append(" = ")
        pp(d, p)
        nl(lvl)
        pp(e, p)(lvl)

      case Sqrt(x) => if (generateDaisyInput || x.getType == FinitePrecisionType(Float128)) {
        ppUnary(x, "sqrt(", ")")
      } else {
        ppUnary(x, "math.sqrt(", ")")

        if (x.getType == FinitePrecisionType(Float32)) {
          sb.append(".toFloat")
        }
      }

      case Sin(expr) if (expr.getType == FinitePrecisionType(Float32)) =>
        ppMathFun(Seq(expr), "sin")
        sb.append(".toFloat")
      case Cos(expr) if (expr.getType == FinitePrecisionType(Float32)) =>
        ppMathFun(Seq(expr), "cos")
        sb.append(".toFloat")
      case Tan(expr) if (expr.getType == FinitePrecisionType(Float32)) =>
        ppMathFun(Seq(expr), "tan")
        sb.append(".toFloat")
      case Atan(expr) if (expr.getType == FinitePrecisionType(Float32)) =>
        ppMathFun(Seq(expr), "atan")
        sb.append(".toFloat")
      case Exp(expr) if (expr.getType == FinitePrecisionType(Float32)) =>
        ppMathFun(Seq(expr), "exp")
        sb.append(".toFloat")
      case Log(expr) if (expr.getType == FinitePrecisionType(Float32)) =>
        ppMathFun(Seq(expr), "log")
        sb.append(".toFloat")

      case FinitePrecisionType(Float128) => sb.append("DblDouble")

      case Cast(expr, FinitePrecisionType(Float32)) => ppUnary(expr, "", ".toFloat")
      case Cast(expr, FinitePrecisionType(Float64)) => ppUnary(expr, "", ".toDouble")
      case Cast(expr, FinitePrecisionType(Float128)) => ppUnary(expr, "DblDouble(", ")")
      case Cast(expr, Int32Type) => ppUnary(expr, "", ".toInt")
      case Cast(expr, Int64Type) => ppUnary(expr, "", ".toLong")

      case Program(id, defs) =>
        assert(lvl == 0)
        sb.append("import scala.annotation.strictfp\n")
        sb.append(importString)
        nl(lvl)
        sb.append("@strictfp\n")
        sb.append("object ")
        pp(id, p)
        sb.append(" {\n")
        nl(lvl)
        defs.foreach {
          m => pp(m, p)(lvl + 1)
        }
        if (ctx.hasFlag("genMain")) {
          sb.append(
          raw"""
            |  def main(args: Array[String]): Unit = {
            |    if (args.isEmpty){
            |      println("Usage: scala ${id} [function] [args]")
            |    } else println(args.head match {
            |""".stripMargin)
          defs.foreach { m =>
            sb.append(
              raw"""      case "${m.id}" => assert(args.size == ${m.params.size + 1}, "Expecting ${m.params.size} arguments")
                   |        ${m.id}(${m.params.indices.map(i => s"args(${i+1}).toDouble").mkString(",")})
                   |""".stripMargin)
          }
          sb.append("      case s => s\"Function $s not defined\"\n    })\n")
          sb.append("  }\n")
        }
        sb.append("}\n")

      case fd: FunDef =>
        if (!generateDaisyInput){
          fd.precondition.foreach{ prec => {
            ind
            sb.append("/* ")
            sb.append("@pre: ")
            pp(prec, p)(lvl)
            sb.append(" */\n")
          }}

          fd.postcondition.foreach{ post => {
            ind
            sb.append("/* ")
            sb.append("@post: ")
            pp(post, p)(lvl)
            sb.append(" */")
          }}
        }

        nl(lvl)

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

        sb.append("): ")
        pp(fd.returnType, p)
        sb.append(" = {")
        nl(lvl+1)

        if (generateDaisyInput){
          sb.append("require(")
          fd.precondition.foreach{ prec => {
            pp(prec, p)(lvl)
          }}
          sb.append(")")
          nl(lvl+1)
        } else {
          sb.append("assert(")
          fd.precondition.foreach{ prec => {
            pp(prec, p)(lvl)
          }}
          sb.append(")")
          nl(lvl+1)
        }

        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl + 1)

          case None =>
            sb.append("[unknown function implementation]")
        }
        nl(lvl)
        sb.append("}")
        if (ctx.resultRealRanges.get(fd.id).isDefined) {
          sb.append(s" // ${ctx.resultRealRanges(fd.id)} +/- ${ctx.resultAbsoluteErrors(fd.id)}")
        }
        if (generateDaisyInput && !fd.postcondition.isEmpty){
          sb.append(" ensuring(")
          fd.postcondition.foreach{ post => {
            pp(post, p)(lvl)
          }}
          sb.append(")")
        }
        nl(lvl - 1)
        nl(lvl - 1)

      case _ => super.pp(tree, parent)
    }
  }

  private def getLastExpression(e: Expr): Expr = e match {
    case Let(_, _, body) => getLastExpression(body)
    case _ => e
  }

}