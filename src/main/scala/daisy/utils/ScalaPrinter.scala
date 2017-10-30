// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import lang.Trees._
import lang.Types._
import tools.FinitePrecision.{DoubleDouble, Float32, Float64}

class ScalaPrinter(buffer: Appendable, ctx: Context) extends CodePrinter(buffer) {

  override val mathPrefix: String = "Math."

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case Let(b,d,e) =>
        sb.append("val ")
        pp(b, p)
        sb.append(": ")
        pp(e.getType,p)
        sb.append(" = ")
        pp(d, p)
        nl(lvl)
        pp(e, p)(lvl)

      case Downcast(expr, FinitePrecisionType(Float32)) => ppUnary(expr, "", ".toFloat")
      case Downcast(expr, FinitePrecisionType(Float64)) => ppUnary(expr, "", ".toDouble")
      case Downcast(expr, FinitePrecisionType(DoubleDouble)) => ppUnary(expr, "DblDouble(", ")")

      case Program(id, defs) =>
        assert(lvl == 0)
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
          sb.append(s"${arg.id} : ")
          pp(arg.getType, p)

          if(c < sz - 1) {
            sb.append(", ")
          }
          c = c + 1
        })

        sb.append(") : ")
        pp(fd.returnType, p)
        sb.append(" = {")
        nl(lvl + 1)
        fd.body match {
          case Some(body) =>
            pp(body, p)(lvl + 1)

          case None =>
            sb.append("[unknown function implementation]")
        }
        nl(lvl)
        sb.append(s"} // ${ctx.resultRealRanges(fd.id)} +/- ${ctx.resultAbsoluteErrors(fd.id)}")
        nl(lvl - 1)
        nl(lvl - 1)


      case _ => super.pp(tree, parent)
    }
  }

}