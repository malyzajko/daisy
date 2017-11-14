// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import lang.TreeOps
import lang.Trees._
import lang.Types._
import tools.FinitePrecision._

// GenerateDaisyInput: If we are interested in generating a real valued program, that will later be again used as input to Daisy
// importString: import and package statements inlcuded at top of generated file
class CPrinter(buffer: Appendable, ctx: Context) extends CodePrinter(buffer) {

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case Let(b,d,e) =>
        pp(b.getType, p)
        sb.append(" ")
        pp(b, p)
        sb.append(" = ")
        pp(d, p)
        sb.append(";")
        nl(lvl)
        e match {
          case x: Let => ;
          case _ => sb.append("return ")
        }
        pp(e, p)(lvl)
        e match {
          case x: Let => ;
          case _ => sb.append(";")
        }

      case Downcast(expr, t) =>
        sb.append("(")
        pp(t,p)
        sb.append(") (")
        pp(expr,p)
        sb.append(")")

      case FinitePrecisionType(Float16) => sb.append("half")
      case FinitePrecisionType(Float32) => sb.append("float")
      case FinitePrecisionType(Float64) => sb.append("double")
      case FinitePrecisionType(DoubleDouble) => sb.append("dd_real")
      case FinitePrecisionType(QuadDouble) => sb.append("qd_real")

      case Int16Type => sb.append("short")
      case Int32Type => sb.append("int")
      case Int64Type => sb.append("long")

      case x @ FinitePrecisionLiteral(r, Float32, strVal) =>
        if (strVal.contains(".") || strVal.contains("e") || strVal.contains("E")) { //valid float number
          sb.append(strVal + "f")
        } else {
          sb.append(strVal + ".0f")
        }

      case Program(id, defs) =>
        assert(lvl == 0)
        // if (ctx.option[List[String]]("comp-opts").nonEmpty &&
        //     defs.exists(_.body.exists(TreeOps.exists { case FMA(_, _, _) => true }))){
        //   sb.append(
        //     """#pragma STDC FP_CONTRACT OFF
        //       |#if !__FMA__ && !__FMA4__
        //       |  #pragma message("Fast FMA not supported by architecture. Supported architectures: >=haswell (FMA3), >=bdver1 (FMA4)'")
        //       |#endif
        //       |
        //       |""".stripMargin)
        // }
        sb.append("#include <math.h>\n")
        if (defs.flatMap(_.body).exists(
          TreeOps.exists{ case e => e.getType match {
            case FinitePrecisionType(pr) => pr >= DoubleDouble case _ => false }})) {
          sb.append("#include <qd/dd_real.h>\n")
        }
        defs.foreach {
          m => pp(m, p)(lvl)
        }
        if (ctx.hasFlag("genMain")) {
          sb.append(
            """
              |#include <stdio.h>
              |#include <string.h>
              |#include <stdlib.h>
              |
              |int main(int argc, char* argv[]){
              |  if (argc == 1) {
              |    printf("Usage: %s [function name]\n", argv[0]);
              |  } else """.stripMargin)
          defs.foreach {
            m => sb.append(
              raw"""if (strcmp(argv[1], "${m.id}") == 0){
               |    if (argc != ${m.params.size + 2}) {
               |      printf("Expecting ${m.params.size} arguments\n");
               |    } else {
               |      printf("%f\n",${m.id}(${m.params.indices.map(i => s"atof(argv[${i+2}])").mkString(",")}));
               |    }
               |  } else """.stripMargin)
          }
          sb.append("""printf("Function %s not defined\n", argv[1]);""")
          sb.append("\n}")
        }

      case fd: FunDef =>
        fd.precondition.foreach{ prec => {
          sb.append("\n")
          ind
          sb.append("/* ")
          sb.append("@pre : ")
          pp(prec, p)(lvl)
          sb.append(" */")
        }}

        fd.postcondition.foreach{ post => {
          ind
          sb.append("/*")
          sb.append("@post: ")
          pp(post, p)(lvl)
          sb.append("*/")
          sb.append("\n")
        }}

        nl(lvl)

        ind
        pp(fd.returnType, p)
        sb.append(" ")
        pp(fd.id, p)
        sb.append("(")

        val sz = fd.params.size
        var c = 0
        fd.params.foreach(arg => {
          pp(arg.getType, p)
          sb.append(s" ${arg.id}")

          if(c < sz - 1) {
            sb.append(", ")
          }
          c = c + 1
        })
        sb.append(") {")
        nl(lvl+1)
        fd.body match {
          case Some(body@Let(_,_,_)) =>
            pp(body, p)(lvl+1)
          case Some(body) =>
            sb.append("return ")
            pp(body,p)
            sb.append(";")
          case None =>
            sb.append("[unknown function implementation]")
        }
        nl(lvl)
        sb.append(s"} // ${ctx.resultRealRanges(fd.id)} +/- ${ctx.resultAbsoluteErrors(fd.id)}")
        nl(lvl-1)

    case _ => super.pp(tree, parent)
    }
  }

  private def getLastExpression(e: Expr): Expr = e match {
    case Let(_, _, body) => getLastExpression(body)
    case _ => e
  }

}
