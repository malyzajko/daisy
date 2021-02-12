// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import lang.TreeOps
import lang.Trees._
import lang.Types._
import tools.FinitePrecision._
import lang.Identifiers._

// GenerateDaisyInput: If we are interested in generating a real valued program, that will later be again used as input to Daisy
// importString: import and package statements inlcuded at top of generated file
class CPrinter(buffer: Appendable, ctx: Context) extends CodePrinter(buffer) {

  val format    =  ctx.option[Precision]("precision")

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {

      case Let(b, Approx(orig, arg, _, _, implName, true), e) =>
        pp(b.getType, p)
        sb.append(" ")
        pp(b, p)
        sb.append(";")
        nl(lvl)
        val fresh = FreshIdentifier("_dummy", lang.Types.RealType, true)
        sb.append("double")
        sb.append(" ")
        pp(fresh, p)
        sb.append(";")
        nl(lvl)

        sb.append(s"$implName(&")
        pp(b, p)
        sb.append(", &")
        pp(fresh, p)
        sb.append(", ")
        pp(arg, p)
        sb.append(");")

        nl(lvl)
        e match {
          case x: Let => ;
          case IfExpr(_, _, _) => ;
          case _ => sb.append("return ")
        }
        pp(e, p)(lvl)
        e match {
          case x: Let => ;
          case _ => sb.append(";")
        }

      case Let(b, Approx(orig, arg, _, _, implName, false), e) if (b.getType == FinitePrecisionType(Float32)) =>
        // double _dummy;
        // f_approx_4_1539098081_d3f7a987cd91d22c691a9da3bcfc2b6a_(&_dummy, x1);
        // float _tmp = (float) _dummy;
        val fresh = FreshIdentifier("_dummy", lang.Types.RealType, true)

        sb.append("double")
        sb.append(" ")
        pp(fresh, p)
        sb.append(";")
        nl(lvl)

        sb.append(s"$implName(&")
        pp(fresh, p)
        sb.append(", ")
        pp(arg, p)
        sb.append(");")
        nl(lvl)

        pp(b.getType, p)
        sb.append(" ")
        pp(b, p)
        sb.append(" = (float) ")
        pp(fresh, p)
        sb.append(";")
        nl(lvl)

        nl(lvl)
        e match {
          case x: Let => ;
          case IfExpr(_, _, _) => ;
          case _ => sb.append("return ")
        }
        pp(e, p)(lvl)
        e match {
          case x: Let => ;
          case _ => sb.append(";")
        }

      case Let(b, Approx(orig, arg, _, _, implName, false), e) =>
        pp(b.getType, p)
        sb.append(" ")
        pp(b, p)
        sb.append(";")
        nl(lvl)

        sb.append(s"$implName(&")
        pp(b, p)
        sb.append(", ")
        pp(arg, p)
        sb.append(");")

        nl(lvl)
        e match {
          case x: Let => ;
          case IfExpr(_, _, _) => ;
          case _ => sb.append("return ")
        }
        pp(e, p)(lvl)
        e match {
          case x: Let => ;
          case _ => sb.append(";")
        }



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
          case IfExpr(_, _, _) => ;
          case _ => sb.append("return ")
        }
        pp(e, p)(lvl)
        e match {
          case x: Let => ;
          case _ => sb.append(";")
        }

      case Cast(expr, t) =>
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

      case APFixedType(tot, int) => sb.append(s"ap_fixed<$tot,$int>")

      case x @ FinitePrecisionLiteral(r, Float32, strVal) =>
        if (strVal.contains(".") || strVal.contains("e") || strVal.contains("E")) { //valid float number
          sb.append(strVal + "f")
        } else {
          sb.append(strVal + ".0f")
        }

      case Program(id, defs) =>
        assert(lvl == 0)
        if (ctx.option[List[String]]("comp-opts").nonEmpty &&
            defs.exists(_.body.exists(TreeOps.exists { case FMA(_, _, _) => true }))){
          sb.append(
            """#pragma STDC FP_CONTRACT OFF
              |#if !__FMA__ && !__FMA4__
              |  #pragma message("Fast FMA not supported by architecture. Supported architectures: >=haswell (FMA3), >=bdver1 (FMA4)'")
              |#endif
              |
              |""".stripMargin)
        }
        sb.append("#include <math.h>\n")
        if (ctx.hasFlag("apfixed") || (ctx.hasFlag("mixed-tuning") && ctx.fixedPoint)) {
          sb.append("#include <ap_fixed.h>\n")
        } else if (ctx.hasFlag("metalibm")) {
          sb.append("""#include "expansion.h"""")
          sb.append("\n")
        }

        if (defs.flatMap(_.body).exists(
          TreeOps.exists{ case e => e.getType match {
            case FinitePrecisionType(FloatPrecision(a)) => a >= 128
            case FinitePrecisionType(FixedPrecision(a)) => a >= 64
            case _ => false }})) {
          sb.append("#include <qd/dd_real.h>\n")
        }

        if (ctx.hasFlag("metalibm")) {
          sb.append(ctx.metalibmWrapperFunctions.mkString("\n"))
           // val prototypes = defs.map(fnc => getPrototypes(fnc.body.get).mkString("")).toList.mkString("")
           // sb.append(s"\n$prototypes")
        }

        nl
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
          sb.append("@pre: ")
          pp(prec, p)(lvl)
          sb.append(" */")
          sb.append("\n")
        }}

        fd.postcondition.foreach{ post => {
          ind
          sb.append("/* ")
          sb.append("@post: ")
          pp(post, p)(lvl)
          sb.append(" */")
          sb.append("\n")
        }}

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
          case Some(body@IfExpr(_, _, _)) =>
            pp(body, p)(lvl+1)
          case Some(body) =>
            sb.append("return ")
            pp(body,p)
            sb.append(";")
          case None =>
            sb.append("[unknown function implementation]")
        }
        nl(lvl)
        sb.append("}")
        if (ctx.resultRealRanges.get(fd.id).isDefined) {
          sb.append(s" // ${ctx.resultRealRanges(fd.id)} +/- ${ctx.resultAbsoluteErrors(fd.id)}")
        }
        nl(lvl - 1)
        nl(lvl - 1)

      //case Approx(_, expr, _, _, fName) => ppMathFun(Seq(expr), fName + "_wrapper")
    case _ => super.pp(tree, parent)
    }
  }

  private def getLastExpression(e: Expr): Expr = e match {
    case Let(_, _, body) => getLastExpression(body)
    case _ => e
  }

}
