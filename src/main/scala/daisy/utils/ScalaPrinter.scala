// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import daisy.lang.Identifiers.FreshIdentifier
import daisy.lang.TreeOps.{isMatrix, isScalar, isVector}
import daisy.opt.ApproxPhase.functionsToConsider
import lang.Trees._
import lang.Types.{Int32Type, _}
import tools.FinitePrecision.{DoubleDouble, Float32, Float64, MatrixFloat64, VectorFloat64}

import java.io.{BufferedWriter, FileWriter}

class ScalaPrinter(buffer: Appendable, ctx: Context,
  val generateDaisyInput: Boolean = false, val importString: String = "") extends CodePrinter(buffer) {

  override val mathPrefix: String = ""

  def toJson(iter: Any): String = iter match {
    case m: Map[_,_] =>
      val m_str = m.map({case (key, value) => toJson(key) + ": " + toJson(value)}).mkString(",\n")
      "{\n "+ m_str + " }\n"
    case s: Seq[_] =>
      val s_str = s.map(toJson).mkString(",\n")
      "[ "+ s_str + " ]\n"
    case num: Double => "\"" + num.toString + "\"" // todo need quotes around? some special treatment?
    case num: Int => "\"" + num.toString + "\"" // todo need quotes around? some special treatment?
    case all => "\"" + all.toString + "\""
  }

  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    implicit val p = Some(tree)

    tree match {
      case VectorFromList(lst, _) =>
        ppNary(lst, "List(", ",", ")")
      case MatrixFromLists(lsts, numRows, numCols) =>
        val sz = lsts.size
        var c = 0
        sb.append("List(")
        lsts.foreach(lst => {
          ppNary(lst, "List(", ",", ")")
          c+=1
          if (c < sz) {sb.append(",")}
        })
        sb.append(")")

      case SizeLength(t) =>
        pp(t, p)
        sb.append(".length") // or .size for all Traversable

      case SizeNumRows(t) => // number of elements on the top level
        pp(t, p)
        sb.append(".length")
      case SizeNumCols(t) => // number of elements on the nested level
        pp(t, p)
        sb.append(".head.length")
      case SubVector(v,from,to) =>
        pp(v, p)
        sb.append(".slice(")
        pp(from, p)
        sb.append(", ")
        pp(to, p) // todo change the semantics of our DSL? in Scala slice doesn't contain the element with index "to"
        sb.append("+1)")

      case EveryNthVector(ds, n, from) =>
        pp(ds, p)
        sb.append(".drop(")
        pp(from, p)
        sb.append(").grouped(")
        pp(n, p)
        sb.append(").map(_.head).toList")

      case EveryNthMatrix(ds, n, from) =>
        pp(ds, p)
        sb.append(".drop(")
        pp(from, p)
        sb.append(").grouped(")
        pp(n, p)
        sb.append(").map(_.head).toList")

      case VectorElement(v, ind) =>
        pp(v, p)
        sb.append("(")
        pp(ind, p)
        sb.append(")")

      case MatrixElement(m, irow, icol) =>
        pp(m, p)
        sb.append("(")
        pp(irow, p)
        sb.append(")(")
        pp(icol, p)
        sb.append(")")

      case RowOfMatrix(m, irow) =>
        pp(m, p)
        sb.append("(")
        pp(irow, p)
        sb.append(")")

      case MapIter(v, Lambda(args, body)) =>
        pp(v, p)
        sb.append(".map(")
        if (args.size > 1) {
          sb.append("{ case ")
          ppNary(args, "(", ",", ") => ")
        } else {
          pp(args.head,p)
          sb.append(" => {")
        }
        nl(lvl + 1)
        pp(body, p)(lvl+1) // indent iterators body
        nl(lvl)
        sb.append("})")

      case FoldIter(v, init, Lambda(args, body)) =>
        pp(v, p)
        sb.append(".foldLeft(")
        pp(init, p)
        sb.append(")(")
        ppNary(args, "(", ",", ") => {")
        nl(lvl + 1)
        pp(body, p)(lvl+1) // indent iterators body
        nl(lvl)
        sb.append("})")

      case FoldElemsIter(v, init, Lambda(args, body)) =>
        // m.foldLeft(0.0)({case (acc, trow) => trow.foldLeft(acc)({case (accel, el) => f(accel,el) }) })
        pp(v, p)
        sb.append(".foldLeft(")
        pp(init, p)
        sb.append(")({ case (_acc_, _mrow_) => ")
        nl(lvl+1)
        sb.append("_mrow_.foldLeft(_acc_)({case ")
        ppNary(args, "(", ",", ") => ")
        nl(lvl + 2)
        pp(body, p)(lvl+2) // indent iterators body
        sb.append("})")
        nl(lvl)
        sb.append("})")

      //case Sum(v, init) if isVector(v) =>
      //  // should never happen, because we replaced it with the fold
      //  ???

      case x@SlideReduceIter(ds, size, step, Lambda(args, body)) if isVector(ds) =>
        pp(ds, p)
        sb.append(".sliding(")
        pp(size, p)
        sb.append(",")
        pp(step, p)
        sb.append(").map( ") // _tmp_sl_red_
        ppNary(args, "(", ",", ") => {")
        nl(lvl + 1)
        pp(body, p)(lvl+1) // indent iterators body
        nl(lvl)
        sb.append("}).toList")

      case x@SlideReduceIter(ds, size, step, Lambda(args, body)) =>
        // ds.transpose.sliding(size,step)
        //   .flatMap(win => win.flatMap( r => r.sliding(
        //   size
        //   ,
        //   step
        //   ).toList)).toList.transpose.flatten.grouped(
        //   size
        //   ).toList.grouped(
        //   size
        //   ).map(r => r.transpose.toList).toList
        pp(ds, p)
        sb.append(".transpose.sliding(")
        pp(size, p)
        sb.append(",")
        pp(step, p)
        sb.append(").flatMap(win => ")
        nl(lvl + 1)
        sb.append("win.flatMap( r => r.sliding( ")
        pp(size, p)
        sb.append(",")
        pp(step, p)
        sb.append(").toList)).toList.transpose.flatten.grouped(")
        pp(size, p)
        sb.append(").toList.grouped(")
        pp(size, p)
        sb.append(").map(r => r.transpose.toList).toList.map(")

        ppNary(args, "(", ",", ") => {")
        pp(body, p)(lvl+2) // indent iterators body
        nl(lvl)
        // group back into a matrix (from a list of windows)
        // .grouped((padded.head.size - 3)/1+1).toList
        sb.append("}).grouped((")
        pp(ds, Some(ds))
        sb.append(".head.size - ")
        pp(size, Some(ds))
        sb.append(")/")
        pp(step, Some(ds))
        sb.append("+1).toList")

      case EnumSlideFlatMap(ds, size, Lambda(args, body)) =>
        pp(ds, p)
        sb.append(".sliding(")
        pp(size, p)
        sb.append(",")
        pp(size, p)
        sb.append(").zipWithIndex.flatMap({ case  ") // _tmp_sl_red_
        ppNaryWithType(Seq(args(1), args.head), "(", ",", ") => ")
        nl(lvl + 1)
        pp(body, p)(lvl+1) // indent iterators body
        nl(lvl)
        sb.append("}).toList")

      case EnumRowsAndMap(ds, Lambda(args, body)) =>
        sb.append("(")
        pp(ds, p)
        sb.append(").zipWithIndex.map({ case  ") // _tmp_sl_red_
        ppNaryWithType(Seq(args(1), args.head), "(", ",", ") => ")
        nl(lvl + 1)
        pp(body, p)(lvl+1) // indent iterators body
        nl(lvl)
        sb.append("}).toList")

      case Concat(lhs, rhs) => ppBinary(lhs, rhs, " ++ ")
      case ZipVectors(lhs, rhs) =>
        sb.append("(")
        pp(lhs, p)
        sb.append(").zip(")
        pp(rhs, p)
        sb.append(").map(_.toList)")

      case FlipUpsideDown(m) =>
        sb.append("(")
        pp(m, p)
        sb.append(").reverse")

      case FlipLeftToRight(m) =>
        sb.append("(")
        pp(m, p)
        sb.append(").map(_.reverse)")

      case PadMatrix(m@MatrixLiteral(id), rows, cols) =>
         // List.fill(rows)(List.fill(cols+m.numCols)(0.0))
        // padding above the rows
        sb.append("List.fill(")
        pp(rows, Some(m))
        sb.append(")(List.fill(2*")
        pp(cols, Some(m))
        sb.append(" + ")
        pp(m, p)
        sb.append(".head.size)(0.0)) ++ ") // number of columns of m
        // the matrix itself padded with extra columns
        pp(m, p)
        sb.append(".map(_r_ => List.fill(") // todo unique name?
        pp(cols, Some(m))
        sb.append(")(0.0) ++ _r_ ++ List.fill(")
        pp(cols, Some(m))
        sb.append(")(0.0))")
        // lower padding
        sb.append(" ++ List.fill(")
        pp(rows, Some(m))
        sb.append(")(List.fill(2*")
        pp(cols, Some(m))
        sb.append(" + ")
        pp(m, p)
        sb.append(".head.size)(0.0)) ") // number of columns of m

      case PadMatrix(m, rows, cols) =>
         // todo if matrix to be padded is an expression -> transform into let stmt with tmp matrixliteral
      ???

      case PadVector(v, size) =>
        sb.append("List.fill(")
        pp(size, Some(v))
        sb.append(")(0.0) ++ ")
        pp(v, p)
        sb.append(" ++ List.fill(")
        pp(size, Some(v))
        sb.append(")(0.0)")

      case MaxOf(m) if isMatrix(m) =>
        // m.foldLeft(m.head.head)({case (acc, r) => math.max(acc,r.max)})
        pp(m, p)
        sb.append(".foldLeft(")
        pp(m, p)
        sb.append(".head.head)({case (_acc_, _r_) => math.max(_acc_,_r_.max)})")
      case MinOf(m) if isMatrix(m) =>
        pp(m, p)
        sb.append(".foldLeft(")
        pp(m, p)
        sb.append(".head.head)({case (_acc_, _r_) => math.min(_acc_,_r_.min)})")

      case MaxOf(v) if isVector(v) =>
        pp(v, p)
        sb.append(".max")
      case MinOf(v) if isVector(v) =>
        pp(v, p)
        sb.append(".min")

      case Sqrt(t) if isVector(t) => ppUnaryVector(t, "sqrt")
      case Sin(t) if isVector(t) => ppUnaryVector(t, "sin")
      case Cos(t) if isVector(t) => ppUnaryVector(t, "cos")
      case Tan(t) if isVector(t) => ppUnaryVector(t, "tan")
      case Exp(t) if isVector(t) => ppUnaryVector(t, "exp")
      case Asin(t) if isVector(t) => ppUnaryVector(t, "asin")
      case Acos(t) if isVector(t) => ppUnaryVector(t, "acos")
      case Atan(t) if isVector(t) => ppUnaryVector(t, "atan")
      case IntPow(t, pow) if isVector(t) =>
        pp(t, p)
        sb.append(s".map(_tmpi => pow(_tmpi, $pow))")
      case Sqrt(t) if isMatrix(t) => ppUnaryMatrix(t, "sqrt")
      case Sin(t) if isMatrix(t) => ppUnaryMatrix(t, "sin")
      case Cos(t) if isMatrix(t) => ppUnaryMatrix(t, "cos")
      case Tan(t) if isMatrix(t) => ppUnaryMatrix(t, "tan")
      case Exp(t) if isMatrix(t) => ppUnaryMatrix(t, "exp")
      case Asin(t) if isMatrix(t) => ppUnaryMatrix(t, "asin")
      case Acos(t) if isMatrix(t) => ppUnaryMatrix(t, "acos")
      case Atan(t) if isMatrix(t) => ppUnaryMatrix(t, "atan")
      case IntPow(t, pow) if isMatrix(t) =>
        pp(t, p)
        sb.append(s".map(_r_ => _r_.map(_tmpi => pow(_tmpi, $pow)))")
      case Log(t) if isVector(t) => // todo which base?
        // Scala uses ln (in the benchmarks write explicitly to get log base 2 log(x)/log(2) )
        //sb.append("(")
        ppUnaryVector(t, "log")
        //sb.append("/ log(2))")

      case Times(l,r) if isVector(l) && isVector(r) =>
        ppBinaryVectors(l, r, " * ")
      case Times(l,r) if isVector(l) && isScalar(r) => ppVectorAndScalar(l, r, " * ")
      case Times(l,r) if isMatrix(l) && isMatrix(r) => ppBinaryMatrix(l, r, " * ")
      case Times(l,r) if isMatrix(l) && isScalar(r) => ppMatrixAndScalar(l, r, " * ")
      
      case Plus(l,r) if isVector(l) && isVector(r) => ppBinaryVectors(l, r, " + ")
      case Plus(l,r) if isVector(l) && isScalar(r) => ppVectorAndScalar(l, r, " + ")
      case Plus(l,r) if isMatrix(l) && isMatrix(r) => ppBinaryMatrix(l, r, " + ")
      case Plus(l,r) if isMatrix(l) && isScalar(r) => ppMatrixAndScalar(l, r, " + ")
      
      case Minus(l,r) if isVector(l) && isVector(r) => ppBinaryVectors(l, r, " - ")
      case Minus(l,r) if isVector(l) && isScalar(r) => ppVectorAndScalar(l, r, " - ")
      case Minus(l,r) if isMatrix(l) && isMatrix(r) => ppBinaryMatrix(l, r, " - ")
      case Minus(l,r) if isMatrix(l) && isScalar(r) => ppMatrixAndScalar(l, r, " - ")
      
      case Division(l,r) if isVector(l) && isVector(r) => ppBinaryVectors(l, r, " / ")
      case Division(l,r) if isVector(l) && isScalar(r) => ppVectorAndScalar(l, r, " / ")
      case Division(l,r) if isMatrix(l) && isMatrix(r) => ppBinaryMatrix(l, r, " / ")
      case Division(l,r) if isMatrix(l) && isScalar(r) => ppMatrixAndScalar(l, r, " / ")

      case CrossProduct(lhs, rhs) if isVector(lhs) && isVector(rhs) =>
      /// special case (multiplication of complex numbers)
      // (a+ib)(c+id) = ac-bd + i(ad+bc)
        val a = VectorElement(lhs, Int32Literal(0))
        val b = VectorElement(lhs, Int32Literal(1))
        val c = VectorElement(rhs, Int32Literal(0))
        val d = VectorElement(rhs, Int32Literal(1))
        val firstElt = Minus(Times(a,c), Times(b,d))
        val secondElt = Plus(Times(a,d), Times(b,c))
        val t = VectorFromList(Seq(firstElt, secondElt), 2)
        pp(t, p)

      case CrossProduct(lhs, rhs) if isMatrix(lhs) && isVector(rhs) =>
      // for (row <- lhs)
        //        yield row.zip(rhs).map.(Function.tupled(_*_)).reduceLeft(_+_)
        // todo what to do if there's a complex expression?
        sb.append("(for (row <- ")
        pp(lhs, p)
        sb.append(") yield ")
        nl(lvl+1)
        sb.append("row.zip(")
        pp(rhs, p)
        sb.append(").map(Function.tupled(_*_)).reduceLeft(_+_))")
        nl(lvl)

      case CrossProduct(lhs, rhs) if isMatrix(lhs) && isMatrix(rhs) =>
      // for (row <- lhs)
       //  yield for(col <- rhs.transpose)
        //        yield row.zip(col).map(Function.tupled(_*_)).reduceLeft(_+_)
        sb.append("(for (row <- ")
        pp(lhs, p)
        sb.append(") yield for(col <- ")
        pp(rhs, p)
        sb.append("rhs.transpose) yield ")
        nl(lvl+1)
        sb.append("row.zip(col).map(Function.tupled(_*_)).reduceLeft(_+_))")
        nl(lvl)

      case Let(b,d,e) =>
        sb.append("val ")
        pp(b, p)
        sb.append(": ")
        pp(b.getType,p)
        sb.append(" = ")
        pp(d, p)
        nl(lvl)
        pp(e, p)(lvl)

      case Sqrt(x) => if (generateDaisyInput || x.getType == FinitePrecisionType(DoubleDouble)) {
        ppUnary(x, "sqrt(", ")")
      } else {
        ppUnary(x, "math.sqrt(", ")")
      }

      case FinitePrecisionType(DoubleDouble) => sb.append("DblDouble")
      case FinitePrecisionType(p@VectorFloat64) => sb.append(p.toString)
      case FinitePrecisionType(p@MatrixFloat64) => sb.append(p.toString)

      case Cast(expr, FinitePrecisionType(Float32)) => ppUnary(expr, "", ".toFloat")
      case Cast(expr, FinitePrecisionType(Float64)) => ppUnary(expr, "", ".toDouble")
      case Cast(expr, FinitePrecisionType(DoubleDouble)) => ppUnary(expr, "DblDouble(", ")")
      case Cast(expr, Int32Type) => ppUnary(expr, "", ".toInt")
      case Cast(expr, Int64Type) => ppUnary(expr, "", ".toLong")

      case prg@Program(id, defs) =>
        assert(lvl == 0)
        sb.append("import scala.annotation.strictfp\n")
        sb.append("import scala.math._\n")
        sb.append(importString)
        nl(lvl)
        sb.append("@strictfp\n")
        sb.append("object ")
        if (ctx.hasFlag("randomIns")) sb.append("Main") else pp(id, p)
        sb.append(" {\n")
        nl(lvl)
        defs.foreach {
          m => pp(m, p)(lvl + 1)
        }
        if (ctx.hasFlag("randomIns")) {
          // random inputs
          sb.append(
              raw"""
                   |    def main(args: Array[String]): Unit = {
                   |""".stripMargin)

          import util.Random._
          functionsToConsider(ctx, prg).foreach(fnc => {
            val dsas = ctx.dsAbstractions.getOrElse(fnc.id, Map()) // empty map for functions without DS
            val args = fnc.params.map(arg => {
              val values = arg.getType match {
                case VectorType(_) =>
                  val values = dsas(VectorLiteral(arg.id)).conseqGroupsWRepresentative().flatMap({
                    case ((n, _), range) =>
                      val lo = range.xlo.doubleValue()
                      val up = range.xhi.doubleValue()
                      List.fill(n)(randomValue(lo,up))
                  }).toList
                  val size = dsas(VectorLiteral(arg.id)).dsSize
                  (values, Seq(size))
                case MatrixType(_) =>
                  val rows = dsas(MatrixLiteral(arg.id)).numRows
                  val cols = dsas(MatrixLiteral(arg.id)).numCols
                  val values = dsas(MatrixLiteral(arg.id)).conseqGroupsWRepresentative().flatMap({
                    case ((n, _), range) =>
                      val lo = range.xlo.doubleValue()
                      val up = range.xhi.doubleValue()
                      List.fill(n)(randomValue(lo,up))
                  }).grouped(cols).toList
                  (values, Seq(rows, cols))
                case FinitePrecisionType(_) =>
                  val range = ctx.specInputRanges(fnc.id)(arg.id)
                  val lo = range.xlo.doubleValue()
                  val up = range.xhi.doubleValue()
                  val value = randomValue(lo, up)
                  (value, Seq(1))
                case Int32Type =>
                  val range = ctx.specInputRanges(fnc.id)(arg.id)
                  val lo = range.xlo.intValue()
                  val up = range.xhi.intValue()
                  val value = if (lo == up) lo else between(lo, up)
                  (value, Seq(1))
              }
              (arg.id -> values)
            })
            args.foreach(arg => {
              sb.append(s"val ${arg._1.uniqueName} = ${arg._2._1}")
              nl(lvl+1)
            })
            val resVar = Variable(FreshIdentifier("res", fnc.returnType))
            sb.append(s"val ${resVar.id.uniqueName}:${fnc.returnType} = ${fnc.id.name}")
            val uniqueArgs = args.map(a => a._1.uniqueName)
            val sz = uniqueArgs.size
            var c = 0
            sb.append("(")
            uniqueArgs.foreach(ex => {
              sb.append(ex)
              c += 1;
              if (c < sz) {
                sb.append(",")
              }
            })
            sb.append(")")

            nl(lvl+1)
            // todo add a flag to decide whether println is needed?
            //  in general good to have, but not for performance measurements
            //sb.append("println(\"Fnc " + s"${fnc.id.name} output: ${resVar.id.uniqueName}" + " = \" + " + resVar.id.uniqueName + ")")
            nl(lvl+1)

            // save generated input values to json file
             if (ctx.hasFlag("inputsToJson")) {
               val filename = System.getProperty("user.dir")+"/output/" + fnc.id + ".json"
               val fstream = new FileWriter(filename)
               val out = new BufferedWriter(fstream)
               val argMap = args.map({
                 case (name, (value, size)) =>
                   Map[String, Any]("name" -> name, "type" -> name.getType, "size" -> size, "value" -> value)
               }).toSeq // todo check if toSeq is needed
               val map = Map("fnc" -> fnc.id.name.toString, "inputs" -> argMap)
               val jsonString = toJson(map)
               //println(jsonString)
               out.append(jsonString)
               out.close()
             }
          })
          // end main
          sb.append("  }\n")
        } else if (ctx.hasFlag("genMain")){
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
        }

        nl(lvl)

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
        if (generateDaisyInput){
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

  //private def randomValue[A](lo:Numeric[A], up:Numeric[A])(implicit n: Numeric[A]) :Numeric[A] = {
  private def randomValue(lo:Double, up:Double): Double = {
    import util.Random._
    if (lo == up) lo else if (lo < up) between(lo, up) else throw DaisyFatalError(Some(s"interval has reversed bounds [$lo, $up]"))
  }

}