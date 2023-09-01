// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import daisy.lang.Identifiers._
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, Program => DaisyProgram, ValDef => DaisyValDef, _}
import daisy.lang.Types.{TypeTree => DaisyType, _}
import daisy.tools.Rational
import daisy.utils.{DefinedPosition, Positioned, OffsetPosition => DaisyOffsetPosition, Position => DaisyPosition, RangePosition => DaisyRangePosition}

import java.io.File
import scala.collection.immutable.Seq
import scala.language.implicitConversions
import scala.reflect.internal.util._


trait CodeExtraction extends ASTExtractors {
  self: DaisyExtraction =>

  import StructuralExtractors._
  import global._
  import global.definitions._

  val ctx: Context

  implicit val debugSection = DebugSectionFrontend

  implicit def scalaPosToDaisyPos(p: global.Position): DaisyPosition = {
    if (p == NoPosition) {
      daisy.utils.NoPosition
    } else if (p.isRange) {
      val start = p.focusStart
      val end   = p.focusEnd
      DaisyRangePosition(start.line, start.column, start.point,
        end.line, end.column, end.point, p.source.file.file)
    } else {
      DaisyOffsetPosition(p.line, p.column, p.point, p.source.file.file)
    }
  }

  def daisyPosToScalaPos(spos: global.Position, p: DaisyPosition): global.Position = {
    (spos, p) match {
      case (NoPosition, _) =>
        NoPosition

      case (p, dp: DefinedPosition) =>
        new OffsetPosition(p.source, dp.focusBegin.point)

      case _ =>
        NoPosition

    }
  }

  /**
    An exception thrown when non-purescala compatible code is encountered.
    Unlike Leon, we will always abort if there is a problem.
   */
  sealed class OutOfSubsetException(pos: Position, msg: String, ot: Option[Tree]) extends Exception(msg) {
    def emit(): Unit = {
      val debugInfo =
        ot.map { t =>
          val strWr = new java.io.StringWriter()
          new global.TreePrinter(new java.io.PrintWriter(strWr)).printTree(t)
          " (Tree: " + strWr.toString + " ; Class: " + t.getClass + ")"
        }.getOrElse("")

      ctx.reporter.error(pos, msg + debugInfo)
    }
  }

  def outOfSubsetError(pos: Position, msg: String) = {
    throw new OutOfSubsetException(pos, msg, None)
  }

  def outOfSubsetError(t: Tree, msg: String) = {
    throw new OutOfSubsetException(t.pos, msg, Some(t))
  }

  // Simple case class to capture function definitions gradually
  // there are later copied over to the immutable FunDef class
  case class TmpFunDef(id: Identifier, params: Seq[DaisyValDef], returnType: DaisyType,
    varMap: Map[Symbol, Identifier]) extends Positioned

  class Extraction(units: List[CompilationUnit]) {
      if (units.isEmpty)
          throw new Exception(s"Could not compile source file ${ctx.file}")

    // We need this map to make sure that all Variables that should be the same
    // actually have the same ID
    case class DefContext(vars: Map[Symbol, Identifier] = Map()){

      def isVariable(s: Symbol): Boolean = vars.contains(s)

      def withNewVars(nvars: Traversable[(Symbol, Identifier)]): DefContext = {
        copy(vars = vars ++ nvars)
      }

      def withNewVar(nvar: (Symbol, Identifier)): DefContext = {
        copy(vars = vars + nvar)
      }

    }

    var defsToDefs: Map[Symbol, TmpFunDef] = Map()

    def extractProgram: Option[DaisyProgram] = {

      try {

        val nonLibUnit = units.filter { u => !isLibrary(u) }
        assert(nonLibUnit.size == 1)
        val defs = nonLibUnit.head.body match {
          case PackageDef(_, body) => body
        }

        // Step 1: discover all FunDefs
        defs.head match {
          case t @ ExObjectDef(n, templ) =>
            // Module
            // val id = FreshIdentifier(n)
            templ.body.foreach {

              // Functions
              case ExFunctionDef(sym, _, _, _,rhs) =>
                val tmpDef = extractFunDef(sym)
                defsToDefs = defsToDefs + (sym -> tmpDef)

              case x @ ExConstructorDef() =>
                None

              case tree =>
                ctx.reporter.error(tree)
                outOfSubsetError(tree, "Unexpected def in top-level object of class: " + tree.getClass);
            }
            t
          case x =>
            outOfSubsetError(NoPosition, "Unexpected top-level object found: " + x)
        }

        // ctx.reporter.info("defsToDefs: " + defsToDefs)

        // Step 2: fill in function bodies
        val daisyProgram = defs.head match {
          case t @ ExObjectDef(n, templ) =>
            val id = FreshIdentifier(n)
            val funDefs = templ.body.flatMap {
              case ExFunctionDef(sym, _, _, _, rhs) =>
                // ctx.reporter.info("going to extract: " + sym)
                Some(extractFunBody(sym, rhs))
              case _ => None
            }
            DaisyProgram(id, funDefs)
        }

        ctx.reporter.debug("Extracted the following program:")
        ctx.reporter.debug(daisyProgram)

        Some(daisyProgram)
      } catch {
        case e: OutOfSubsetException =>
          e.emit()
          None
      }

    }

    // This one never fails, on error, it returns Untyped
    def daisyType(tpt: Type)(implicit pos: Position): DaisyType = {
      try {
        extractType(tpt)
      } catch {
        case e: OutOfSubsetException =>
          e.emit()
          Untyped
      }
    }



    private def isLibrary(u: CompilationUnit) = {
      // TODO: big hack
      u.source.file.absolute.path.endsWith(new File(self.ctx.libFiles.head).getAbsolutePath()) ||
        u.source.file.absolute.path.endsWith(new File(self.ctx.libFiles(1)).getAbsolutePath())
    }

    // private def extractPackageRef(refPath: RefTree): PackageRef = {
    //   (getSelectChain(refPath.qualifier) :+ refPath.name.toString).filter(_ != "<empty>")
    // }

    private def extractType(t: Tree): DaisyType = {
      extractType(t.tpe)(t.pos)
    }

    private def extractType(tpt: Type)(implicit pos: Position): DaisyType = tpt match {
      // case tpe if tpe == CharClass.tpe =>
      //   CharType

      case tpe if tpe == IntClass.tpe =>
        Int32Type

      case tpe if tpe == BooleanClass.tpe =>
        BooleanType

      // case tpe if tpe == UnitClass.tpe =>
      //   UnitType

      case tpe if tpe == NothingClass.tpe =>
        Untyped

      case ct: ConstantType =>
        extractType(ct.value.tpe)

      // case TypeRef(_, sym, _) if isBigIntSym(sym) =>
      //   IntegerType

      case TypeRef(_, sym, _) if isRealSym(sym) =>
        RealType

      case TypeRef(_, sym, args) if sym.toString == "class Tuple2" =>
        TupleType(args map { x => extractType(x) })

      case TypeRef(_, sym, args) if sym.toString == "class Tuple3" =>
        TupleType(args map { x => extractType(x) })

      case TypeRef(_, sym, args) if sym.toString == "class Vector" =>
        if (args.nonEmpty)
          VectorType(args map { x => extractType(x) })
        else
          VectorType(Seq(RealType))

      case SingleType(_, sym) if sym.toString == "object Vector" =>
        VectorType(Seq(RealType))

      case TypeRef(_, sym, args) if sym.toString == "class Matrix" =>
        if (args.nonEmpty)
          MatrixType(args map { x => extractType(x) })
        else
          MatrixType(Seq(RealType))

      case SingleType(_, sym) if sym.toString == "object Matrix" =>
        MatrixType(Seq(RealType))

      case MethodType(_, TypeRef(_,sym,Nil)) if sym.toString() == "class Vector" =>
        // todo other options except RealType
        VectorType(Seq(RealType))

      case MethodType(_, TypeRef(_,sym,Nil)) if sym.toString() == "class Matrix" =>
        // todo other options except RealType
        MatrixType(Seq(RealType))

      case TypeRef(_, sym , args) if sym.toString() == "type List" =>
        val innerTpe = args.map(extractType)
        VectorType(innerTpe)

      case _ =>
        if (tpt ne null) {
          outOfSubsetError(tpt.typeSymbol.pos, "Could not extract type as PureScala: " +
            tpt + " (" + tpt.getClass + ")")
        } else {
          outOfSubsetError(NoPosition, "Tree with null-pointer as type found")
        }
    }

    private def extractFunDef(sym: Symbol): TmpFunDef = {

      var varMap = Map[Symbol, Identifier]()

      val params = sym.info.paramss.flatten.map { sym =>
        val ptpe = daisyType(sym.tpe)(sym.pos)
        // TODO: need this?
        val tpe = if (sym.isByNameParam) FunctionType(Seq(), ptpe) else ptpe
        val newID = FreshIdentifier(sym.name.toString, tpe).setPos(sym.pos)
        val vd = DaisyValDef(newID).setPos(sym.pos)

        varMap = varMap + (sym -> newID)
        vd
      }

      val returnType = daisyType(sym.info.finalResultType)(sym.pos)

      // TODO: what's this?
      // val idType = {
      //   val argTypes = params map { _.getType }
      //   if (argTypes.nonEmpty) FunctionType(argTypes, returnType)
      //   else returnType
      // }

      val id = FreshIdentifier(sym.name.toString.trim, returnType)
      id.setPos(sym.pos)


      TmpFunDef(id, params, returnType, varMap)
    }

    private def extractFunBody(sym: Symbol, body0: Tree): DaisyFunDef = {

      /** Extracts the body without its specification
       *
       * [[Expressions.Expr]] trees contain its specifications as part of certain nodes.
       * This function helps extracting only the body part of an expression
       *
       * @return An option type with the resulting expression if not [[Expressions.NoTree]]
       * @see [[Expressions.Ensuring]]
       * @see [[Expressions.Require]]
       */
      def withoutSpec(expr: DaisyExpr): Option[DaisyExpr] = expr match {
        // TODO ???
        // case Let(i, e, b)                    => withoutSpec(b).map(Let(i, e, _))
        case Require(pre, b)                 => Option(b).filterNot(_.isInstanceOf[NoTree])
        case Ensuring(Require(pre, b), post) => Option(b).filterNot(_.isInstanceOf[NoTree])
        case Ensuring(b, post)               => Option(b).filterNot(_.isInstanceOf[NoTree])
        case b                               => Option(b).filterNot(_.isInstanceOf[NoTree])
      }

      /** Returns the precondition of an expression wrapped in Option */
      def preconditionOf(expr: DaisyExpr): Option[DaisyExpr] = expr match {
        // case Let(i, e, b)                 => preconditionOf(b).map(Let(i, e, _).copiedFrom(expr))
        case Require(pre, _)              => Some(pre)
        case Ensuring(Require(pre, _), _) => Some(pre)
        case b                            => None
      }

      /** Returns the postcondition of an expression wrapped in Option */
      def postconditionOf(expr: DaisyExpr): Option[DaisyExpr] = expr match {
        // case Let(i, e, b)      => postconditionOf(b).map(Let(i, e, _).copiedFrom(expr))
        case Ensuring(_, post) => Some(post)
        case _                 => None
      }

      val tmpFunDef = defsToDefs(sym)

      // TODO: flags
      // fd.addFlags(annotationsOf(sym).map { case (name, args) => FunctionFlag.fromName(name, args) }.toSet)
      val flags = false   // this is actually called isField right now

      val returnType = tmpFunDef.returnType

      val fullBody = try {
        extractTreeOrNoTree(body0)(DefContext(tmpFunDef.varMap))
      } catch {
        case e: OutOfSubsetException =>
          e.emit()
          ctx.reporter.error(sym.pos, "Function " + tmpFunDef.id.name + " could not be extracted.")
          NoTree(returnType)
      }

      val body = withoutSpec(fullBody)

      val precondition = preconditionOf(fullBody)
      val postcondition = postconditionOf(fullBody)

      val fd = new DaisyFunDef(tmpFunDef.id, returnType, tmpFunDef.params,
        precondition, body, postcondition, flags)
      fd.setPos(sym.pos)
      fd

    }


    private def extractTreeOrNoTree(tr: Tree)(implicit dctx: DefContext): DaisyExpr = {
      try {
        extractTree(tr)
      } catch {
        case e: OutOfSubsetException =>
          // NoTree(extractType(tr)).setPos(tr.pos)
          throw e
      }
    }

    private def extractTree(tr: Tree)(implicit dctx: DefContext): DaisyExpr = {
      import ExtractorHelpers._
      import lang.Constructors.{and, or}

      val (current, tmpRest) = tr match {
        case Block(Block(e :: es1, l1) :: es2, l2) =>
          (e, Some(Block(es1 ++ Seq(l1) ++ es2, l2)))
        case Block(e :: Nil, last) =>
          (e, Some(last))
        case Block(e :: es, last) =>
          (e, Some(Block(es, last)))
        case Block(Nil, last) =>
          (last, None)
        case e =>
          (e, None)
      }

      var rest = tmpRest

      // ctx.reporter.info("extracting: " + current)

      val res = current match {

        /* ----- Contracts ----- */

        case ExEnsuredExpression(body, contract) =>
          val post = extractTree(contract)

          val b = extractTreeOrNoTree(body)

          val closure = (post: @unchecked) match {
            // case IsTyped(_, BooleanType) =>
            //   val resId = FreshIdentifier("res", b.getType).setPos(post)
            //   Lambda(Seq(DaisyValDef(resId)), post).setPos(post)
            case l: Lambda => l
            // case other =>
            //   val resId = FreshIdentifier("res", b.getType).setPos(post)
            //   Lambda(Seq(LeonValDef(resId)), application(other, Seq(Variable(resId)))).setPos(post)
          }

          Ensuring(b, closure)


        // TODO: depending on how often this Block thing happens,
        // we can just merge it with this extraction (e.g. make it like a let)
        case ExRequiredExpression(contract) =>
          val pre = extractTree(contract)

          val b = rest.map(extractTreeOrNoTree).getOrElse(UnitLiteral())

          rest = None

          Require(pre, b)

        case ExBulkVectorElemRangeSpec(ds, specTuples) =>
          // parsing vector spec as a list
          val vec = extractTree(ds) match {case v:VectorLiteral => v}
          val specList = specTuples.map({
            case (indices, ranges) =>
              val r1 = RealLiteral(tools.Rational.fromString(ranges._1.toString))
              val r2 = RealLiteral(tools.Rational.fromString(ranges._2.toString))
              VectorRange(vec, indices._1, indices._2, r1, r2)})
          if (specList.length < 2)
            specList.head
          else
            And(specList)

        case ExBulkMatrixElemRangeSpec(ds, specTuples) =>
          // parsing matrix spec as a list
          val vec = extractTree(ds) match {case v:MatrixLiteral => v}
          val specList = specTuples.map({
            case (indices, ranges) =>
              val r1 = RealLiteral(tools.Rational.fromString(ranges._1.toString))
              val r2 = RealLiteral(tools.Rational.fromString(ranges._2.toString))
              MatrixRange(vec, indices, r1, r2)})
          if (specList.length < 2)
            specList.head
          else
            And(specList)

        case l @ ExLambdaExpression(args, body) =>
          val vds = args map { vd =>
            val aTpe = extractType(vd.tpt)
            val newID = FreshIdentifier(vd.symbol.name.toString, aTpe)
            DaisyValDef(newID)
          }

          val newVars = (args zip vds).map { case (vd, lvd) =>
            vd.symbol -> lvd.id
          }

          val exBody = extractTree(body)(dctx.withNewVars(newVars))

          Lambda(vds, exBody)

        // range spec for sub-vectors or sub-matrices
        case ExElemRangeSpec(v, iT, jT, loT, upT) =>
          val m = extractTree(v) match {case r: MatrixLiteral => r}
          val i = extractTree(iT) match {case Int32Literal(i) => i}
          val j = extractTree(jT) match {case Int32Literal(i) => i}
          val low = extractTree(loT) match {case r: RealLiteral => r}
          val up = extractTree(upT) match {case r: RealLiteral => r}
          MatrixRange(m, Seq((i,j)), low, up)

        case ExSubRangeSpec(v, indTrees, loT, upT) =>
          val ds = extractTree(v)
          val low = extractTree(loT) match {case r: RealLiteral => r}
          val up = extractTree(upT) match {case r: RealLiteral => r}
          val indices = indTrees.map(tu => {
            val t1 = extractTree(tu._1) match {case Int32Literal(i) => i}
            val t2 = extractTree(tu._2) match {case Int32Literal(i) => i}
            (t1,t2)
          })
          ds.getType match {
            case VectorType(_) =>
              val vec = ds match {case r: VectorLiteral => r}
              VectorRange(vec, indices.head._1, indices.head._2, low, up)
            case MatrixType(_) =>
              val m = ds match {case r: MatrixLiteral => r}
              MatrixRange(m, indices, low, up)
          }
        /* ----- Literals ----- */

        case ExInt32Literal(v) => Int32Literal(v)

        case ExImplicitInt2Real(i) => RealLiteral(tools.Rational(i), i.toString)
          // cast an integer variable to a real one (e.g. if used in an arithmetic operation)
          // todo need to create a different identifier?
        case ExImplicitInt2RealOnVars(lit) =>
          extractTree(lit) match {
            case Variable(id) =>
              val realId = id.changeType(RealType)
              Variable(realId)
          }

        case ExBooleanLiteral(v) => BooleanLiteral(v)

        case ExFloat64Literal(d) => RealLiteral(tools.Rational.fromString(d.toString), d.toString)

        case ExImplicitDouble2Real(d) => RealLiteral(tools.Rational.fromString(d.toString), d.toString)

        case ex @ ExIdentifier(sym, tpt) if dctx.isVariable(sym) =>
          (dctx.vars.get(sym): @unchecked) match {
            case Some(id) =>
              id.getType match {
                case VectorType(_) => VectorLiteral(id).setPos(ex.pos) // size is filled in the spec processing phase
                case MatrixType(_) => MatrixLiteral(id).setPos(ex.pos) // size is filled in the spec processing phase
                case _ => Variable(id).setPos(ex.pos)
              }

          }

        case ex @  ExIdentifier(sym, tpt) =>
          ctx.reporter.info("matched ex id")
          null

        /* ----- Unary ops ----- */

        case ExRealUMinus(e)  => UMinus(extractTree(e))
        case ExSqrt(e)        => Sqrt(extractTree(e))
        case ExSin(e)         => Sin(extractTree(e))
        case ExCos(e)         => Cos(extractTree(e))
        case ExTan(e)         => Tan(extractTree(e))
        case ExAsin(e)        => Asin(extractTree(e))
        case ExAcos(e)        => Acos(extractTree(e))
        case ExAtan(e)        => Atan(extractTree(e))
        case ExExp(e)         => Exp(extractTree(e))
        case ExLog(e)         => Log(extractTree(e))

        // size of vector/matrix
        case ExImplicitSize2Real(len) => SizeLength(extractTree(len))
        case ExAccessTuple(tplTree, at) =>
          val tpl = extractTree(tplTree)
          tpl.getType match {
            case TupleType(_) => TupleElement(tpl, at)
            case _ => throw new Exception(s"Trying to access tuple element on a non-tuple expression ${tpl}: ${tpl.getType}")
          }

        case ExCall(tr, sym, args) if (args.isEmpty) => // methods with no args, e.g. x.length()
          val lhs = extractTree(tr)

          (lhs, sym.name.decoded) match {
            case (IsTyped(a1, VectorType(_)), "length") =>
              SizeLength(a1)
            case (IsTyped(a1, MatrixType(_)), "numRows") =>
              SizeNumRows(a1)
            case (IsTyped(a1, MatrixType(_)), "numCols") =>
              SizeNumCols(a1)
            case (IsTyped(a1, VectorType(_)), "toList") =>
              a1
            case (IsTyped(a1, VectorType(_)), "sum") =>
              Sum(a1, RealLiteral(Rational.zero))
            case (IsTyped(a1, VectorType(_)), "max") =>
              MaxOf(a1)
            case (IsTyped(a1, VectorType(_)), "min") =>
              MinOf(a1)
            case (IsTyped(a1, MatrixType(_)), "max") =>
              MaxOf(a1)
            case (IsTyped(a1, MatrixType(_)), "min") =>
              MinOf(a1)
            case (IsTyped(a1, MatrixType(_)), "min") =>
              MinOf(a1)
            case (IsTyped(a1, MatrixType(_)), "flatten") =>
              FlattenMatrix(a1)
            case (IsTyped(a1, MatrixType(_)), "transpose") =>
              Transpose(a1)
            case (IsTyped(a1, MatrixType(_)), "determinant") =>
              Determinant(a1)
            case (IsTyped(a1, MatrixType(_)), "inverse") =>
              MatrixInverse(a1)
            case (IsTyped(a1, MatrixType(_)), "flipud") =>
              FlipUpsideDown(a1)
            case (IsTyped(a1, MatrixType(_)), "fliplr") =>
              FlipLeftToRight(a1)

              // casts for Reals
            case (IsTyped(a1, RealType), "intValue") =>
              Cast(a1, Int32Type)
          }

        case ExFlatVector(fromTree) =>
          val from = extractTree(fromTree)
          FlatVector(from)

        // Vector.zeroVector(n) or Matrix.zeroMatrix(n,m)
        case ExZeroDS(v, args) =>
          val tpe = extractType(v)
          val zeroV = Seq(RealLiteral(Rational.zero))
          tpe match {
            case VectorType(_) =>
              //val id =  FreshIdentifier(f"zeroV${v.pos.line}.${v.pos.point}", tpe)
              val size = extractTree(args.head)
              size match {
                case Int32Literal(i) => VectorFromList(zeroV, i)
              }

            case MatrixType(_) =>
              //val id =  FreshIdentifier(f"zeroM${v.pos.toString}", tpe)
              val sizeI = extractTree(args.head)
              val sizeJ = extractTree(args(1))
              (sizeI, sizeJ) match {
                case (Int32Literal(i), Int32Literal(j)) => MatrixFromLists(Seq(zeroV), i, j)
              }
          }


        // Vector(List(0.0,...)) and Matrix(List(List(1.5,...),...))
        case ExConstructDSFromList(tpe, args) =>
          val tpt = extractType(tpe)
          (tpt, args) match {
            case (VectorType(_), Seq(values)) =>
              //val id = FreshIdentifier(f"newV${current.pos.line}.${current.pos.point}", tpt)
              val exprs = values.map(extractTree)
              VectorFromList(exprs, exprs.length)
            case (MatrixType(_), _) =>
              //val id = FreshIdentifier(f"newM${current.pos.line}.${current.pos.point}", tpt)
              val consts = args.map(row => row.map(extractTree))
              MatrixFromLists(consts, args.length, args.head.length)
          }

        case ExConstructDSFromExpr(tpe, fncApp) =>
          val tpt = extractType(tpe)
          val arg = extractTree(fncApp)

          tpt match {
            case VectorType(_) => VectorFromExpr(arg)
            case MatrixType(_) => MatrixFromExpr(arg)
          }

        case ExHeadElement(v) =>
            val ds = extractTree(v)
            ds.getType match {
                case VectorType(_) => VectorElement(ds, Int32Literal(0))
                case MatrixType(_) => MatrixElement(ds, Int32Literal(0), Int32Literal(0))
            }

        case ExElementAt(v, i ,someJ) =>
            val ds = extractTree(v)
            val at = extractTree(i)
            ds.getType match {
                case VectorType(_) =>
                  VectorElement(ds, at)
                case MatrixType(_) =>
                    val j = someJ match {
                        case Some(x) => extractTree(x)
                    }
                    MatrixElement(ds, at, j)
            }

        case ExSizeSpec(v, sTrees) =>
          val ds = extractTree(v)
          val sizes = sTrees.map(e => extractTree(e) match {case Int32Literal(ii) => ii})
          ds.getType match {
            case VectorType(_) => SizeLessEquals(ds, sizes.head, -1)
            case MatrixType(_) => SizeLessEquals(ds, sizes.head, sizes(1))
          }

        case ExEveryNth(m, n, from) =>
          val ds = extractTree(m)
          val nTree = extractTree(n)
          val fromTree = extractTree(from)
          ds.getType match {
            case VectorType(_) => EveryNthVector(ds, nTree, fromTree)
            case MatrixType(_) =>  EveryNthMatrix(ds, nTree, fromTree)
          }

        /* ----- Binary ops ----- */

        case ExPlusMinus(l, r)    => AbsError(extractTree(l), extractTree(r))
        case ExPow(l, r)      =>
          val power = extractTree(r) match {
            case Int32Literal(v) if (v >= 2) => v
            case x => ctx.reporter.fatalError("Power is only supported for positive integer powers > 2")
          }
          val tree = extractTree(l)
          var res = tree
          for (i <- 1 until power) {
            res = Times(res, tree)
          }
          res

        // Function call
        case ExCall(tr, sym, args) if (defsToDefs contains sym) =>
          val rargs = args.map(extractTree)
          val tmpFunDef = defsToDefs(sym)
          FunctionInvocation(tmpFunDef.id, tmpFunDef.params, rargs, tmpFunDef.returnType)

        case ExCall(tr, sym, args) if (args.length == 1) =>
          val lhs = extractTree(tr)
          val rhs = extractTree(args.head)

          (lhs, sym.name.decoded, rhs) match {
            case (IsTyped(a1, RealType), "+", IsTyped(a2, RealType)) =>
              Plus(a1, a2)
            case (IsTyped(a1, RealType), "-", IsTyped(a2, RealType)) =>
              Minus(a1, a2)
            case (IsTyped(a1, RealType), "*", IsTyped(a2, RealType)) =>
              Times(a1, a2)
            case (IsTyped(a1, RealType), "/", IsTyped(a2, RealType)) =>
              Division(a1, a2)

            case (IsTyped(a1, RealType), ">", IsTyped(a2, RealType)) =>
              GreaterThan(a1, a2)
            case (IsTyped(a1, RealType), ">=", IsTyped(a2, RealType)) =>
              GreaterEquals(a1, a2)
            case (IsTyped(a1, RealType), "<", IsTyped(a2, RealType)) =>
              LessThan(a1, a2)
            case (IsTyped(a1, RealType), "<=", IsTyped(a2, RealType)) =>
              LessEquals(a1, a2)

            case (IsTyped(a1, RealType), "!=", IsTyped(a2, RealType)) =>
              Not(Equals(a1, a2))

            case (IsTyped(a1, RealType), "==", IsTyped(a2, RealType)) =>
              Equals(a1, a2)
            case (IsTyped(a1, Int32Type), "==", IsTyped(a2, Int32Type)) =>
              Equals(a1, a2)

            // Boolean methods
            case (IsTyped(a1, BooleanType), "&&", IsTyped(a2, BooleanType)) =>
              and(a1, a2)

            case (IsTyped(a1, BooleanType), "||", IsTyped(a2, BooleanType)) =>
              or(a1, a2)

            case (IsTyped(a1, RealType), "!=", Int32Literal(i)) =>
              Not(Equals(a1, RealLiteral(tools.Rational(i))))

            case (Int32Literal(i), "!=", IsTyped(a1, RealType)) =>
              Not(Equals(a1, RealLiteral(tools.Rational(i))))

            case (IsTyped(a1, RealType), "==", Int32Literal(i)) =>
              Equals(a1, RealLiteral(tools.Rational(i)))

            case (Int32Literal(i), "==", IsTyped(a1, RealType)) =>
              Equals(a1, RealLiteral(tools.Rational(i)))

            // mixed operations?
            case (IsTyped(a1, RealType), "+", IsTyped(a2, Int32Type)) =>
              Plus(a1, a2)

            // operations on ints
            case (IsTyped(a1, Int32Type), "+", IsTyped(a2, Int32Type)) =>
              Plus(a1, a2)
            case (IsTyped(a1, Int32Type), "-", IsTyped(a2, Int32Type)) =>
              Minus(a1, a2)
            case (IsTyped(a1, Int32Type), "*", IsTyped(a2, Int32Type)) =>
              Times(a1, a2)
            case (IsTyped(a1, Int32Type), "==", IsTyped(a2, Int32Type)) =>
              Equals(a1, a2)
            case (IsTyped(a1, Int32Type), "<=", IsTyped(a2, Int32Type)) =>
              LessEquals(a1, a2)
            case (IsTyped(a1, Int32Type), ">=", IsTyped(a2, Int32Type)) =>
              GreaterEquals(a1, a2)
            case (IsTyped(a1, Int32Type), "<", IsTyped(a2, Int32Type)) =>
              LessThan(a1, a2)
            case (IsTyped(a1, Int32Type), ">", IsTyped(a2, Int32Type)) =>
              GreaterThan(a1, a2)

            // data struct range spec
              // vectors
            case (IsTyped(a1, VectorType(_)), ">", IsTyped(a2, RealType)) =>
              GreaterThan(a1, a2)
            case (IsTyped(a1, VectorType(_)), ">=", IsTyped(a2, RealType)) =>
              GreaterEquals(a1, a2)
            case (IsTyped(a1, VectorType(_)), "<", IsTyped(a2, RealType)) =>
              LessThan(a1, a2)
            case (IsTyped(a1, VectorType(_)), "<=", IsTyped(a2, RealType)) =>
              LessEquals(a1, a2)
              // matrices
            case (IsTyped(a1, MatrixType(_)), ">", IsTyped(a2, RealType)) =>
              GreaterThan(a1, a2)
            case (IsTyped(a1, MatrixType(_)), ">=", IsTyped(a2, RealType)) =>
              GreaterEquals(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "<", IsTyped(a2, RealType)) =>
              LessThan(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "<=", IsTyped(a2, RealType)) =>
              LessEquals(a1, a2)
              // element-wise arithmetic operations
            //case (IsTyped(a1, VectorType(_)), "+", IsTyped(a2, RealType)) => Plus(a1, a2) // element-wise sum with a constant?
            case (IsTyped(a1, VectorType(_)), "+", IsTyped(a2, VectorType(_))) =>
              Plus(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "+", IsTyped(a2, MatrixType(_))) =>
              Plus(a1, a2)
            case (IsTyped(a1, VectorType(_)), "-", IsTyped(a2, VectorType(_))) =>
              Minus(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "-", IsTyped(a2, MatrixType(_))) =>
              Minus(a1, a2)
            case (IsTyped(a1, VectorType(_)), "*", IsTyped(a2, VectorType(_))) =>
              Times(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "*", IsTyped(a2, MatrixType(_))) =>
              Times(a1, a2)
            case (IsTyped(a1, VectorType(_)), "/", IsTyped(a2, VectorType(_))) =>
              Division(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "/", IsTyped(a2, MatrixType(_))) =>
              Division(a1, a2)
              // operations with constants
            case (IsTyped(a1, RealType), "*", IsTyped(a2, VectorType(_))) =>
              Times(a1, a2)
            case (IsTyped(a1, VectorType(_)), "*", IsTyped(a2, RealType)) =>
              Times(a1, a2)
            case (IsTyped(a1, VectorType(_)), "+", IsTyped(a2, RealType)) =>
              Plus(a1, a2)
            case (IsTyped(a1, RealType), "*", IsTyped(a2, MatrixType(_))) =>
              Times(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "*", IsTyped(a2, RealType)) =>
              Times(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "+", IsTyped(a2, RealType)) =>
              Plus(a1, a2)
            case (IsTyped(a1, RealType), "/", IsTyped(a2, VectorType(_))) =>
              Division(a1, a2)
            case (IsTyped(a1, VectorType(_)), "/", IsTyped(a2, RealType)) =>
              Division(a1, a2)
            case (IsTyped(a1, RealType), "/", IsTyped(a2, MatrixType(_))) =>
              Division(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "/", IsTyped(a2, RealType)) =>
              Division(a1, a2)
              // products
            case (IsTyped(a1, VectorType(_)), "x" , IsTyped(a2, VectorType(_))) =>
              CrossProduct(a1,a2)
            case (IsTyped(a1, MatrixType(_)), "x" , IsTyped(a2, MatrixType(_))) =>
              CrossProduct(a1,a2)
            case (IsTyped(a1, MatrixType(_)), "x" , IsTyped(a2, VectorType(_))) =>
              CrossProduct(a1,a2)

            case (IsTyped(a1, VectorType(_)), "map" , a2@ Lambda(_,_)) =>
              MapIter(a1,a2)
            case (IsTyped(a1, MatrixType(_)), "map" , a2@ Lambda(_,_)) =>
              MapIter(a1,a2)
            case (IsTyped(a1, MatrixType(_)), "mapElements" , a2@ Lambda(_,_)) =>
              MapElemsIter(a1,a2)
            case (IsTyped(a1, VectorType(_)), "filter" , a2@ Lambda(_,_)) =>
              FilterIter(a1,a2)
            case (IsTyped(a1, MatrixType(_)), "filter" , a2@ Lambda(_,_)) =>
              FilterIter(a1,a2)
            case (IsTyped(a1, VectorType(_)), "enumMap" , fnc@Lambda(args, body)) =>
              EnumVectorAndMap(a1, fnc)
            case (IsTyped(a1, VectorType(_)), "pad" , i@Int32Literal(_)) =>
              PadVector(a1, i)
            case (IsTyped(a1, MatrixType(_)), "pad" , i@Int32Literal(_)) =>
              PadMatrix(a1, i, i)
            case (IsTyped(a1, VectorType(_)), "++" , IsTyped(a2, VectorType(_))) =>
              Concat(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "++" , IsTyped(a2, MatrixType(_))) =>
              Concat(a1, a2)
            case (IsTyped(a1, VectorType(_)), "+:" , IsTyped(a2, RealType)) =>
              PrependElement(a1, a2) // todo is this ever executed?
            case (IsTyped(a1, MatrixType(_)), "+:" , IsTyped(a2, VectorType(_))) =>
              PrependElement(a1, a2) // todo is this ever executed?
            case (IsTyped(a1, VectorType(_)), ":+" , IsTyped(a2, RealType)) =>
              AppendElement(a1, a2)
            case (IsTyped(a1, MatrixType(_)), ":+" , IsTyped(a2, VectorType(_))) =>
              AppendElement(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "row" , Int32Literal(i)) =>
              RowOfMatrix(a1, Int32Literal(i))
            case (IsTyped(a1, MatrixType(_)), "row" , IsTyped(a2, Int32Type)) =>
              RowOfMatrix(a1, a2)
            case (IsTyped(a1, MatrixType(_)), "enumRowsMap" , fnc@Lambda(args, body)) =>
              EnumRowsAndMap(a1, fnc)

          }

        /* ----- Ternary ops ----- */
        case ExFMA(f1, f2, s) => FMA(extractTree(f1), extractTree(f2), extractTree(s))

        // match for synthetic fnc by ScalaCompiler (creates additional unnecessary binding
        // Example: val (k,x) = y -> let x$1 = y in let x = x$1._1 in k = x$1._2
        // only tested for one testcase val (k,x) = y; not suitable for "match-case" in general
        case ExMatchSynthetic(selector) => extractTree(selector)

        /* ----- Let ----- */
        case ExValDef(vs, tpt, bdy) =>
          val valTree = extractTree(bdy)

          rest match {
            case Some(rst) =>
              rst match {
                case ExPrependElement(dsTree, _) =>
                  val ds = extractTree(dsTree)
                  rest = None
                  PrependElement(ds, valTree)
                case _ =>
                  val binderTpe = extractType(tpt)
                  val newID = FreshIdentifier(vs.name.toString, binderTpe)
                  val nctx = dctx.withNewVar(vs -> newID)
                  val restTree = extractTree(rst)(nctx)
                  rest = None
                  Let(newID, valTree, restTree)
                }
            case None =>
              val binderTpe = extractType(tpt)
              val newID = FreshIdentifier(vs.name.toString, binderTpe)
              rest = None
              Let(newID, valTree, UnitLiteral())
          }


        case ExIfThenElse(t1,t2,t3) =>
          def containsLetDef(expr: DaisyExpr): Boolean = {
            lang.TreeOps.exists { case Let(_,_,_) => true }(expr)
          }

          val r1 = extractTree(t1)
          if(containsLetDef(r1)) {
            outOfSubsetError(t1, "Condition of if-then-else expression should not contain nested function definition")
          }
          val r2 = extractTree(t2)
          val r3 = extractTree(t3)
          // val lub = leastUpperBound(r2.getType, r3.getType)
          if (r2.getType == r3.getType) {
            IfExpr(r1, r2, r3)
          } else {
            outOfSubsetError(tr, "Both branches of ifthenelse have incompatible types (" +
              r2.getType + " and " + r3.getType + ")")
          }

        case ExTuple(args) =>
          Tuple(args map {x => extractTree(x)})

        case ExFold(tv, tinit, tfnc) =>
          val v = extractTree(tv)
          val init = extractTree(tinit)
          val fnc = extractTree(tfnc) match { case x: Lambda => x }
          v.getType match {
            case VectorType(_) =>
              fnc match {
                case Lambda(args, Plus(Variable(id1), Variable(id2))) if args.map(_.id).contains(id1) && args.map(_.id).contains(id2) => Sum(v, init)
                case _ => FoldIter(v, init, fnc)
              }
            case MatrixType(_) =>
              fnc.returnType match {
                case VectorType(_) => FoldIter(v, init, fnc)
                case RealType => FoldElemsIter(v, init, fnc)
              }
            case _ => outOfSubsetError(tr, "Could not extract as PureScala. Applying fold to a non-vector variable " + tr.symbol.nameString + "(" + tr.getClass + ")")
          }

        case ExZipVectors(lh, rh) =>
          val v1 = extractTree(lh)
          val v2 = extractTree(rh)
          ZipVectors(v1, v2)

        case ExPadMatrix(v, sizeI, sizeJ) =>
          val ds = extractTree(v)
          val i = extractTree(sizeI)
          val j = extractTree(sizeJ)
          PadMatrix(ds, i, j)

        case ExSlideReduce(dsTree, sizeTree, stepTree, fncTree) =>
          val ds = extractTree(dsTree)
          val size = extractTree(sizeTree) match {case i: Int32Literal => i }
          val step = extractTree(stepTree) match {case i: Int32Literal => i }
          val fnc = extractTree(fncTree) match {case f: Lambda => f }
          SlideReduceIter(ds, size, step, fnc)

        case ExEnumSlideFlatMap(dsTree, sizeTree, fncTree) =>
          val ds = extractTree(dsTree)
          val size = extractTree(sizeTree) match {case i: Int32Literal => i }
          val fnc = extractTree(fncTree) match {case f: Lambda => f }
          EnumSlideFlatMap(ds, size, fnc)

        case ExSubsetDS(dsTree, fromInd, toInd) =>
          val ds = extractTree(dsTree)
          ds.getType match {
            case VectorType(_) =>
              val i = extractTree(fromInd.head) //match {case Int32Literal(ii) => ii}
              val j = extractTree(toInd.head) // match {case Int32Literal(ii) => ii}
              SubVector(ds, i, j)
            case MatrixType(_) =>
              val i = fromInd.map(e => extractTree(e) match {case Int32Literal(ii) => ii})
              val from = (i.head, i(1))
              val j = toInd.map(e => extractTree(e) match {case Int32Literal(ii) => ii})
              val to = (j.head, j(1))
              SubMatrix(ds, Seq(from, to))
          }


        // default behaviour is to complain :)
        case _ =>
          outOfSubsetError(tr, "Could not extract as PureScala (Scala tree of type " + tr.getClass + ")")
      }

      res.setPos(tr.pos)

      // sanity check that we haven't forgotten anything
      assert(rest.isEmpty, s"Part of the tree is not parsed! ${rest.get}")
      res
  }

  }
}
