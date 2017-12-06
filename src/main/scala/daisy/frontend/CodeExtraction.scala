// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import scala.reflect.internal.util._
import scala.language.implicitConversions

import scala.collection.immutable.Seq

import lang.Trees.{
  Expr => DaisyExpr,
  Program => DaisyProgram,
  ValDef => DaisyValDef,
  FunDef => DaisyFunDef,
  _}
import lang.Identifiers._
import lang.Types.{TypeTree => DaisyType, _}
import utils.Positioned

import utils.{Position => DaisyPosition, OffsetPosition => DaisyOffsetPosition,
  RangePosition => DaisyRangePosition, DefinedPosition}


trait CodeExtraction extends ASTExtractors {
  self: DaisyExtraction =>

  import global._
  import global.definitions._
  import StructuralExtractors._

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
      u.source.file.absolute.path.endsWith(self.ctx.libFiles.head)
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

        /* ----- Literals ----- */

        case ExInt32Literal(v) => Int32Literal(v)

        case ExImplicitInt2Real(i) => RealLiteral(tools.Rational(i), i.toString)

        case ExBooleanLiteral(v) => BooleanLiteral(v)

        case ExFloat64Literal(d) => RealLiteral(tools.Rational.fromString(d.toString), d.toString)

        case ExImplicitDouble2Real(d) => RealLiteral(tools.Rational.fromString(d.toString), d.toString)

        case ex @ ExIdentifier(sym, tpt) if dctx.isVariable(sym) =>
          (dctx.vars.get(sym): @unchecked) match {
            case Some(id) =>
              Variable(id).setPos(ex.pos)
            // case None =>
            //   // Maybe it is a function
            //   defsToDefs.get(sym) match {
            //     case Some(fd) =>
            //       FunctionInvocation(fd.typed, Seq()).setPos(sym.pos)
            //     case None =>
            //       outOfSubsetError(tr, "Unidentified variable " + sym + " " + sym.id + ".")
            //   }
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
        case ExExp(e)         => Exp(extractTree(e))
        case ExLog(e)         => Log(extractTree(e))

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
              LessEquals(a1, a2)

            case (IsTyped(a1, RealType), "==", IsTyped(a2, RealType)) =>
              LessEquals(a1, a2)

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


          }

        /* ----- Ternary ops ----- */
        case ExFMA(f1, f2, s) => FMA(extractTree(f1), extractTree(f2), extractTree(s))

        /* ----- Let ----- */
        case ExValDef(vs, tpt, bdy) =>
          val binderTpe = extractType(tpt)
          val newID = FreshIdentifier(vs.name.toString, binderTpe)
          val valTree = extractTree(bdy)

          val restTree = rest match {
            case Some(rst) =>
              val nctx = dctx.withNewVar(vs -> newID)
              extractTree(rst)(nctx)
            case None =>
              UnitLiteral()
          }

          rest = None
          Let(newID, valTree, restTree)


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

        // default behaviour is to complain :)
        case _ =>
          outOfSubsetError(tr, "Could not extract as PureScala (Scala tree of type " + tr.getClass + ")")
      }

      res.setPos(tr.pos)

      // sanity check that we haven't forgotten anything
      assert(rest == None)
      res
  }

  }
}
