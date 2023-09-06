// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import scala.collection.immutable.Seq
import scala.tools.nsc._


/** Contains extractors to pull-out interesting parts of the Scala ASTs. */
trait ASTExtractors {
  val global: Global

  import global._
  import global.definitions._

  def annotationsOf(s: Symbol): Map[String, Seq[Option[Any]]] = {
    val actualSymbol = s.accessedOrSelf

    (for {
      a <- actualSymbol.annotations ++ actualSymbol.owner.annotations
      name = a.atp.safeToString.replaceAll("\\.package\\.", ".")
      if (name startsWith "daisy.lang.annotation.")
    } yield {
      val args = a.args.map {
        case Literal(x) => Some(x.value)
        case _ => None
      }
      (name.split("\\.", 3)(2), args)
    }).toMap
  }

  def classFromName(str: String): global.ClassSymbol = {
    rootMirror.getClassByName(newTypeName(str))
  }

  protected lazy val realSym          = classFromName("daisy.lang.Real")
  def isRealSym(sym: Symbol): Boolean = {
    // getResolvedTypeSym(sym) == realSym
    // @eva: I don't think we have type aliases...
    sym == realSym
  }

  object ExtractorHelpers {
    /** Matches nested `Select(Select(...Select(a, b) ...y) , z)` and returns the list `a,b, ... y,z` */
    object ExSelected {
      def unapplySeq(select: Select): Option[Seq[String]] = select match {
        case Select(This(scalaName), name) =>
          Some(Seq(scalaName.toString, name.toString))

        case Select(from: Select, name) =>
          unapplySeq(from).map(prefix => prefix :+ name.toString)

        case Select(from: Ident, name) =>
          Some(Seq(from.toString, name.toString))

        case _ =>
          None
      }
    }

    // this is not very structural...
    object IsTyped {
      def unapply[T <: daisy.lang.Types.Typed](e: T): Option[(T, daisy.lang.Types.TypeTree)] = Some((e, e.getType))
    }

    /** Extracts the string representation of a name of something having the `Name` trait */
    object ExNamed {
      def unapply(name: Name): Option[String] = Some(name.toString)
    }
  }


  object StructuralExtractors {
    import ExtractorHelpers._

    /** Matches an object with no type parameters, and regardless of its
     * visibility. Does not match on case objects or the automatically generated companion
     * objects of case classes (or any synthetic class).
     */
    object ExObjectDef {
      def unapply(md: ModuleDef): Option[(String,Template)] = md match {
        case ModuleDef(mods, name, impl) if
          (md.symbol.isModule) &&
          !md.symbol.isSynthetic &&
          !md.symbol.isCaseClass
        => {
          Some((name.toString, impl))
        }
        case _ => {
          println(s"${md.symbol.isModule} ${md.symbol.isModuleClass} ${md.symbol.hasPackageFlag} ${!md.symbol.isSynthetic} ${!md.symbol.isCaseClass}")
          None }
      }
    }

    /** Matches a function with a single list of arguments,
     * and regardless of its visibility.
     */
    object ExFunctionDef {
      def unapply(dd: DefDef): Option[(Symbol, Seq[Symbol], Seq[ValDef], Type, Tree)] = dd match {
        case DefDef(_, name, tparams, vparamss, tpt, rhs) if name != nme.CONSTRUCTOR && !dd.symbol.isAccessor =>
          if (dd.symbol.isSynthetic && dd.symbol.isImplicit && dd.symbol.isMethod) {
            // Check that the class it was generated from is not ignored
            // if (annotationsOf(tpt.symbol).isDefinedAt("ignore")) {
            //   None
            // } else {
              Some((dd.symbol, tparams.map(_.symbol), vparamss.flatten, tpt.tpe, rhs))
            // }
          } else if (!dd.symbol.isSynthetic) {
            Some((dd.symbol, tparams.map(_.symbol), vparamss.flatten, tpt.tpe, rhs))
          } else {
            None
          }
        case _ => None
      }
    }

    object ExConstructorDef {
      def unapply(dd: DefDef): Boolean = dd match {
        case DefDef(_, name, tparams, vparamss, tpt, rhs) if name == nme.CONSTRUCTOR && tparams.isEmpty => true
        case _ => false
      }
    }

    object ExBooleanLiteral {
      def unapply(tree: Literal): Option[Boolean] = tree match {
        case Literal(Constant(true)) => Some(true)
        case Literal(Constant(false)) => Some(false)
        case _ => None
      }
    }

    object ExInt32Literal {
      def unapply(tree: Literal): Option[Int] = tree match {
        case Literal(c @ Constant(i)) if c.tpe == IntClass.tpe => Some(c.intValue)
        case _ => None
      }
    }

    object ExFloat64Literal {
      def unapply(tree: Literal): Option[Double] = tree match {
        case Literal(c @ Constant(i)) if c.tpe == DoubleClass.tpe => Some(c.doubleValue)
        case _ => None
      }
    }

    object ExIdentifier {
      def unapply(tree: Ident): Option[(Symbol,Tree)] = tree match {
        case i: Ident => Some((i.symbol, i))
        case _ => None
      }
    }

    object ExValDef {
      /** Extracts val's in the head of blocks. */
      def unapply(tree: ValDef): Option[(Symbol,Tree,Tree)] = tree match {
        case vd @ ValDef(mods, _, tpt, rhs) if !mods.isMutable => Some((vd.symbol, tpt, rhs))
        case _ => None
      }
    }

    /** Extracts the 'require' contract from an expression (only if it's the
     * first call in the block).
     */
    object ExRequiredExpression {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(ExSelected("scala", "Predef", "require"), contractBody :: Nil) =>
          Some(contractBody)
        case _ => None
      }
    }

    object ExEnsuredExpression {
      /** Extracts the 'ensuring' contract from an expression. */
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(Apply(TypeApply(ExSelected("scala", "Predef", "Ensuring"), _ :: Nil),
          body :: Nil), ExNamed("ensuring")), contract :: Nil)
          => Some((body, contract))
        case _ => None
      }
    }

    object ExCall {
      def unapply(tree: Tree): Option[(Tree, Symbol, Seq[Tree])] = tree match {

        case Apply(s @ Select(t, _), args) =>

          Some(t, s.symbol, args)

        case _ => None

      }
    }

    // object ExUMinus {
    //   def unapply(tree: Select): Option[Tree] = tree match {
    //     case Select(t, n) if n == nme.UNARY_- => Some(t)
    //     case _ => None
    //   }
    // }

    object ExRealUMinus {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(Select(t, n), List()) if (n == nme.UNARY_-) => Some(t)
        case _ => None
      }
    }

    object ExImplicitInt2Real {
      def unapply(tree: Apply): Option[Int] = tree match {
        case Apply(select, List(Literal(c @ Constant(i)))) if (select.toString == "daisy.lang.Real.int2real") =>
          Some(c.intValue)
        case _ => None
      }
    }

    object ExImplicitInt2RealOnVars {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(x:Ident)) if select.toString == "daisy.lang.Real.int2real" =>
          Some(x)
        case _ => None
      }
    }

    object ExImplicitDouble2Real {
      def unapply(tree: Apply): Option[Double] = tree match {
        case Apply(select, List(Literal(c @ Constant(i)))) if (select.toString == "daisy.lang.Real.double2real") =>
          Some(c.doubleValue)
        case _ => None
      }
    }

    object ExLambdaExpression {
      def unapply(tree: Function): Option[(Seq[ValDef], Tree)] = tree match {
        case Function(vds, body) => Some((vds, body))
        case _ => None
      }
    }

    object ExSqrt {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.sqrt") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "sqrt") => Some(t)
        case _ => None
      }
    }

    object ExFMA {
      def unapply(tree: Apply): Option[(Tree, Tree, Tree)] = tree match {
        case Apply(select, List(f1,f2,s)) if select.toString == "daisy.lang.Real.fma" => Some((f1,f2,s))
        case _ => None
      }
    }

    object ExPow {
      def unapply(tree: Apply): Option[(Tree, Tree)] = tree match {
        case Apply(select, List(arg1, arg2)) if (select.toString == "daisy.lang.Real.pow") => Some((arg1, arg2))
        case _ => None
      }
    }

    object ExSin {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.sin") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "sin") => Some(t)
        case _ => None
      }
    }

    object ExCos {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.cos") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "cos") => Some(t)
        case _ => None
      }
    }

    object ExTan {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.tan") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "tan") => Some(t)
        case _ => None
      }
    }

    object ExAsin {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.asin") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "asin") => Some(t)
        case _ => None
      }
    }

    object ExAcos {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.acos") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "acos") => Some(t)
        case _ => None
      }
    }

    object ExAtan {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.atan") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "atan") => Some(t)
        case _ => None
      }
    }

    object ExLog {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.log") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "log") => Some(t)
        case _ => None
      }
    }

    object ExExp {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.exp") => Some(arg)
        case Apply(Select(t, nm), List()) if (nm.toString == "exp") => Some(t)
        case _ => None
      }
    }


    object ExPlusMinus {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n.toString == "$plus$div$minus") => Some((lhs,rhs))
        case _ => None
      }
    }

    object ExIfThenElse {
      def unapply(tree: If): Option[(Tree,Tree,Tree)] = tree match {
        case If(t1,t2,t3) => Some((t1,t2,t3))
        case _ => None
      }
    }

    object ExTuple {
      def unapply(tree: Apply): Option[Seq[Tree]] = tree match {
        case Apply(select, args) if (select.toString.startsWith("new")) => Some(args)
        case _ => None
      }
    }

    // Data struct Operations
    object ExFold{
      def unapply(tree:Apply): Option[(Tree,Tree,Tree)] = tree match {
        case Apply(Apply(Select(v,sym), List(init)), List(fnc)) if sym.toString.contains("fold") =>
          Some(v, init, fnc)
        case _ => None
      }
    }

    // TODO remove if x.length() has type Real
    object ExImplicitSize2Real {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(Apply(x @ Select(id, innerFnc), Nil))) if (select.toString == "daisy.lang.Real.int2real" && innerFnc.toString() == "length") =>
          Some(id)
        case _ => None
      }
    }

    object ExZipVectors {
      def unapply(tree: Apply): Option[(Tree, Tree)] = tree match {
        case Apply(select, List(v1,v2)) if (select.toString == "daisy.lang.Vector.zip") =>
          Some(v1, v2)
        case _ => None
      }
    }

    object ExZeroDS {
      def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
        case Apply(Select(t, nm), List(i)) if nm.toString() == "zeroVector" =>
          Some(t, Seq(i))
        case Apply(Select(t, nm), List(i,j)) if nm.toString() == "zeroMatrix" =>
          Some(t, Seq(i,j))
        case _ => None
      }
    }

    object ExConstructDSFromList {
      def unapply(tree: Tree): Option[(Tree, Seq[Seq[Tree]])] = tree match {
        case Apply(t@Select(New(v), _),List(Apply(_,args))) if v.toString() == "daisy.lang.Vector" =>
          args match {
            case List(Function(_,_)) => None //  this case is for ExConstructDSFromExpr
            case List(TypeApply(_,_)) => None //  this case is for ExConstructDSFromExpr
            case _ => Some(t, Seq(args))
          }
        case Apply(t@Select(New(v), _),List(Apply(l,args))) if v.toString() == "daisy.lang.Matrix" =>
          l match {
            // create from constants: Matrix(List(List(0.0)))
            case TypeApply(Select(list, _),_) if list.symbol.nameString == "List" =>
              val resArgs: Seq[Seq[Tree]] = args.map { case Apply(_, inner) => inner }
              Some(t, resArgs)
            case _ => None
          }

        case _ => None
      }
    }

    object ExConstructDSFromExpr {
      def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
        case Apply(t@Select(New(v), _), List(res)) if v.toString() == "daisy.lang.Vector" || v.toString() == "daisy.lang.Matrix" =>
          res match {
            case Apply(TypeApply(x, _), body) => //if to.symbol.nameString != "List"
              val redo = Apply(x, body) // todo test properly -> TypeApply is Apply for generics, applies actual type
              Some(t, redo)
            case Ident(_) =>
              Some(t, res)
            case _ => None
          }
        case _ => None
      }
    }

    object ExFlatVector {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Apply(Select(_, nm), List(expr)) if nm.toString() == "flatVector" => // todo need the case for multiple expressions as arg?
          expr match {
            case Apply(TypeApply(x, _), body) =>
              val redo = Apply(x, body)
              Some(redo)
            case _ =>  Some(expr)
          }
        case _ => None
      }
    }

    object ExAccessTuple {
        def unapply(tree: Tree): Option[(Tree, Int)] = tree match {
          case Select(tpl, nm) if nm.decoded == "_1" =>
            Some(tpl, 0)
          case Select(tpl, nm) if nm.decoded == "_2" =>
            Some(tpl, 1)
          case _ => None
        }}

    object ExSlideReduce {
      def unapply(tree: Tree): Option[(Tree, Tree, Tree, Tree)] = tree match {
        case Apply(Apply(Select(v,sym), List(size, step)), List(fnc)) if sym.toString == "slideReduce" =>
          Some(v, size, step, fnc)
        case _ => None
      }
    }

    object ExEnumSlideFlatMap {
      def unapply(tree: Tree): Option[(Tree, Tree, Tree)] = tree match {
        case Apply(Apply(Select(v,sym), List(size)), List(fnc)) if sym.toString == "enumSlideFlatMap" =>
          Some(v, size, fnc)
        case _ => None
      }
    }

    object ExPadMatrix {
      def unapply(tree: Tree): Option[(Tree, Tree, Tree)] = tree match {
        case Apply(Select(v,sym), List(i, j)) if sym.toString == "pad" =>
          Some(v, i, j)
        case _ => None
      }
    }

    object ExElementAt {
      def unapply(tree: Tree): Option[(Tree, Tree, Option[Tree])] = tree match {
        case Apply(Select(v,sym), List(i, j)) if sym.toString == "at" =>
          Some(v, i, Some(j))
        case Apply(Select(v,sym), List(i)) if sym.toString == "at" =>
          Some(v, i, None)
        case _ => None
      }
    }

    object ExEveryNth {
      def unapply(tree: Tree): Option[(Tree, Tree, Tree)] = tree match {
        case Apply(Select(v,sym), List(i, j)) if sym.toString == "everyNth" =>
          Some(v, i, j)
        case _ => None
      }
    }

    object ExHeadElement {
      def unapply(tree: Tree): Option[(Tree)] = tree match {
        case Select(v, sym) if sym.toString == "head" =>
          Some(v)
        case _ => None
      }
    }

    object ExPrependElement {
      def unapply(tree: Tree): Option[(Tree,Tree)] = tree match {
        case Apply(Select(ds, nm), List(el)) if nm.decoded == "+:" => Some((ds, el))
        case _ => None
      }
    }

    object ExSubsetDS {
      def unapply(tree: Tree): Option[(Tree, Seq[Tree], Seq[Tree])] = tree match {
        case Apply(Select(v,sym), List(i, j)) if sym.toString == "slice" =>
          Some(v, Seq(i), Seq(j))
        case Apply(Apply(Select(v,sym), List(fromI, fromJ)), List(toI, toJ)) if sym.toString == "slice" =>
          Some(v, Seq(fromI, fromJ), Seq(toI, toJ))
        case _ => None
      }
    }

    object ExSizeSpec {
      def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
        case Apply(Select(v,sym), List(i)) if sym.toString == "size" =>
          Some(v, Seq(i))
        case Apply(Select(v,sym), List(i, j)) if sym.toString == "size" =>
          Some(v, Seq(i,j))
        case _ => None
      }
    }

    object ExElemRangeSpec {
      def unapply(tree: Tree): Option[(Tree, Tree, Tree, Tree, Tree)] = tree match {
        case Apply(Apply(Select(v, sym), List(i, j)), List(low, up)) if sym.toString == "el" =>
          Some(v, i, j, low, up)
        case _ => None
      }
    }

    object ExBulkVectorElemRangeSpec {
      def unapply(tree: Tree): Option[(Tree, List[((Int, Int),(Double, Double))])] = tree match {
        case Apply(Select(v, sym), List(Apply(_, specList))) if sym.toString == "specV" =>
          val specTuples = specList.map({
            case ExBulkVectorPartSpec(i1,i2, r1, r2) => ((i1,i2),(r1,r2))
            case _ => ((-100,-100),(0.0,0.0))
          })
          if (specTuples.contains(((-100,-100),(0.0,0.0))))
            None
          else
            Some(v, specTuples)
        case _ => None
      }
    }


    object ExBulkVectorPartSpec {
      def unapply(tree: Tree): Option[(Int, Int, Double, Double)] = tree match {
        case Apply(select, List(Apply(_,
        List(ExInt32Literal(i1), ExInt32Literal(i2))), Apply(_,
        List(ExImplicitDouble2Real(r1), ExImplicitDouble2Real(r2))))) if select.toString.startsWith("new") =>
          Some(i1, i2, r1, r2)
        case Apply(select, List(Apply(_,
        List(ExInt32Literal(i1), ExInt32Literal(i2))), Apply(_,
        List(ExImplicitInt2Real(r1), ExImplicitInt2Real(r2))))) if select.toString.startsWith("new") =>
          Some(i1, i2, r1.doubleValue(), r2.doubleValue())
        case _ => None
      }
    }

    object ExBulkMatrixElemRangeSpec {
      def unapply(tree: Tree): Option[(Tree, List[(Seq[(Int, Int)],(Double, Double))])] = tree match {
        case Apply(Select(v, sym), List(Apply(_, specList))) if sym.toString == "specM" =>
          val specTuples = specList.map({
            case ExBulkMatrixPartSpec(inds, r1, r2) => (inds, (r1, r2))
            case _ => (Seq(), (0.0, 0.0))
          })
          if (specTuples.contains((Seq(), (0.0, 0.0))))
            None
          else
            Some(v, specTuples)
        case _ => None
      }
    }

    object ExBulkMatrixPartSpec {
      def unapply(tree: Tree): Option[(Seq[(Int, Int)], Double, Double)] = tree match {
        case Apply(select,
            List(Apply(_, inds),
            Apply(_, List(ExImplicitDouble2Real(r1), ExImplicitDouble2Real(r2))))) if select.toString.startsWith("new") =>
          val indSeq = inds.map({case Apply(_, List(ExInt32Literal(i1), ExInt32Literal(i2))) => (i1,i2)})
          Some(indSeq, r1, r2)
        case Apply(select,
            List(Apply(_, inds),
            Apply(_, List(ExImplicitInt2Real(r1), ExImplicitInt2Real(r2))))) if select.toString.startsWith("new") =>
          val indSeq = inds.map({case Apply(_, List(ExInt32Literal(i1), ExInt32Literal(i2))) => (i1,i2)})
          Some(indSeq, r1.doubleValue(), r2.doubleValue())
        case _ => None
      }
    }


    object ExSubRangeSpec {
      def unapply(tree: Tree): Option[(Tree, Seq[(Tree,Tree)], Tree, Tree)] = tree match {
        case Apply(Apply(Select(v,sym), List(from, to)), List(low, up)) if sym.toString == "range" =>
          Some(v, Seq((from,to)), low, up)
        case Apply(Apply(Select(v,sym), z), List(low, up)) if sym.toString == "range" =>
          val tmp = z.head
          tmp match {
            case Apply(TypeApply(_,_), args) =>
              args match {
                case ExSeqTuples(tuples) => Some (v, tuples, low, up) //
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }
    }

    object ExSeqTuples {
      def unapply(tree: Seq[Tree]): Option[Seq[(Tree,Tree)]] = {
        if (tree.nonEmpty)
          Some(tree.map { case ExTuple(args) => (args.head, args(1))})
        else
          None
      }
    }

    // only tested for one testcase val (k,x) = y; not suitable for "match-case" in general
    // todo remove the var x$1 directly from the let stmt: let x$1 = synthetic match {..} in let x = x$1._1
    object ExMatchSynthetic {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Match(Typed(sel, tpt), cases) => Some(sel)
        case _ => None
      }
    }

  } // end StructuralExtractors
}
