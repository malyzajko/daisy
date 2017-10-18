// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import scala.tools.nsc._

import scala.collection.immutable.Seq


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
      def unapply(cd: ClassDef): Option[(String,Template)] = cd match {
        case ClassDef(_, name, tparams, impl) if
          (cd.symbol.isModuleClass || cd.symbol.hasPackageFlag) &&
          tparams.isEmpty &&
          !cd.symbol.isSynthetic &&
          !cd.symbol.isCaseClass
        => {
          Some((name.toString, impl))
        }
        case _ => None
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

    // object ExFloat64Literal {
    //   def unapply(tree: Literal): Option[Double] = tree match {
    //     case Literal(c @ Constant(i)) if c.tpe == DoubleClass.tpe => Some(c.doubleValue)
    //     case _ => None
    //   }
    // }

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
        case _ => None
      }
    }

    object ExCos {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.cos") => Some(arg)
        case _ => None
      }
    }

    object ExTan {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.tan") => Some(arg)
        case _ => None
      }
    }

    object ExLog {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.log") => Some(arg)
        case _ => None
      }
    }

    object ExExp {
      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(select, List(arg)) if (select.toString == "daisy.lang.Real.exp") => Some(arg)
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

  } // end StructuralExtractors
}