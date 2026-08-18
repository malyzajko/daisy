package daisy
package frontend

import io.github.treesitter.jtreesitter.Node
import daisy.lang.Identifiers._
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, ValDef => DaisyValDef, _}
import daisy.lang.Constructors._
import daisy.lang.Types._
import daisy.tools.Rational

class TreesitterFPCore(ctx: Context) extends TreesitterCommon(ctx) {

  protected val grammar = "fpcore"
  protected val languageSymbol = "tree_sitter_fpcore"

  protected override val preconditionName = ":pre"
  protected override val constants = Map(
    "E"          -> Math.E,
    "LOG2E"      -> (1.0 / Math.log(2.0)),
    "LOG10E"     -> (1.0 / Math.log(10.0)),
    "LN2"        -> Math.log(2.0),
    "LN10"       -> Math.log(10.0),
    "PI"         -> Math.PI,
    "PI_2"       -> (Math.PI / 2.0),
    "PI_4"       -> (Math.PI / 4.0),
    "M_1_PI"     -> (1.0 / Math.PI),
    "M_2_PI"     -> (2.0 / Math.PI),
    "M_2_SQRTPI" -> (2.0 / Math.sqrt(Math.PI)),
    "SQRT2"      -> Math.sqrt(2.0),
    "SQRT1_2"    -> Math.sqrt(0.5)
  )

  // FPCore has no infix operators
  protected override val binaryOps = Map.empty[String, (DaisyExpr, DaisyExpr) => DaisyExpr]
  protected override val unaryOps = Map.empty[String, DaisyExpr => DaisyExpr]

  protected override val builtins = Map[String, PartialFunction[List[DaisyExpr], DaisyExpr]](
    "+" -> { case a :: b :: Nil => Plus(a, b) },
    "*" -> { case a :: b :: Nil => Times(a, b) },
    "/" -> { case a :: b :: Nil => Division(a, b) },
    "-" -> {
      case a :: Nil      => UMinus(a)
      case a :: b :: Nil => Minus(a, b)
    },

    "fma"  -> { case a :: b :: c :: Nil => FMA(a, b, c) },
    "sqrt" -> { case a :: Nil => Sqrt(a) },
    "exp"  -> { case a :: Nil => Exp(a)  },
    "log"  -> { case a :: Nil => Log(a)  },
    "sin"  -> { case a :: Nil => Sin(a)  },
    "cos"  -> { case a :: Nil => Cos(a)  },
    "tan"  -> { case a :: Nil => Tan(a)  },
    "asin" -> { case a :: Nil => Asin(a) },
    "acos" -> { case a :: Nil => Acos(a) },
    "atan" -> { case a :: Nil => Atan(a) },

    "pow" -> {
      case base :: RealLiteral(r) :: Nil if r.isValidInt => IntPow(base, r.toInt)
      case base :: e :: Nil                              => Exp(Times(e, Log(base)))
    },

    "<"  -> sorted(LessThan),
    "<=" -> sorted(LessEquals),
    ">"  -> sorted(GreaterThan),
    ">=" -> sorted(GreaterEquals),
    "==" -> sorted(Equals),
    "!=" -> distinct,

    "and" -> { case args if args.nonEmpty => andJoin(args) },
    "or"  -> { case args if args.nonEmpty => orJoin(args) },
    "not" -> { case a :: Nil => Not(a) }
  )

  /** Parses chains of comparisons like `x < y < z` into `and(x < y, y < z)`. */
  private def sorted(op: (DaisyExpr, DaisyExpr) => DaisyExpr)
    : PartialFunction[List[DaisyExpr], DaisyExpr] = {
    case args if args.length >= 2 =>
      andJoin(args.zip(args.tail).map { case (a, b) => op(a, b) })
  }

  /** Parses chains of disequalities like `x != y != z` into `and(x != y, y != z, x != z)`. */
  private def distinct: PartialFunction[List[DaisyExpr], DaisyExpr] = {
    case args if args.length >= 2 =>
      andJoin(args.combinations(2).map { case Seq(a, b) => Not(Equals(a, b)) }.toList)
  }

  override protected def convertNode(node: Node): DaisyExpr = node.getType match {

    case "symbol" => lookup(text(node))

    case "constant" =>
      text(node) match {
        case "TRUE"  => BooleanLiteral(true)
        case "FALSE" => BooleanLiteral(false)
        case name =>
          constants.get(name)
            .map(v => RealLiteral(Rational.fromReal(v)))
            .getOrElse(fail(node, s"constant '$name' has no Real counterpart"))
      }

    case "decnum" => RealLiteral(Rational.fromString(text(node)))

    case "rational" =>
      val Array(n, d) = text(node).split("/")
      RealLiteral(Rational(BigInt(n), BigInt(d)))

    case "hexnum" =>
      RealLiteral(Rational.fromReal(java.lang.Double.parseDouble(text(node))))

    // (digits m e b) is m * b^e
    case "digits" =>
      val m = BigInt(text(field(node, "mantissa")))
      val e = text(field(node, "exponent")).toInt
      val b = BigInt(text(field(node, "base")))
      val scale =
        if (e >= 0) Rational(b.pow(e), BigInt(1)) else Rational(BigInt(1), b.pow(-e))
      RealLiteral(Rational(m, BigInt(1)) * scale)

    case "if_expr" =>
      IfExpr(
        convertNode(field(node, "condition")),
        convertNode(field(node, "consequence")),
        convertNode(field(node, "alternative")))

    case "let_expr" => convertLet(node)

    case "application" =>
      val op = field(node, "operator")
      val args = fields(node, "argument").map(convertNode)
      val name = text(op)

      if (op.getType != "operation") invoke(name, args)
      else builtins.get(name) match {
        case Some(f) if f.isDefinedAt(args) => f(args)
        case Some(_) => fail(node, s"'$name' does not take ${args.length} argument(s)")
        case None    => fail(node, s"FPCore operation '$name' has no Daisy equivalent")
      }

    case "annotation" =>
      fields(node, "property").map(p => text(field(p, "name"))).find(isSemantic) match {
        case Some(p) => fail(node, s"annotation property '$p' is not supported yet")
        case None    => convertNode(field(node, "body"))
      }

    case "integer_annotation" =>
      fail(node, "integer annotations are not supported yet")

    case "cast_expr" =>
      fail(node, "cast is not supported yet")

    case other =>
      fail(node, s"FPCore construct '$other' has no Daisy equivalent")
  }

  /** Properties that change what the program computes, as opposed to metadata
    * such as :name or :description. */
  private def isSemantic(property: String): Boolean =
    property == ":precision" || property == ":round" || property == ":math-library"

  /** `let` evaluates every value in the enclosing scope; `let*` lets each value
    * see the bindings before it. Daisy's Let is sequential, so only the order
    * of conversion differs. */
  private def convertLet(node: Node): DaisyExpr = {
    val sequential = allChildren(node).map(text).contains("let*")
    val bindings = allNamedChildren(field(node, "bindings")).toList

    val bound: List[(Identifier, DaisyExpr)] =
      if (sequential) {
        bindings.map { b =>
          val value = convertNode(field(b, "value"))
          (varId(text(field(b, "name"))), value)
        }
      } else {
        val values = bindings.map(b => convertNode(field(b, "value")))
        bindings.map(b => varId(text(field(b, "name")))).zip(values)
      }

    bound.foldRight(convertNode(field(node, "body"))) {
      case ((id, value), body) => Let(id, value, body)
    }
  }
  protected override def convertStmt(node: Node): Stmt =
    fail(node, "FPCore has no statements")

  protected override def findFunctions(root: Node): Seq[Node] =
    allNamedChildren(root).filter(_.getType == "fpcore")

  protected override def convertFunction(node: Node): DaisyFunDef = {
    val name = fieldOpt(node, "name")
      .map(text)
      .getOrElse(s"fpcore_${node.getStartPoint.row() + 1}")

    locals.clear()
    val fid = FreshIdentifier(name)
    functionIds.put(name, fid)
    val params = argumentNames(field(node, "arguments")).map(p => DaisyValDef(varId(p)))

    val preconditions = fields(node, "property")
      .filter(p => text(field(p, "name")) == preconditionName)
      .map(p => convertNode(field(p, "value")))

    DaisyFunDef(
      fid,
      RealType,
      params,
      Option.when(preconditions.nonEmpty)(andJoin(preconditions)),
      body = Some(convertNode(field(node, "body"))),
      postcondition = Some(BooleanLiteral(true)),
      isField = false
    )
  }

  private def argumentNames(argumentList: Node): List[String] =
    allNamedChildren(argumentList).toList.map { arg =>
      arg.getType match {
        case "symbol" => text(arg)

        case "annotated_argument" =>
          fields(arg, "property").map(p => text(field(p, "name"))).find(isSemantic) match {
            case Some(p) => fail(arg, s"annotation property '$p' is not supported yet")
            case None    => text(field(arg, "name"))
          }

        case "tensor_argument" =>
          fail(arg, "tensor arguments have no Daisy equivalent")

        case other =>
          fail(arg, s"argument form '$other' is not supported")
      }
    }
}
