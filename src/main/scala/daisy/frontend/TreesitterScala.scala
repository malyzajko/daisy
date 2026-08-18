package daisy
package frontend

import io.github.treesitter.jtreesitter.Node
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, Program => DaisyProgram, _}
import daisy.lang.Constructors._
import daisy.tools.Rational

class TreesitterScala(ctx: Context) extends TreesitterCommon(ctx) {

  protected override val grammar = "scala"
  protected override val languageSymbol = "tree_sitter_scala"

  protected override val preconditionName = "require"

  protected override val constants = Map(
    "pi" -> Math.PI,
    "e"  -> Math.E
  )

  protected override val binaryOps = Map[String, (DaisyExpr, DaisyExpr) => DaisyExpr](
    "+"  -> Plus,
    "-"  -> Minus,
    "*"  -> Times,
    "/"  -> Division,
    ">"  -> GreaterThan,
    ">=" -> GreaterEquals,
    "<"  -> LessThan,
    "<=" -> LessEquals,
    "==" -> Equals,
    "!=" -> ((l, r) => Not(Equals(l, r))),
    "&&" -> ((l, r) => and(l, r)),
    "||" -> ((l, r) => or(l, r))
  )

  protected override val unaryOps = Map[String, DaisyExpr => DaisyExpr](
    "-" -> UMinus,
    "!" -> Not,
    "+" -> identity
  )

  protected override val builtins = Map[String, PartialFunction[List[DaisyExpr], DaisyExpr]](
    "sqrt" -> { case a :: Nil => Sqrt(a) },
    "sin"  -> { case a :: Nil => Sin(a)  },
    "cos"  -> { case a :: Nil => Cos(a)  },
    "tan"  -> { case a :: Nil => Tan(a)  },
    "asin" -> { case a :: Nil => Asin(a) },
    "acos" -> { case a :: Nil => Acos(a) },
    "atan" -> { case a :: Nil => Atan(a) },
    "exp"  -> { case a :: Nil => Exp(a)  },
    "log"  -> { case a :: Nil => Log(a)  },
    "fma"  -> { case a :: b :: c :: Nil => FMA(a, b, c) },
    "pow"  -> {
      case base :: RealLiteral(r) :: Nil if r.isValidInt => IntPow(base, r.toInt)
      case base :: e :: Nil                              => Exp(Times(e, Log(base)))
    }
  )

  override protected def convertNode(node: Node): DaisyExpr = node.getType match {

    case "infix_expression" =>
      convertBinary(
        text(field(node, "operator")),
        convertNode(field(node, "left")),
        convertNode(field(node, "right")),
        node)

    // The operator is the first child and is anonymous,
    // the operand is the only named child.
    case "prefix_expression" =>
      val op = allChildren(node).headOption
        .map(text)
        .getOrElse(fail(node, "prefix expression without an operator"))
      val operand = allNamedChildren(node).headOption
        .getOrElse(fail(node, s"'$op' without an operand"))
      convertUnary(op, convertNode(operand), node)

    case "if_expression" =>
      val elze = fieldOpt(node, "alternative")
        .map(convertNode)
        .getOrElse(fail(node, "if without an else branch has no value"))
      IfExpr(
        convertNode(field(node, "condition")),
        convertNode(field(node, "consequence")),
        elze)

    case "call_expression" =>
      val name = text(field(node, "function"))
      val args = allNamedChildren(field(node, "arguments")).map(convertNode).toList
      convertCall(name, args, node)

    case "identifier" => lookup(text(node))

    case "floating_point_literal" | "integer_literal" =>
      text(node).toDoubleOption match {
        case Some(d) => RealLiteral(Rational.fromReal(d))
        case None    => fail(node, s"cannot read '${text(node)}' as a number")
      }

    case "parenthesized_expression" =>
      allNamedChildren(node).headOption
        .map(convertNode)
        .getOrElse(fail(node, "empty parentheses"))

    case "block" | "indented_block" =>
      convertBlock(node)

    case other =>
      fail(node, s"Scala construct '$other' is not supported")
  }

  protected override def convertStmt(node: Node): Stmt = node.getType match {

    case "val_definition" =>
      val pat = field(node, "pattern")
      if (pat.getType != "identifier")
        fail(pat, s"only `val x = ...` is supported, found '${pat.getType}'")
      Bind(varId(text(pat)), convertNode(field(node, "value")))

    case "call_expression" if text(field(node, "function")) == preconditionName =>
      allNamedChildren(field(node, "arguments")).map(convertNode).toList match {
        case cond :: Nil => Pre(cond)
        case args => fail(node, s"$preconditionName takes 1 argument, got ${args.length}")
      }

    case _ => Result(convertNode(node))
  }

  protected override def findFunctions(root: Node): Seq[Node] =
    if (root.getType == "function_definition") Seq(root)
    else allNamedChildren(root).flatMap(findFunctions)


  protected override def convertFunction(node: Node): DaisyFunDef = {
    val paramNames = fields(node, "parameters") match {
      case Nil         => Nil
      case sole :: Nil =>
        allNamedChildren(sole)
          .filter(_.getType == "parameter")
          .map(p => text(field(p, "name")))
          .toList
      case more =>
        fail(node, s"curried parameter lists are not supported (${more.length} clauses)")
    }
    makeFunDef(text(field(node, "name")), paramNames, field(node, "body"))
  }
}
