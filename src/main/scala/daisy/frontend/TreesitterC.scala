package daisy
package frontend

import io.github.treesitter.jtreesitter.Node
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, Program => DaisyProgram, _}
import daisy.lang.Constructors._
import daisy.tools.Rational

class TreesitterC(ctx: Context) extends TreesitterCommon(ctx) {

  protected override val grammar = "c"
  protected override val languageSymbol = "tree_sitter_c"

  protected override val preconditionName = "__PRECOND"

  protected override val constants = Map(
    "M_PI" -> Math.PI,
    "M_E"  -> Math.E
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
    "sqrt"  -> { case a :: Nil => Sqrt(a) },
    "sqrtf" -> { case a :: Nil => Sqrt(a) },
    "sin"   -> { case a :: Nil => Sin(a)  }, 
    "sinf"  -> { case a :: Nil => Sin(a)  },
    "cos"   -> { case a :: Nil => Cos(a)  }, 
    "cosf"  -> { case a :: Nil => Cos(a)  },
    "tan"   -> { case a :: Nil => Tan(a)  }, 
    "tanf"  -> { case a :: Nil => Tan(a)  },
    "asin"  -> { case a :: Nil => Asin(a) }, 
    "asinf" -> { case a :: Nil => Asin(a) },
    "acos"  -> { case a :: Nil => Acos(a) }, 
    "acosf" -> { case a :: Nil => Acos(a) },
    "atan"  -> { case a :: Nil => Atan(a) }, 
    "atanf" -> { case a :: Nil => Atan(a) },
    "exp"   -> { case a :: Nil => Exp(a)  }, 
    "expf"  -> { case a :: Nil => Exp(a)  },
    "log"   -> { case a :: Nil => Log(a)  }, 
    "logf"  -> { case a :: Nil => Log(a)  },
    "fma"  -> { case a :: b :: c :: Nil => FMA(a, b, c) },
    "pow"  -> powCases, "powf" -> powCases
  )

  private def powCases: PartialFunction[List[DaisyExpr], DaisyExpr] = {
    case base :: RealLiteral(r) :: Nil if r.isValidInt => IntPow(base, r.toInt)
    case base :: e :: Nil                              => Exp(Times(e, Log(base)))
  }

  protected override def convertNode(node: Node): DaisyExpr = node.getType match {

    case "binary_expression" =>
      convertBinary(
        text(field(node, "operator")),
        convertNode(field(node, "left")),
        convertNode(field(node, "right")),
        node)

    case "unary_expression" =>
      convertUnary(text(field(node, "operator")), convertNode(field(node, "argument")), node)

    case "conditional_expression" =>
      IfExpr(
        convertNode(field(node, "condition")),
        convertNode(field(node, "consequence")),
        convertNode(field(node, "alternative")))

    case "call_expression" =>
      val name = text(field(node, "function"))
      val args = allNamedChildren(field(node, "arguments")).map(convertNode).toList
      convertCall(name, args, node)

    case "identifier" => lookup(text(node))

    // C literals carry type suffixes: 3.5f, 10L, 1e3F.
    case "number_literal" =>
      val raw = text(node)
      raw.reverse.dropWhile(c => "fFlLuU".contains(c)).reverse.toDoubleOption match {
        case Some(d) => RealLiteral(Rational.fromReal(d))
        case None    => fail(node, s"cannot read '$raw' as a number")
      }

    case "parenthesized_expression" =>
      allNamedChildren(node).headOption
        .map(convertNode)
        .getOrElse(fail(node, "empty parentheses"))

    case "compound_statement" =>
      convertBlock(node)

    // `(b * c)` following an identifier parses as a cast when `b` is not a known
    // type.
    case "cast_expression" =>
      fail(node, s"'${text(node)}' was parsed as a cast, not a product")

    case other =>
      fail(node, s"C construct '$other' is not supported")
  }

  protected override def convertStmt(node: Node): Stmt = node.getType match {
    case "declaration" =>
      val decl = field(node, "declarator")
      if (decl.getType != "init_declarator")
        fail(node, s"'${text(node)}' declares a variable without a value")
      Bind(varId(text(field(decl, "declarator"))), convertNode(field(decl, "value")))

    case "expression_statement" =>
      allNamedChildren(node).headOption match {
        case None => fail(node, "empty statement")
        case Some(e) => e.getType match {
          case "assignment_expression" =>
            Bind(varId(text(field(e, "left"))), convertNode(field(e, "right")))
          case "call_expression" if text(field(e, "function")) == preconditionName =>
            allNamedChildren(field(e, "arguments")).map(convertNode).toList match {
              case cond :: Nil => Pre(cond)
              case args => fail(e, s"$preconditionName takes 1 argument, got ${args.length}")
            }
          case _ => Result(convertNode(e))
        }
      }

    case "return_statement" =>
      allNamedChildren(node).headOption
        .map(e => Result(convertNode(e)))
        .getOrElse(fail(node, "return without a value"))

    // C's `if` is a statement, not an expression. Daisy can represent one only
    // when both branches assign the same variable: `if (c) t = a; else t = b;`
    case "if_statement" =>
      val cond = convertNode(field(node, "condition"))
      val thenB = convertStmt(field(node, "consequence"))
      val elseB = fieldOpt(node, "alternative")
        .map(convertStmt)
        .getOrElse(fail(node, "if without an else branch has no value"))
      (thenB, elseB) match {
        case (Bind(t, tv), Bind(e, ev)) if t == e => Bind(t, IfExpr(cond, tv, ev))
        case _ => fail(node,
          "only an if whose branches both assign the same variable is supported")
      }

    case _ => Result(convertNode(node))
  }


  protected override def findFunctions(root: Node): Seq[Node] =
    allNamedChildren(root).filter(_.getType == "function_definition")

  protected override def convertFunction(node: Node): DaisyFunDef = {
    val decl = field(node, "declarator")
    val paramNames = allNamedChildren(field(decl, "parameters"))
      .filter(_.getType == "parameter_declaration")
      .map(p => text(field(p, "declarator")))
      .toList
    makeFunDef(text(field(decl, "declarator")), paramNames, field(node, "body"))
  }
}
