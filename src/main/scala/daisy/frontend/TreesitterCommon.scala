package daisy
package frontend

import io.github.treesitter.jtreesitter.{Parser, Node, Language}
import java.util.Optional
import java.lang.foreign.{Arena, SymbolLookup}
import scala.io.Source
import scala.collection.mutable
import daisy.lang.Identifiers._
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, Program => DaisyProgram, ValDef => DaisyValDef, _}
import daisy.lang.Constructors._
import daisy.lang.Types._
import daisy.tools.Rational

trait TreesitterCommon {
  val constants = Map(
    "pi" -> Math.PI,
    "M_PI" -> Math.PI,
    "e" -> Math.E
  )

  var currentCtx : Context = null
  var currentSrc : String = _
  protected def preconditionName: String

  protected def optToScala[A](opt: Optional[A]): Option[A] =
    Some(opt.get)

  protected def safeChild(n: Node, i: Int): Option[Node] = {
    optToScala(n.getChild(i))
  }

  protected def safeNamedChild(n: Node, i: Int): Option[Node] = {
    optToScala(n.getNamedChild(i))
  }

  protected final def safeAnyChild(n: Node, i: Int): Option[Node] =
    safeChild(n, i).orElse(safeNamedChild(n, i))

  protected final def textOr(src: String, nodeOpt: Option[Node], default: => String): String =
    nodeOpt.map(extractText(_, src)).getOrElse(default)

  protected def allChildren(n: Node): Seq[Node] = (0 until n.getChildCount).flatMap(i => safeChild(n, i))

  protected def allNamedChildren(n: Node): Seq[Node] = (0 until n.getNamedChildCount).flatMap(i => safeNamedChild(n, i))

  protected def findIdent(n: Node): Option[Node] =
    if (n == null) None else if (n.getType == "identifier") Some(n)
    else allNamedChildren(n).flatMap(findIdent).headOption

  protected def findInit(n: Node): Option[Node] = {
    allNamedChildren(n).find { ch =>
      val t = ch.getType
      t.endsWith("initializer") || t.endsWith("expression")
    } orElse {
      allNamedChildren(n)
        .view
        .flatMap(findInit)
        .headOption
    }
  }
  protected def convertOrZero(n: Option[Node], convertSome: Node => DaisyExpr): DaisyExpr = n.map(convertSome).getOrElse(RealLiteral(Rational.zero))

  protected def unwrapExpr(n: Node): Node = {
    if (n == null) return n

    n.getType match {
      case "parenthesized_expression" | "expression_statement" =>
        val inner = safeNamedChild(n, 0)
        inner.map(unwrapExpr).getOrElse(n)

      case "init_declarator" =>
        safeNamedChild(n, 1).orElse(safeNamedChild(n, 0)).map(unwrapExpr).getOrElse(n)

      case _ =>
        n
    }
  }

  protected val locals = mutable.Map[String, Identifier]()

  protected def varId(n: String): Identifier = locals.getOrElseUpdate(n, FreshIdentifier(n, RealType))

  protected val functionIds = mutable.Map[String, Identifier]()

  protected var precondExpr: Option[DaisyExpr] = None

  protected final def foldBlock(stmts: List[DaisyExpr], acc: DaisyExpr, dropNonLet: Boolean): DaisyExpr =
    stmts.foldRight(acc) {
      case (Let(id, v, Variable(x)), rest) if id == x => Let(id, v, rest)
      case (other, rest)                              => if (dropNonLet) rest else other
    }

  protected final def buildBinaryExpr(op: String, left: DaisyExpr, right: DaisyExpr): DaisyExpr = op match {
    case "+"  => Plus(left, right)
    case "-"  => Minus(left, right)
    case "*"  => Times(left, right)
    case "/"  => Division(left, right)
    case "&&" => And(left, right)
    case "||" => Or(left, right)
    case ">=" => GreaterEquals(left, right)
    case "<=" => LessEquals(left, right)
    case ">"  => GreaterThan(left, right)
    case "<"  => LessThan(left, right)
    case "==" => Equals(left, right)
    case "!=" => Not(Equals(left, right))
    case _    => throw new RuntimeException(s"Unsupported operator '$op'")
  }

  protected final val binaryOperatorTokens: Set[String] =
    Set("&&", "||", ">=", "<=", "==", "!=", ">", "<", "+", "-", "*", "/")

  protected final def convertBinaryByParts(
    node: Node,
    leftNodeOpt: Option[Node],
    opTokenOpt: Option[String],
    rightNodeOpt: Option[Node],
    contextLabel: String,
    allowRawArithmeticFallback: Boolean
  ): DaisyExpr = {
    val leftExpr = convertOrZero(leftNodeOpt.map(unwrapExpr), convertNode)
    val rightExpr = convertOrZero(rightNodeOpt.map(unwrapExpr), convertNode)
    val opToken = opTokenOpt.getOrElse("<unknown>").trim

    try buildBinaryExpr(opToken, leftExpr, rightExpr)
    catch {
      case _: RuntimeException if allowRawArithmeticFallback =>
        val raw = extractText(node, currentSrc)
        if (raw.contains("+")) Plus(leftExpr, rightExpr)
        else if (raw.contains("-")) Minus(leftExpr, rightExpr)
        else if (raw.contains("*")) Times(leftExpr, rightExpr)
        else if (raw.contains("/")) Division(leftExpr, rightExpr)
        else throw new RuntimeException(
          s"Unsupported or unknown binary operator for node '$contextLabel': '${raw.take(120)}'"
        )

      case _: RuntimeException =>
        val raw = extractText(node, currentSrc)
        throw new RuntimeException(
          s"Unsupported operator '$opToken' in $contextLabel. Raw node: '${raw.take(120)}'"
        )
    }
  }

  protected final def findOperatorTripletFromAllChildren(node: Node): (Option[Node], Option[String], Option[Node]) = {
    val children: List[Node] = allChildren(node).toList
    val opIndexOpt = children.zipWithIndex
      .find { case (ch, _) => binaryOperatorTokens.contains(extractText(ch, currentSrc).trim) }
      .map(_._2)

    opIndexOpt match {
      case Some(opIdx) if opIdx > 0 && opIdx < children.length - 1 =>
        (Some(children(opIdx - 1)), Some(extractText(children(opIdx), currentSrc).trim), Some(children(opIdx + 1)))
      case _ =>
        (safeNamedChild(node, 0), None, safeNamedChild(node, 1))
    }
  }

  protected final def convertIfByChildren(
    node: Node,
    condIndex: Int = 0,
    thenIndex: Int = 1,
    elseIndex: Int = 2
  ): DaisyExpr = {
    val condNode = safeNamedChild(node, condIndex).getOrElse(
      throw new Exception("if: missing condition")
    )
    val thenNode = safeNamedChild(node, thenIndex).getOrElse(
      throw new Exception("if: missing then-branch")
    )
    val elseExpr = safeNamedChild(node, elseIndex)
      .map(convertNode)
      .getOrElse(RealLiteral(Rational.zero))

    IfExpr(
      convertNode(condNode),
      convertNode(thenNode),
      elseExpr
    )
  }

  protected def convertNode(raw: Node): DaisyExpr = {
    val node = unwrapExpr(raw)

    node.getType match {
      case "declaration" =>
        val declChildOpt = allNamedChildren(node).find(ch => ch.getType == "init_declarator")

        val (idOpt, initOpt) = declChildOpt match {
          case Some(declChild) =>
            val idNode = safeNamedChild(declChild, 0).orElse(findIdent(declChild))
            val initNode = safeNamedChild(declChild, 1).orElse(findInit(declChild))
            (idNode, initNode)
          case None =>
            (findIdent(node), findInit(node))
        }

        idOpt.map { idn =>
          val idName = extractText(idn, currentSrc)
          val id = varId(idName)
          val initExpr = initOpt.map(convertNode).getOrElse(
            constants.get(idName).map(v => RealLiteral(Rational.fromReal(v))).getOrElse(RealLiteral(Rational.zero))
          )
          Let(id, initExpr, Variable(id))
        }.getOrElse(RealLiteral(Rational.zero))

      case "parenthesized_expression" =>
        val inner = safeNamedChild(node, 0)
          .map(unwrapExpr)
          .getOrElse(throw new Exception("Empty parenthesized expression"))
        convertNode(inner)

      case "assignment" | "assignment_expression" =>
        val id = varId(extractText(safeNamedChild(node, 0).orElse(safeChild(node, 0)).get, currentSrc))
        val expr = convertNode((safeNamedChild(node, 2).orElse(safeChild(node, 2)).get))
        Let(id, expr, Variable(id))

      case "return_expression" | "return_statement" =>
        val exprNode = safeNamedChild(node, 0).getOrElse(throw new Exception("Return without expression"))
        convertNode(exprNode)

      case "identifier" | "type_identifier" =>
        val idText = extractText(node, currentSrc)
        if (locals.contains(idText)) {
          Variable(locals(idText))
        } else {
          constants.get(idText) match {
            case Some(value) => RealLiteral(Rational.fromReal(value))
            case None        => Variable(varId(idText))
          }
        }

      case "number_literal" | "floating_point_literal" | "integer_literal" =>
        extractText(node, currentSrc).toDoubleOption match {
          case Some(d) => RealLiteral(Rational.fromReal(d))
          case None => throw new RuntimeException("Failed to parse numeric literal")
        }

      case "if_expression" | "conditional_expression" =>
          convertIfByChildren(node)

      case "prefix_expression" | "unary_expression" =>
        val firstChild  = safeChild(node, 0)
        val op = extractText(firstChild.getOrElse(node), currentSrc)
        val secondChild = safeChild(node, 1)
        val expr = convertOrZero(secondChild,convertNode)
        op match {
          case "!" => Not(expr)
          case "-" => UMinus(expr)
          case _ => expr
        }
      
      case "call_expression" =>
        val firstChild  = safeChild(node, 0)
        val firstNamedChild  = safeNamedChild(node, 0)
        val fnNode = firstNamedChild.orElse(firstChild).getOrElse(throw new Exception("call_expression: missing function name"))
        val fname = extractText(fnNode, currentSrc)

        val secondChild  = safeChild(node, 1)
        val secondNamedChild  = safeNamedChild(node, 1)
        val argListNode = secondNamedChild.orElse(secondChild)
        val args = argListNode.map(n => allNamedChildren(n).map(convertNode).toList).getOrElse(List.empty)

        fname match {
          case name if name == preconditionName && args.nonEmpty =>
            val cond = args.head
            precondExpr = precondExpr
              .map(existing => And(existing, cond))
              .orElse(Some(cond))
            null 

          case "sqrt" | "sqrtf" => Sqrt(args(0))
          case "sin"  | "sinf"  => Sin(args(0))
          case "cos"  | "cosf"  => Cos(args(0))
          case "tan"  | "tanf"  => Tan(args(0))
          case "exp"  | "expf"  => Exp(args(0))
          case "log"  | "logf"  => Log(args(0))
          case "atan" | "atanf" => Atan(args(0))

          case "pow" | "powf" =>
            args match {
              case base :: RealLiteral(r) :: Nil if r.isValidInt =>
                IntPow(base, r.toInt)
              case base :: exp :: Nil =>
                Exp(Times(exp, Log(base)))
              case _ =>
                throw new Exception(s"pow called with unexpected arguments: $args")
            }
          case _ =>
            val fid = functionIds.getOrElse(fname, FreshIdentifier(fname))
            FunctionInvocation(fid, Seq(), args, RealType)
        }

      case "cast_expression" =>
        val text = extractText(node, currentSrc)
        val msg =
          s"[TS ERROR] Found cast_expression: '$text'. " +
          "Parentheses in expressions need to be rewritten, e.g. (a * (b * c)) -> ((a * b) * c)."
        currentCtx.reporter.error(msg)
        throw new RuntimeException(msg)

      case _ =>
        throw new RuntimeException(
          s"Unsupported node type: '${node.getType}'"
        )
    }
  }

  protected final def parseRootNode(libName: String, languageSymbol: String): Node = {
    val libPath = System.getProperty("user.dir") + s"/lib/$libName"
    System.load(libPath)
    val lookup = SymbolLookup.libraryLookup(libPath, Arena.global())
    val language = Language.load(lookup, languageSymbol)
    val parser = new Parser()
    parser.setLanguage(language)
    parser.parse(currentSrc).orElseThrow().getRootNode
  }

  protected final def buildProgramFromFunctions(
    functionNodes: Seq[Node],
    convertFunction: Node => DaisyFunDef,
    filename: String
  ): DaisyProgram = {
    functionIds.clear()
    val fns = functionNodes.map(convertFunction)
    val baseFilename = new java.io.File(filename).getName
    val baseName = baseFilename.replaceAll("\\.[^.]+$", "")
    DaisyProgram(FreshIdentifier(baseName), fns.toList)
  }

  protected final def keepOnlyVerifiedDefinitions(prog: DaisyProgram): DaisyProgram = {
    val valid = prog.defs.filter(f => f.body.isDefined && f.precondition.isDefined && f.postcondition.isDefined)
    DaisyProgram(prog.id, valid)
  }

  protected final def runTreesitterPhase(
    ctx: Context,
    libName: String,
    languageSymbol: String
  )(
    findFunctions: Node => Seq[Node],
    convertFunction: (Context, Node, String) => DaisyFunDef
  ): (Context, DaisyProgram) = {
    currentSrc = Source.fromFile(ctx.file).mkString
    currentCtx = ctx
    val root = parseRootNode(libName, languageSymbol)
    val prog = buildProgramFromFunctions(findFunctions(root), fn => convertFunction(ctx, fn, currentSrc), ctx.file)
    (ctx, keepOnlyVerifiedDefinitions(prog))
  }

  protected final def extractParamsCommon(
    paramsNodeOpt: Option[Node],
    keepParamNodeIfNoIdentifier: Boolean
  ): List[DaisyValDef] =
    paramsNodeOpt
      .map { paramsNode =>
        allNamedChildren(paramsNode).flatMap { param =>
          val chosen = findIdent(param).orElse(if (keepParamNodeIfNoIdentifier) Some(param) else None)
          chosen.map(idNode => DaisyValDef(varId(extractText(idNode, currentSrc))))
        }.toList
      }
      .getOrElse(Nil)

	protected final def convertFunctionCommon(
    ctx: Context, 
    node: Node, 
    src: String)
    (extractName: Node => String, 
    extractParams: Node => List[DaisyValDef]
    ): DaisyFunDef = {
    currentCtx = ctx
    currentSrc = src
    precondExpr = None
    locals.clear()
		val name = extractName(node)
		val fid = FreshIdentifier(name)
    functionIds.put(name, fid)

		val params = extractParams(node)
    params.foreach { p =>
      locals.update(p.id.name, p.id)
    }
    val bodyExpr = convertOrZero((safeNamedChild(node, node.getNamedChildCount - 1)),convertNode)

		DaisyFunDef(
      fid,
      RealType,
      params,
      precondExpr,
      body = Some(bodyExpr),
      postcondition = Some(BooleanLiteral(true)),
      isField = false
    )

}

	protected def extractText(n: Node, src: String): String =
    try src.substring(n.getStartByte, n.getEndByte) catch { case _: Throwable => "" }
}