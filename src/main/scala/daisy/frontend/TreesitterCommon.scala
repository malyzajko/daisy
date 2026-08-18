package daisy
package frontend

import io.github.treesitter.jtreesitter.{Parser, Node, Language}
import java.lang.foreign.{Arena, SymbolLookup}
import scala.jdk.CollectionConverters._
import scala.io.Source
import scala.collection.mutable
import scala.jdk.OptionConverters._
import daisy.lang.Identifiers._
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, Program => DaisyProgram, ValDef => DaisyValDef, _}
import daisy.lang.Constructors._
import daisy.lang.Types._
import daisy.tools.Rational

abstract class TreesitterCommon(protected val ctx: Context) {

  protected def grammar: String
  protected def languageSymbol: String

  /** Parse this frontend's language and build the program. */
  final def run: (Context, DaisyProgram) = {
    val root = parseRootNode(grammar, languageSymbol)
    functionIds.clear()
    val fns = findFunctions(root).map(convertFunction).toList
    val baseName = new java.io.File(ctx.file).getName.replaceAll("\\.[^.]+$", "")
    (ctx, keepOnlyVerifiedDefinitions(DaisyProgram(FreshIdentifier(baseName), fns)))
  }

  /** Name of the annotation that marks a function's precondition. **/
  protected def preconditionName: String

  /** Mathematical constants */
  protected def constants: Map[String, Double]

  /** Binary operators **/
  protected def binaryOps: Map[String, (DaisyExpr, DaisyExpr) => DaisyExpr]

  protected def convertBinary(op: String, l: DaisyExpr, r: DaisyExpr, at: Node): DaisyExpr =
    binaryOps.getOrElse(op, fail(at, s"unsupported binary operator '$op'"))(l, r)

  /** Unary operators. */
  protected def unaryOps: Map[String, DaisyExpr => DaisyExpr]

  protected def convertUnary(op: String, e: DaisyExpr, at: Node): DaisyExpr =
    unaryOps.getOrElse(op, fail(at, s"unsupported unary operator '$op'"))(e)

  /** Mathematical functions */
  protected def builtins: Map[String, PartialFunction[List[DaisyExpr], DaisyExpr]]

  /** Converts a node in the frontend's grammar to a Daisy expression **/
  protected def convertNode(node: Node): DaisyExpr

  protected val locals = mutable.Map[String, Identifier]()

  protected def varId(n: String): Identifier = locals.getOrElseUpdate(n, FreshIdentifier(n, RealType))

  protected def lookup(name: String): DaisyExpr =
    locals.get(name).map(Variable(_))
      .orElse(constants.get(name).map(v => RealLiteral(Rational.fromReal(v))))
      .getOrElse(Variable(varId(name)))


  protected def fail(n: Node, msg: String): Nothing = {
    val p = n.getStartPoint
    ctx.reporter.fatalError(s"${ctx.file}:${p.row() + 1}:${p.column() + 1}: $msg")
  }

  /** Fetches a field that the grammar marks required */
  protected def field(n: Node, name: String): Node =
    n.getChildByFieldName(name).toScala.getOrElse(
      fail(n, s"'${n.getType}' has no '$name' field"))

  /** Fetches a field that the grammar marks optional */
  protected def fieldOpt(n: Node, name: String): Option[Node] =
    n.getChildByFieldName(name).toScala

  /** Fetches all children that the grammar marks with this field name. */
  protected def fields(n: Node, name: String): List[Node] =
    n.getChildrenByFieldName(name).asScala.toList

  /** The node's source text */
  protected def text(n: Node): String =
    Option(n.getText).getOrElse(fail(n, "node has no source text"))

  protected def allChildren(n: Node): Seq[Node] = n.getChildren().asScala.toSeq

  protected def allNamedChildren(n: Node): Seq[Node] = n.getNamedChildren().asScala.toSeq

  protected def findFunctions(root: Node): Seq[Node]

  /** In most cases relies on makeFunDef **/
  protected def convertFunction(node: Node): DaisyFunDef

  protected val functionIds = mutable.Map[String, Identifier]()

  protected def convertCall(name: String, args: List[DaisyExpr], at: Node): DaisyExpr =
    builtins.get(name) match {
      case Some(f) if f.isDefinedAt(args) => f(args)
      case Some(_) => fail(at, s"'$name' does not take ${args.length} argument(s)")
      case None    => invoke(name, args)
    }

  /** A call to another function in the program being parsed. */
  protected def invoke(name: String, args: List[DaisyExpr]): DaisyExpr =
    FunctionInvocation(functionIds.getOrElse(name, FreshIdentifier(name)), Seq(), args, RealType)

  protected final def makeFunDef(
    name: String,
    paramNames: List[String],
    bodyNode: Node
  ): DaisyFunDef = {
    locals.clear()

    val fid = FreshIdentifier(name)
    functionIds.put(name, fid)

    val params: List[DaisyValDef] = paramNames.map(p => DaisyValDef(varId(p)))
    val stmts: List[Stmt] = stmtsOf(bodyNode)
    val preconditions: List[DaisyExpr] = stmts.collect { case Pre(c) => c }
    val precondition: Option[DaisyExpr] = Option.when(preconditions.nonEmpty)(andJoin(preconditions))
    val bodyExpr: DaisyExpr = foldBinds(bodyNode, stmts)

    DaisyFunDef(
      fid,
      RealType,
      params,
      precondition,
      body = Some(bodyExpr),
      postcondition = Some(BooleanLiteral(true)),
      isField = false
    )
  }

  /** Statements **/
  protected sealed trait Stmt
  protected case class Bind(id: Identifier, value: DaisyExpr) extends Stmt
  protected case class Pre(cond: DaisyExpr) extends Stmt
  protected case class Result(expr: DaisyExpr) extends Stmt

  protected def convertStmt(node: Node): Stmt

  private def stmtsOf(block: Node): List[Stmt] =
    allNamedChildren(block).toList.map(convertStmt)

  private def foldBinds(at: Node, stmts: List[Stmt]): DaisyExpr = {
    val result = stmts.lastOption match {
      case Some(Result(e))   => e
      case _                 => fail(at, "block has no value")
    }

    stmts.collect { case b: Bind => b }
      .foldRight(result) { case (Bind(id, v), body) => Let(id, v, body) }
  }

  protected final def convertBlock(node: Node): DaisyExpr = {
    val stmts = stmtsOf(node)
    stmts.foreach {
      case Pre(_) => fail(node, s"$preconditionName is only allowed at the top level of a function")
      case _      => ()
    }
    foldBinds(node, stmts)
  }

  protected final def parseRootNode(grammar: String, languageSymbol: String): Node = {
    TreesitterCommon.loadCoreRuntime(ctx)
    val libPath = TreesitterCommon.libraryPath(s"tree-sitter-$grammar")

    if (!new java.io.File(libPath).exists) {
      ctx.reporter.fatalError(
        s"$libPath not found.")
    }
    else{
      val lookup = SymbolLookup.libraryLookup(libPath, Arena.global())
      val language = Language.load(lookup, languageSymbol)
      val parser = new Parser()
      parser.setLanguage(language)

      val root: Node = parser.parse(Source.fromFile(ctx.file).mkString).toScala
        .getOrElse(ctx.reporter.fatalError(s"parsing ${ctx.file} was halted"))
        .getRootNode

      // Tree-sitter recovers from syntax errors rather than failing;
      // we report the first error or missing node
      if (root.hasError) {
        val firstError = firstErrorNode(root).getOrElse(root)
        val p = firstError.getStartPoint
        val what = if (firstError.isMissing) s"missing '${firstError.getType}'" else "syntax error"
        ctx.reporter.fatalError(s"${ctx.file}:${p.row() + 1}:${p.column() + 1}: $what")
      }
      root
    }
  }

  /** Walks the tree to find the first node that is an error or missing. */
  private def firstErrorNode(n: Node): Option[Node] =
    if (n.isError || n.isMissing) Some(n)
    else allChildren(n).view.flatMap(firstErrorNode).headOption



  protected final def keepOnlyVerifiedDefinitions(prog: DaisyProgram): DaisyProgram = {
    val valid = prog.defs.filter(f => f.body.isDefined && f.precondition.isDefined && f.postcondition.isDefined)
    DaisyProgram(prog.id, valid)
  }



}
object TreesitterCommon {
  /** Instead of guessing the library name, the frontend asks the sbt build for it. **/
  def libraryPath(name: String): String = {
    val file = TreesitterBuildInfo.libraries.getOrElse(name,
      sys.error(s"no tree-sitter library is configured for '$name'"))
    s"${System.getProperty("user.dir")}/lib/$file"
  }

  private val treeSitterPath = libraryPath("tree-sitter")

  private lazy val core: Unit = System.load(treeSitterPath)

  def loadCoreRuntime(ctx: Context): Unit = {
    val path = treeSitterPath
    if (!new java.io.File(path).exists)
      ctx.reporter.fatalError(s"$path not found.")
    core   // lazy val: loaded exactly once per JVM, thread-safely
  }
}
