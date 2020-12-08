package daisy
package transform

import scala.collection.immutable.Seq

import lang.Trees._
import lang.Extractors._
import lang.Identifiers._

/*
  This Phase assigns elementary function calls to local variables. This is a
  prerequisite to the MetalibmPhase which will approximate the entire right-hand-side
  of a local variable assignment which contains an elementary function.

  The depth parameter controls whether a compound function will be split up.
  E.g. for depth=0, the entire argument of each elementary function call is also
  assigned to local variable, for depth=1, the argument of AST-height 1 is left
  and the remaining part is assigned to a local variable.
 */
object DecompositionPhase extends DaisyPhase {

  override val name = "Decomposition"
  override val description = "Expression decomposition for Metalibm"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    NumOption("depth", 0, "depth of decomposed trees")
  )

  override implicit val debugSection = DebugSectionTransform

  var reporter: Reporter = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val depth = ctx.option[Long]("depth").toInt
    reporter = ctx.reporter


    val newDefs = transformConsideredFunctions(ctx,prg){ fnc =>
      val (lets, body) = traverse(fnc.body.get, depth)

      val newBody = reconstructLet(lets, body)

      fnc.copy(body = Some(newBody))
    }
    (ctx.copy(originalFunctions = prg.defs.map(f => (f.id, f)).toMap),
      Program(prg.id, newDefs))
  }

  def freshID: Identifier = FreshIdentifier("_tmp", lang.Types.RealType, true)

  def traverse(e: Expr, maxDepth: Int): (Seq[(Identifier, Expr)], Expr) = e match {

    case x @ ArithOperator(_, _) if (isUnary(x) && containsElemFnc(x) && depthToLowestElemFnc(x) <= maxDepth) =>
      // replace by local variable
      val newLocal = freshID
      (Seq((newLocal, x)), Variable(newLocal))

    // case elemFnc(non-unary):
    case x @ ArithOperator(Seq(t), recons) =>
      val newLocal = freshID
      val (lets, newT) = traverse(t, maxDepth)
      val newLocalElem = freshID
      (lets ++ Seq((newLocal, newT), (newLocalElem, recons(Seq(Variable(newLocal))))), Variable(newLocalElem))

    // cannot be replaced completely
    case x @ ArithOperator(args, recons) if (containsElemFnc(x)) =>
      val (lets: Seq[Seq[(Identifier, Expr)]], newArgs: Seq[Expr]) = args.map(traverse(_, maxDepth)).unzip
      (lets.flatten, recons(newArgs))

    case Let(id, value @ ArithOperator(Seq(t), _), body) =>
      //val (lets, newValue) = traverse(value, maxDepth)
      val (letsB, newBody) = traverse(body, maxDepth)
      (Seq(), reconstructLet(Seq((id, value)) ++ letsB, newBody))

    case Let(id, value, body) =>
      val (lets, newValue) = traverse(value, maxDepth)
      val (letsB, newBody) = traverse(body, maxDepth)
      (Seq(), reconstructLet(lets ++ Seq((id, newValue)) ++ letsB, newBody))

    case _ => (Seq(), e)

  }

  def reconstructLet(mapping: Seq[(Identifier, Expr)], last: Expr): Expr = {
    if (mapping.isEmpty) last
    else {
      val (id, expr) = mapping.head
      Let(id, expr, reconstructLet(mapping.tail, last))
    }
  }

  def depthToLowestElemFnc(e: Expr): Int = e match {
    // last elementary function
    case x @ ArithOperator(Seq(t), _) if (!containsElemFnc(t)) => 0

    case x @ ArithOperator(args, _) =>
      val depths = args.map(arg => {
        if (containsElemFnc(arg)) depthToLowestElemFnc(arg)
        else 0
      })
      depths.max + 1

    case t: Terminal => 0
  }

  // TODO: duplicate functions from MetalibmPhase, cleanup
  def containsElemFnc(e: Expr): Boolean = {
    lang.TreeOps.exists {
      case Sin(_) | Cos(_) | Tan(_) | Exp(_) | Log(_) | Sqrt(_) => true
    }(e)
  }

  def isUnary(e: Expr): Boolean = {
    lang.TreeOps.allVariablesOf(e).size == 1
  }
}
