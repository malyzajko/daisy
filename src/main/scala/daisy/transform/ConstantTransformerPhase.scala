

package daisy
package transform

import scala.collection.immutable.Seq

import lang.Trees._
import lang.Identifiers._
import lang.Types.RealType
import lang.Extractors._

/**
  Pulls out constants. e.g.
  from
    (((((((-3 * u) * u) * u) + ((3 * u) * u)) + (3 * u)) + 1) / 6.0)
  to
    val _const0: Double = -3.0
    val _const1: Double = 3.0
    val _const2: Double = 3.0
    val _const3: Double = 1.0
    val _const4: Double = 6.0
    (((((((_const0 * u) * u) * u) + ((_const1 * u) * u)) + (_const2 * u)) + _const3) / _const4)

  Prerequisites:
    None
  */
object ConstantTransformerPhase extends DaisyPhase {

  override val name = "Constant transformer"
  override val shortName = "const-trans"
  override val description = "Pulls out constants"

  implicit val debugSection = DebugSectionTransform

  var reporter: Reporter = null

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    // need to replace function bodies, so create a copy of the whole program

    val functionsToTransform = if (ctx.hasFlag("approx")) prg.defs.filter(_.returnType == RealType) else prg.defs
    val newDefs = functionsToTransform.map(fnc =>
      if (fnc.body.isDefined) {
        
        val newBody = pullOutConstants(fnc.body.get)
        fnc.copy(body = Some(newBody))

      } else {
        fnc
      })


    (ctx, Program(prg.id, newDefs ++ prg.defs.diff(functionsToTransform)))
  }

  def pullOutConstants(expr: Expr): Expr = {
    // generate vals for all constants
    def toLets(consts: Seq[(Identifier, RealLiteral)], lastExpr: Expr): Expr = {
      if (consts.length > 0) {
        val (fresh, const) = consts.head
        Let(fresh, const, toLets(consts.tail, lastExpr))
      } else {
        lastExpr
      }
    }

    // find all constants
    var counter = 0


    def mapConstants(e: Expr): (Expr, Seq[(Identifier, RealLiteral)]) = (e: @unchecked) match {
      case v: Variable => (v, Seq())
      case x @ RealLiteral(r) =>
        val fresh = FreshIdentifier("_const" + counter, RealType)
        counter = counter + 1
        //constants = constants :+ (fresh, x)
        (Variable(fresh), Seq((fresh, x)))

      case ArithOperator(es, recons) =>
        val (exprs, constants) = es.map(mapConstants).unzip
        (recons(exprs), constants.flatten)

      case IfExpr(cond, thenn, elze) =>
        // don't extract constants from conditionals for now
        val (ifExpr, ifConsts) = mapConstants(thenn)
        val (elseExpr, elseConst) = mapConstants(elze)

        (IfExpr(cond, toLets(ifConsts, ifExpr), toLets(elseConst, elseExpr)), Seq())

      case Let(id, r @ RealLiteral(_), b) => 
        val (expr, consts) = mapConstants(b)
        (Let(id, r, expr), consts)

      case Let(id, v, b) => 
        val (vExpr, vConsts) = mapConstants(v)
        val (bExpr, bConsts) = mapConstants(b)
        
        (Let(id, vExpr, bExpr), vConsts ++ bConsts)

      case x @ ApproxPoly(orig, arg, funId, err) =>
        val (expr, consts) = mapConstants(arg)
        (ApproxPoly(orig, expr, funId, err), consts)

      case x @ Cast(castedExpr, typ) =>
        val (expr, consts) = mapConstants(castedExpr)
        (Cast(expr, typ), consts)
    }

    val (exprNew, consts) = mapConstants(expr)


    toLets(consts, exprNew)

  }
}
