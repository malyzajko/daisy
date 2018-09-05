

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

    val newDefs = prg.defs.map(fnc =>
      if (!fnc.body.isEmpty) {

        val newBody = pullOutConstants(fnc.body.get)

        fnc.copy(body = Some(newBody))

      } else {
        fnc
      })

    //println("new program: " + newDefs)

    (ctx, Program(prg.id, newDefs))
  }

  def pullOutConstants(expr: Expr): Expr = {
    // find all constants
    var constants = Seq[(Identifier, RealLiteral)]()
    var counter = 0


    def mapConstants(e: Expr): Expr = (e: @unchecked) match {
      case v: Variable => v
      case x @ RealLiteral(r) =>
        val fresh = FreshIdentifier("_const" + counter, RealType)
        counter = counter + 1
        constants = constants :+ (fresh, x)
        Variable(fresh)

      case ArithOperator(es, recons) =>
        recons(es.map(mapConstants(_)))

      case IfExpr(cond, thenn, elze) =>
        IfExpr(mapConstants(cond), mapConstants(thenn), mapConstants(elze))

      case GreaterThan(l, r) => GreaterThan(mapConstants(l), mapConstants(r))
      case GreaterEquals(l, r) => GreaterEquals(mapConstants(l), mapConstants(r))
      case LessThan(l, r) => LessThan(mapConstants(l), mapConstants(r))
      case LessEquals(l, r) => LessEquals(mapConstants(l), mapConstants(r))

      case Let(id, r @ RealLiteral(_), b) => Let(id, r, mapConstants(b))
      case Let(id, v, b) => Let(id, mapConstants(v), mapConstants(b))
    }

    val expr2 = mapConstants(expr)

    // generate vals for all constants
    def makeIntoLets(consts: Seq[(Identifier, RealLiteral)]): Expr = {
      if (consts.length > 0) {
        val (fresh, const) = consts.head
        Let(fresh, const, makeIntoLets(consts.tail))
      } else {
        expr2
      }
    }

    makeIntoLets(constants)

  }
}
