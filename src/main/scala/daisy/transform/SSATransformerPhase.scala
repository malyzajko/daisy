// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package transform

import scala.collection.immutable.Seq

import lang.Trees._
import lang.Identifiers._
import lang.Types.RealType
import lang.Extractors._
import lang.TreeOps.replace

/**
  Transforms the code into SSA-like form.

  Note:
    - does the transformation for all functions
    - only puts arithmetic in SSA form
    - the order of computation is kept

  Prerequisites:
    None
 */
object SSATransformerPhase extends DaisyPhase {

  override val name = "SSA transformer"
  override val description = "Transforms the function bodies into SSA form."
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {

    reporter = ctx.reporter
    reporter.info("\nStarting SSA transformation phase")
    val timer = ctx.timers.ssa.start

    // need to replace function bodies, create a copy of the whole program
    val newDefs = prg.defs.map(fnc =>
      if (!fnc.body.isEmpty) {

        val newBody = toSSA(fnc.body.get)

        fnc.copy(body = Some(newBody))

      } else {
        fnc
      })

    timer.stop
    reporter.info("Finished SSA transformation phase")

    // return modified program
    (ctx, Program(prg.id, newDefs))
  }

  def fresh(): Identifier = FreshIdentifier("_tmp", RealType, true)

  def toSSA(e: Expr): Expr = e match {
    case t: Terminal => t

    case ArithOperator(Seq(t), recons) =>
      val tree = toSSA(t)
      merge(tree, NoTree(RealType), recons)

    case n @ ArithOperator(Seq(l, r), recons) =>
      val lhs = toSSA(l)
      val rhs = toSSA(r)

      merge(lhs, rhs, recons)


    case Let(id, value, body) =>
      val ssaValue = toSSA(value)

      val ssaBody = toSSA(body)

      val tmp = ssaValue match {
        case l: Let => replaceBody(ssaValue, id, ssaBody)
        case _ =>
          Let(id, ssaValue, ssaBody)
      }

      tmp

  }

  private def replaceBody(expr: Expr, nextId: Identifier, nextBody: Expr): Expr = (expr: @unchecked) match {
    case Let(id, value, x @ Let(id2, value2, body)) =>
      Let(id, value, replaceBody(x, id, nextBody))

    case Let(id, value, body) =>
      Let(id, value,
        Let(nextId, body, nextBody))
  }

  def merge(lhs: Expr, rhs: Expr, recons: (Seq[Expr]) => Expr): Expr = (lhs, rhs) match {
    case (Let(id, v, b), _) if isSimpleExpr(b)=>
      val t1 = fresh()
      Let(id, v,
        Let(t1, b, merge(Variable(t1), rhs, recons)))

    case (Let(id, v, b), _) =>
      Let(id, v, merge(b, rhs, recons))

    case (p1, Let(id, v, b)) if (isSimpleExpr(p1)) =>
      val t1 = fresh()
      Let(t1, p1,
        Let(id, v,
          merge(Variable(t1), b, recons)))

    case (t: Terminal, Let(id, v, b)) if isSimpleExpr(b) =>
      val t1 = fresh()
      Let(id, v,
        Let(t1, b, merge(t, Variable(t1), recons)))


    case (t: Terminal, Let(id, v, b)) =>
      Let(id, v, merge(t, b, recons))

    case (t1: Terminal, NoTree(_)) => recons(Seq(t1))

    case (t1: Terminal, t2: Terminal) => recons(Seq(t1, t2))

    case (v: Terminal, p) if isSimpleExpr(p) =>
      val t1 = fresh()
      Let(t1, p, recons(Seq(v, Variable(t1))))

    case (p, v: Terminal) if isSimpleExpr(p) =>
      val t1 = fresh()
      Let(t1, p, recons(Seq(Variable(t1), v)))

    case (p1, p2) if (isSimpleExpr(p1) && isSimpleExpr(p2))=>
      val t1 = fresh()
      val t2 = fresh()
      Let(t1, p1,
        Let(t2, p2, recons(Seq(Variable(t1), Variable(t2)))))
  }


  def isSimpleExpr(e: Expr): Boolean = e match {
    case Plus(l: Terminal, r: Terminal) => true
    case Minus(l: Terminal, r: Terminal) => true
    case Times(l: Terminal, r: Terminal) => true
    case Division(l: Terminal, r: Terminal) => true
    case UMinus(l: Terminal) => true
    case Sqrt(l: Terminal) => true
    case _ => false
  }
}
