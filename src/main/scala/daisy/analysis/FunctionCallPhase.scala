// Copyright 2021 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import lang.Trees._
import daisy.lang.Extractors._
import daisy.lang.Identifiers.FreshIdentifier

/**
This phase either inlines function calls or computes contracts to be used
later during analysis.
 */
object FunctionCallPhase extends DaisyPhase {
  override val name = "functions"
  override val description = "Handles function calls"

  override implicit val debugSection = DebugSectionAnalysis

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    // TODO: do a check: if there is no function call, do nothing

    // for now, just inline
    // TODO: find the right order in which to inline functions, right now it's just
    // the order in which they appear in the input file
    val newDefs = prg.defs.map(fnc => {
      fnc.copy(body = Some(inlineFncCalls(fnc.body.get, prg)))
    })

    // count number of arithmetic operations for statistics purposes
    // keep commented out for experiments
    // val arithCountOrig = prg.defs.map(f => numArithOps(f.body.get)).sum
    // val arithCountInlined = newDefs.map(f => numArithOps(f.body.get)).sum
    // val funcCalls = prg.defs.map(f => numFuncInvoc(f.body.get)).sum

    // println("num arith. ops original: " + arithCountOrig)
    // println("num arith. ops inline  : " + arithCountInlined)
    // println("number of function calls: " +  funcCalls)

    (ctx, Program(prg.id, newDefs))
  }

  def numArithOps(e: Expr): Int = e match {
    case Let(_, value, body) =>
      numArithOps(value) + numArithOps(body)
    case ArithOperator(es, _) =>
      es.map(numArithOps).sum + 1
    case t: Terminal => 0
    case t: FunctionInvocation => 0
  }

  def numFuncInvoc(e: Expr): Int = e match {
    case FunctionInvocation(_, _, args, _) =>
      args.map(numFuncInvoc).sum + 1
    case Let(_, value, body) =>
      numFuncInvoc(value) + numFuncInvoc(body)
    case ArithOperator(es, _) =>
      es.map(numFuncInvoc).sum
    case t: Terminal => 0
  }


  def inlineFncCalls(e: Expr, prg: Program): Expr = {
    lang.TreeOps.replace {
      case fc @ FunctionInvocation(fdId, params: Seq[ValDef], args: Seq[Expr], returnType) =>

        // the function body to inline
        val fncBody = prg.defs.find(_.id == fdId) match {
          case Some(fd) => fd.body.get
        } // throws exception if not found for some reason

        // map parameter to arguments
        val params2args: PartialFunction[Expr, Expr] = params.map({x: ValDef => Variable(x.id)}).zip(args).toMap

        // rename variables
        inlineFncCalls(lang.TreeOps.replace(params2args)(fncBody), prg)

    }(e)
  }

  // use for translation to FPTaylor. it does not work if you have function calls.
  // if you have function calls first use inlineDaisy Translation then apply FPTaylor translation
  // on the produced results
  // rename to inlineFncCalls when you want to translate to FPTaylor
  def inlineFncCallsForFPTaylor(e: Expr, prg: Program): Expr = {
    e match {
      case Let(i, v, b) =>
        v match {
          case Let(i2, v2, b2) =>
            b2 match {
              case Let(i3, v3, b3) =>
                val freshId = FreshIdentifier("res")
                Let(i2, inlineFncCalls(v2, prg), Let(i3, inlineFncCalls(v3, prg), Let(freshId, inlineFncCalls(b3, prg), Let(i, Variable(freshId), inlineFncCalls(b, prg)))))
              case _ =>
                val freshId = FreshIdentifier("res")
                Let(i2, inlineFncCalls(v2, prg), Let(freshId, inlineFncCalls(b2, prg), Let(i, Variable(freshId), inlineFncCalls(b, prg))))
            }
          case _ =>
            Let(i, inlineFncCalls(v, prg), inlineFncCalls(b, prg))
        }
      //
      case ArithOperator(Seq(l, r), recons) =>
        recons(Seq(inlineFncCalls(l, prg), inlineFncCalls(r, prg)))

      case ArithOperator(Seq(u), recons) =>
        recons(Seq(inlineFncCalls(u, prg)))

      case Variable(_) | RealLiteral(_) =>
        e
    }
  }
}
