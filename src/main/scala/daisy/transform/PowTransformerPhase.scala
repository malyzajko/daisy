package daisy
package transform

import daisy.lang.TreeOps
import lang.Trees._

/**
  * Transforms expressions of form (x * x * ... * x) to IntPow(x, n) and back, depending on the value of "powers"
  * command line option
  *
  * Motivation: this avoids handling the cases of even powers for times and IntPow
  * simultaneously at the later stages of analysis
  */
object PowTransformerPhase extends DaisyPhase {

  override val name = "Pow transformer"
  override val shortName = "pow-transform"
  override val description = "Converts between expressions of form (x * x * ... * x) and IntPow(x, n)"

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val transformationFunction = if (ctx.hasFlag("pow-roll")) {
        timesToPow
      } else {
        assert(ctx.hasFlag("pow-unroll"))
        powToTimes
      }

    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>
      fnc.copy(body = Some(transformationFunction(fnc.body.get)))
    }

    (ctx, Program(prg.id, newDefs))
  }

  private val rollTimes: PartialFunction[Expr, Expr] = {
    case Times(x, y) if x == y => IntPow(x, 2)
    case Times(x, IntPow(y, n)) if x == y => IntPow(x, n + 1)
    case Times(IntPow(y, n), x) if x == y => IntPow(x, n + 1)
  }

  private val unrollTimes: PartialFunction[Expr, Expr] = {
    case IntPow(x, 1) => x
    case IntPow(x, n) => Times(x, IntPow(x, n -1))
  }

  val timesToPow: Expr => Expr = TreeOps.replace(rollTimes, applyRec = true)
  val powToTimes: Expr => Expr = TreeOps.replace(unrollTimes, applyRec = true)
}
