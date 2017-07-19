

package daisy
package frontend

import scala.collection.immutable.Seq
import scala.util.Random

import lang.Trees._
import lang.Identifiers._
import lang.Types._
import utils.Rational
import Rational._
import lang.TreeOps._


/**
  Generates random expressions, with knobs to control
  nonlinearity, number of variables and the size

  Prerequisites:
    -
 */
object RandomCodeGeneration {//extends DaisyPhase {

  //override val name = "random code generation"
  //override val description = "Generates random expressions"
  //override val definedOptions: Set[OptionDef] = Set()
  //implicit val debugSection =

  var reporter: Reporter = null

  def run(ctx: Context): Program = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting random code gen")
    val timer = ctx.timers.randomGen.start

    // Create a new program here, which will then be printed in the
    // code generation phase

    val fncs = for (i <- 0 until 10) yield {

      // TODO: add several variables
      val vars = Seq(FreshIdentifier("x", RealType))

      var generatedBody: Expr = null
      while(generatedBody == null) { // no suitable body found

        var expr = randomGenerate(5, vars)

        // needs to be done several times, to clean up the zeros, and one's
        var change = true
        var i = 0
        while(change) {
           val tmp = simplify(expr)
          change = (expr != tmp)
          expr = tmp
          i = i + 1
        }
        reporter.info("simplification iterations needed: " + i)

        // perform some check? (check min. size?)
        if (size(expr) > 15) {
          generatedBody = expr
        }
      }



      FunDef(
        FreshIdentifier("f" + i), // id: Identifier
        RealType,                     // returnType: TypeTree
        vars.map(ValDef(_)),               //params: Seq[ValDef],
        // Some(And(
        //   vars.flatMap(x => Seq(LessEquals(RealLiteral(one), Variable(x)),
        //   LessEquals(Variable(x), RealLiteral(Rational(10)))))
        //      )
        // ),
        None,                         //precondition: Option[Expr],
        Some(generatedBody),                            //body: Option[Expr],
        None,                         //postcondition: Option[Expr],
        false                         //isField: Boolean = false
        )

    }

    //val defs: Seq[FunDef] = Seq(fnc)

    val prg = Program(FreshIdentifier("RandomProgram"), fncs)


    timer.stop
    reporter.info(s"Finished random code gen")
    prg
  }

  val seed = System.currentTimeMillis
  println(seed)
  val rand = new Random(seed)

  // probabilities
  //val probRecurse = 0.7


  // Generates a tree of height at most @height.
  private def randomGenerate(maxHeight: Int, vars: Seq[Identifier]): Expr = {

    def go(n: Int): Expr = {
      // choose: recurse further or stop?
      // probability of recursing decreases with every level,
      // and after maxHeight steps becomes 0
      if (rand.nextDouble <= (maxHeight - n)) {
        rand.nextInt(5) match {
          case 0 =>
            Plus(go(n + 1), go(n + 1))

          case 1 =>
            Minus(go(n + 1), go(n + 1))

          case 2 =>
            Times(go(n + 1), go(n + 1))

          case 3 =>
            Division(go(n + 1), go(n + 1))

          case 4 =>
            UMinus(go(n + 1))

        }
      } else {
        // choose at random which option
        // TODO: add possibility of constats too
        Variable(vars(0))
      }
    }
    go(0)
  }

  // TODO: this is very adhoc
  private def simplify(e: Expr): Expr = e match {
    // x / x
    case Division(l, r) if (l == r) => RealLiteral(one)

    // x - x
    case Minus(l, r) if (l == r) => RealLiteral(zero)

    // - -  (double minus)
    case UMinus(UMinus(t)) => t

    // l - -(r)   // this changes nothing
    case Minus(l, UMinus(r)) => Plus(l, r)


    //cleanup
    case Times(_, RealLiteral(zero)) => RealLiteral(zero)
    case Times(RealLiteral(zero), _) => RealLiteral(zero)

    case Plus(l, RealLiteral(zero)) => l
    case Plus(RealLiteral(zero), r) => r

    case UMinus(RealLiteral(r)) => RealLiteral(-r)   //changes nothing

    case Minus(l, RealLiteral(zero)) => l
    case Minus(RealLiteral(zero), r) => UMinus(r)

    // this is not semantically correct, but / 0.0 just doesn't make any sense
    case Division(l, RealLiteral(zero)) => l
    case Division(RealLiteral(zero), r) => r

    case Plus(l, r) => Plus(simplify(l), simplify(r))
    case Minus(l, r) => Minus(simplify(l), simplify(r))
    case Times(l, r) => Times(simplify(l), simplify(r))
    case Division(l, r) => Division(simplify(l), simplify(r))
    case UMinus(x) => UMinus(simplify(x))
    case x: Variable => x
    case x: RealLiteral => x
  }

}