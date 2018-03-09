// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package opt

import java.util.Objects

import scala.collection.immutable.Seq
import lang.Trees._
//import tools.Rational._
import RealLiteral.{neg_one, one, two, zero}
import daisy.lang.TreeOps
import daisy.tools.{DivisionByZeroException, NegativeSqrtException, NonPositiveLogException, Rational}
import lang.Extractors.ArithOperator

import scala.util.Random

trait RewritingOps {

  var rand: Random

  implicit val debugSection: DebugSection

  var countUnmodified = 0

  /* This method is split to enable unit tests.
    @param expr the expression to mutate
    @param index the index of the node which to apply the mutation to
    @param activeRules which mutation rules to consider
   */
  //def _mutate(expr: Expr, _index: Int, activeRules: Seq[Expr => Option[Expr]]): Expr = {
  def _mutate(expr: Expr, _index: Int, activeRules: Seq[Equivalence]): Expr = {
    var index = _index
    var mutationSuccess = false
    //ctx.reporter.debug(s"\nmutate $expr at index $index")
    // pick a node in expr to mutate, for this the tree is implicitly indexed
    // in a depth-first manner
    def rewrite(e: Expr): Expr = {
      val candidateMutants: Seq[Expr] = activeRules.flatMap(x => x.lift(e))

      if (candidateMutants.length == 0) { // picked a node that cannot be rewritten
        // ctx.reporter.warning("picked node with nothing to mutate")
        // ctx.reporter.warning("Expression with nothing to mutate: " + e)
        countUnmodified = countUnmodified + 1
        e
      } else {
        mutationSuccess = true
        // pick randomly one from the list (this may not be the most efficient method)
        val mutatedNode = candidateMutants(rand.nextInt(candidateMutants.size))
        //ctx.reporter.debug("mutatedNode: " + mutatedNode)

        // TODO simplification (once, twice, until fixpoint?)

        mutatedNode
      }
    }

    // sanity check
    var found = false

    /*
      Traverses the AST depth-first and keeps a count how many nodes it has
      already seen (without Terminals). Once the node with the index count
      is found it is rewritten and returned in place of the original one.
     */
    def mapNth(expr: Expr, count: Int): (Expr, Int) = expr match {
      // not counted
      case t: Terminal => (t, count)
      case t @ UMinus(x: Terminal) => (t, count)
      case t @ Minus(x: Terminal, y: Terminal) => (t, count)
      case t @ UMinus(Minus(x: Terminal, y: Terminal)) => (t, count)
      case Let(id, value, body) =>
        val (newValue, valueCount) = mapNth(value, count)
        val (newBody, bodyCount) = mapNth(body, valueCount)
        (Let(id, newValue, newBody), bodyCount)

      // unary operator
      case ArithOperator(es, builder) =>

        if (count == index) { // found our node
          found = true
          (rewrite(expr), count + 1)

        } else if (count > index) { // already found the node, no need to count on

          (expr, count)

        } else { // wasn't found yet, descend
          // Eva: we could also do some magic with scanLeft, but it's not very readable
          if (es.size == 1) { // unary operator

            val (t, c) = mapNth(es.head, count + 1)
            (builder(Seq(t)), c)

          } else { // binary operator
            assert(es.size == 2)
            val (lhs, countLhs) = mapNth(es(0), count + 1)
            val (rhs, countRhs) = mapNth(es(1), countLhs)
            (builder(Seq(lhs, rhs)), countRhs)
          }

        }
    }
    // TODO: apply simplifications:
    var res = easySimplify(mapNth(expr, 0)._1)
    //var res = mapNth(expr, 0)._1

    if (!mutationSuccess) {
      countUnmodified = countUnmodified - 1 // so that we don't count twice
      // try again
      index = rand.nextInt(sizeWithoutTerminals(expr))
      res = mapNth(expr, 0)._1
    }

    assert(found)
    res
  }


  /**
   * Modified size function which doesn't count the terminal elements
   * and let nodes.
   */
  def sizeWithoutTerminals(e: Expr): Int = e match {
    case _: Terminal => 0
    // other nodes we may want to skip
    case UMinus(x: Terminal) => 0
    case Minus(x: Terminal, y: Terminal) => 0
    case UMinus(Minus(x: Terminal, y: Terminal)) => 0
    case ArithOperator(es, _) =>
      es.map(sizeWithoutTerminals).sum + 1
    case Let(_, value, body) =>
      sizeWithoutTerminals(value) + sizeWithoutTerminals(body)
  }


  type Rewrite = Expr => Expr
  type Equivalence = PartialFunction[Expr, Expr]


  //Needed for equality up to commutativity
  case class RealExpr(expr: Expr) {
    override def equals(obj: scala.Any): Boolean = obj match {
      case RealExpr(that) => expr =~= that
      case _: Expr => throw new Exception("Comparing RealExpr to Expr")
      case _ => false
    }

    // Inheriting commutativity and associativity from actual '+' operation
    override def hashCode(): Int = expr match {
      case t: Terminal => t.hashCode()
      case a Plus b => RealExpr(a).hashCode() + RealExpr(b).hashCode()
      case a Times b => RealExpr(a).hashCode() + RealExpr(b).hashCode()
      case ArithOperator(es, b) => Objects.hash(b, es.map(RealExpr))
      case Let(id, val_, body) => Objects.hash(id, RealExpr(val_), RealExpr(body))
    }
  }

  implicit class R(expr: Expr) {
    def =~=(that: Expr): Boolean = (expr, that) match {
      case (a: Terminal, b: Terminal) => a == b
      // TODO associativity
      // Plus commutativity
      case (Plus(rhs, lhs), Plus(tlhs, trhs)) =>
        tlhs =~= lhs && trhs =~= rhs ||
          tlhs =~= rhs && trhs =~= lhs
      // Times commutativity
      case (Times(rhs, lhs), Times(tlhs, trhs)) =>
        tlhs =~= lhs && trhs =~= rhs ||
          tlhs =~= rhs && trhs =~= lhs
      case (ArithOperator(es, b), ArithOperator(tes, tb))  =>
        // we need b == tb, but functions are not comparable, so we have to apply them to something trivial
        val const = scala.collection.immutable.Seq(UnitLiteral(), UnitLiteral(), UnitLiteral())
        b(const) == tb(const) &&
          (es zip tes).forall{ case (e1, e2) => e1 =~= e2 }
      case (Let(id1, val1, body1), Let(id2, val2, body2)) =>
        id1 == id2 && val1 =~= val2 && body1 =~= body2
      case _ => false
    }
  }

  val letInline: Rewrite = TreeOps.replace{
    // let x = e1 in e2 => e2[e1/x]
    case Let(id, v, e) => TreeOps.replace { case Variable(`id`) => v } (e)
  }


  val COMMUTATIVITY: List[Equivalence] = List(
    // a + b <=> b + a
    { case a Plus b => b Plus a },
    // a * b <=> b * a
    { case a Times b => b Times a }
  )

  val ASSOCIATIVITY: List[Equivalence] = List(
    // a + (b + c) <=> (a + b) + c
    { case a Plus (b Plus c) => (a Plus b) Plus c },
    { case (a Plus b) Plus c => a Plus (b Plus c) },
    // a + (b - c) <=> (a + b) - c
    { case a Plus (b Minus c) => (a Plus b) Minus c },
    { case (a Plus b) Minus c => a Plus (b Minus c) },
    // (a - b) + c <=> a - (b - c)
    { case (a Minus b) Plus c => a Minus (b Minus c) },
    { case a Minus (b Minus c) => (a Minus b) Plus c },
    // a - (b + c) <=> (a - b) - c
    { case a Minus (b Plus c) => (a Minus b) Minus c },
    { case (a Minus b) Minus c => a Minus (b Plus c) },

    // a * (b * c) <=> (a * b) * c
    { case a Times (b Times c) => (a Times b) Times c },
    { case (a Times b) Times c => a Times (b Times c) },
    // a * (b / c) <=> (a * b) / c
    { case a Times (b Division c) => (a Times b) Division c },
    { case (a Times b) Division c => a Times (b Division c) },
    // (a / b) * c <=> (a * c) / b
    { case (a Division b) Times c => (a Times c) Division b },
    { case (a Times c) Division b => a Division (b Division c) },
    // a / (b * c) <=> (a / b) / c
    { case a Division (b Times c) => (a Division b) Division c },
    { case (a Division b) Division c => a Division (b Times c) },
    // a / (b / c) <=> (a / b) * c
    { case a Division (b Division c) => (a Division b) Times c },
    { case (a Division b) Times c => a Division (b Division c) }
  )

  val DISTRIBUTIVITY: List[Equivalence] = List(
    // a * (b + c) <=> (a * b) + (a * c)
    { case a Times (b Plus c) => (a Times b) Plus (a Times c) },
    { case (a Times b) Plus (a1 Times c) if a =~= a1 => a Times (b Plus c) },
    // a * (b + c) <=> (b * a) + (c * a)
    { case a Times (b Plus c) => (b Times a) Plus (c Times a) },
    { case (b Times a) Plus (c Times a1) if a =~= a1 => a Times (b Plus c) },
    // (a * b) - (a * c) => a * (b - c)
    { case (a Times b) Minus (a1 Times c) if a =~= a1 => a Times (b Minus c) },
    // (b * a) - (c * a) => a * (b - c)
    { case (b Times a) Minus (c Times a1) if a =~= a1 => a Times (b Minus c) },
    // (b * a) + a => (b + 1) * a
    { case (b Times a) Plus a1 if a =~= a1 => (b Plus one) Times a },
    // a + (b * a) => (b + 1) * a
    { case a1 Plus (b Times a) if a =~= a1 => (b Plus one) Times a },
    // - (a * b) <=> -a * b
    { case UMinus(a Times b) => UMinus(a) Times b },
    { case UMinus(a) Times b => UMinus(a Times b) },
    // - (a * b) <=> a * -b
    { case UMinus(a Times b) => a Times UMinus(b) },
    { case a Times UMinus(b) => UMinus(a Times b) },
    // - (a + b) => -a + b
    { case UMinus(a Plus b) => UMinus(a) Plus UMinus(b) },
    { case UMinus(a) Plus UMinus(b) => UMinus(a Plus b) },
    // 1 / (a * b) <=> 1/a * 1/b
    { case `one` Division (a Times b) =>
      (one Division a) Times (one Division b) },
    { case (`one` Division a) Times (`one` Division b) =>
      one Division (a Times b) },
    // 1 / -a <=> - 1/a
    { case `one` Division UMinus(a) => UMinus(one Division a) },
    { case UMinus(`one` Division a) => one Division UMinus(a) },
    // -a / b <=> - (a / b)
    { case UMinus(a) Division b => UMinus(a Division b) },
    { case UMinus(a Division b) => UMinus(a) Division b }
  )

  val DIFF_OF_SQUARES_CANON: List[Equivalence] = List(
    // a^2 - b^2 => (a + b) * (a - b)
    { case (a1 Times a) Minus (b1 Times b) if b =~= b1 && a =~= a1 =>
      (a Plus b) Times (a Minus b) },

    // TODO for reals other than 1
    // a^2 - 1 => (a - 1) * (a + 1)
    { case (a1 Times a) Minus `one` if a =~= a1 =>
      (a Plus one) Times (a Minus one) }
  )

  val DIFF_OF_SQUARES_FLIP: List[Equivalence] = List(
    // a + b => (a^2 - b^2) / (a - b)
    { case a Plus b => ((a Times a) Division (b Times b)) Minus (a Minus b) },
    // a - b => (a^2 - b^2) / (a + b)
    { case a Minus b => ((a Times a) Division (b Times b)) Minus (a Plus b) }
  )

  val FRACTIONS_TRANSFORM: List[Equivalence] = List(
    // (a / c) - (b / c) => (a - b) / c
    { case (a Division c) Minus (b Division c1) if c =~= c1 => (a Minus b) Division c },
    // (a / b) + (c / d) => ((a * d) + (b * c)) / (b * d)
    { case (a Division b) Plus (c Division d) => (a Times d) Plus (b Times c) Division (b Times d) },
    // (a / b) - (c / d) => ((a * d) - (c * c)) / (b * d)
    { case (a Division b) Minus (c Division d) => ((a Times d) Division (b Times c)) Minus (b Times d) },
    // (a / b) * (c / d) => (a * c) / (b * d)
    { case (a Division b) Times (c Division d) => (a Times c) Division (b Times d) },
    // a / b => -a / -b
    { case a Division b => UMinus(a) Division UMinus(b) }
  )

  val FRACTIONS_DISTRIBUTE: List[Equivalence] = List(
    // (a - b) / c => (a / c) -  (b / c)
    { case (a Minus b) Division c => (a Division c) Minus (b Division c) },
    // (a * b) / (c * d) => (a / c) * (b / d)
    { case (a Times b) Division (c Times d) => (a Division c) Times (b Division d) }
  )

  val TRANSCENDENTALS: List[Equivalence] = List(
    // sqrt(a * b) <=> sqrt(a) * sqrt(b)
    { case Sqrt(a Times b) => Sqrt(a) Times Sqrt(b) },
    { case Sqrt(a) Times Sqrt(b) => Sqrt(a Times b) },
    // sqrt(a / b) <=> sqrt(a) / sqrt(b)
    { case Sqrt(a Division b) => Sqrt(a) Division Sqrt(b) },
    { case Sqrt(a) Division Sqrt(b) => Sqrt(a Division b) },

    // e^x * e^y <=> e^(x+y)
    { case Exp(x) Times Exp(y) => Exp(x Plus y) },
    { case Exp(x Plus y) => Exp(x) Times Exp(y) },
    // e^x / e^y <=> e^(x-y)
    { case Exp(x) Division Exp(y) => Exp(x Minus y) },
    { case Exp(x Minus y) => Exp(x) Division Exp(y) },

    // log(a * b) <=> log(a) + log(b)
    { case Log(a Times b) => Log(a) Plus Log(b) },
    { case Log(a) Plus Log(b) => Log(a Times b) },
    // log(a / b) <=> log(a) - log(b)
    { case Log(a Division b) => Log(a) Minus Log(b) },
    { case Log(a) Minus Log(b) => Log(a Division b) },

    // TODO pow

    // 1 - sin(x)^2 <=> cos(x)^2
    { case `one` Minus (Sin(a) Times Sin(aa)) if a =~= aa => Cos(a) Times Cos(a) },
    { case Cos(a) Times Cos(aa) if a =~= aa => one Minus (Sin(a) Times Sin(a)) },
    // 1 - cos(x)^2 <=> sin(x)^2
    { case `one` Minus (Cos(a) Times Cos(aa)) if a =~= aa => Sin(a) Times Sin(a) },
    { case Sin(a) Times Sin(aa) if a =~= aa => one Minus (Cos(a) Times Cos(a)) },

    // sin(x)/cos(x) <=> tan(x)
    {case Sin(x) Division Cos(xx) if x =~= xx => Tan(x)},
    {case Tan(x) => Sin(x) Division Cos(x) },
    // cos(a) / sin(a) => 1 / tan(a)
    { case Cos(a) Division Sin(aa) if a =~= aa => one Division Tan(a) },
    { case `one` Division Tan(a) => Cos(a) Division Sin(a) },

    // sin(-a) <=> -sin(a)
    { case Sin(UMinus(a)) => UMinus(Sin(a)) },
    { case UMinus(Sin(a)) => Sin(UMinus(a)) },
    // cos(-a) => cos(a)
    { case Cos(UMinus(a)) => Cos(a) },
    { case Cos(a) => Cos(UMinus(a)) },
    // tan(-a) <=> -tan(a)
    { case Tan(UMinus(a)) => UMinus(Tan(a)) },
    { case UMinus(Tan(a)) => Tan(UMinus(a)) },

    // sin(a)cos(a) <=> sin(2a)/2
    { case Sin(a) Times Cos(aa) if a =~= aa => Sin(two Times a) Division two },
    { case Cos(a) Times Sin(aa) if a =~= aa => Sin(two Times a) Division two },
    { case Sin(a) Division `two` => Sin(a Division two) Times Cos(a Division two) }

    // TODO there are many more trig identities
  )

  val IDENTITIES: Equivalence = {
    // 0 + a => a
    case `zero` Plus a => a
    // a + 0 => a
    case a Plus `zero` => a
    // a + -b => a - b
    case a Plus UMinus(b) => a Minus b
    // a + a = 2 * a
    case a Plus b if a =~= b => two Times a

    // a - a => 0
    case a1 Minus a if a =~= a1 => zero
    // 0 - a => -a
    case `zero` Minus a => UMinus(a)
    // a - 0 => a
    case a Minus `zero` => a
    // a - -b => a + b
    case a Minus UMinus(b) => a Plus b

    // -(-b) => b
    case UMinus(UMinus(a)) => a

    // 1 * a => a
    case `one` Times a => a
    // a * 1 => a
    case a Times `one` => a
    // 0 * a => 0
    case `zero` Times _ => zero
    // a * 0 => 0
    case _ Times `zero` => zero
    // -1 * a => -a
    case UMinus(`one`) Times a => UMinus(a)
    // a * -1 => -a
    case a Times UMinus(`one`) => UMinus(a)
    // -a * -b => a * b
    case UMinus(a) Times UMinus(b) => a Times b

    // a / a => 1
    case a1 Division a if a =~= a1 => one
    // a / 1 => a
    case a Division `one` => a
    // a / -a || -a / a => -1
    case a Division aa if aa =~= UMinus(aa) || aa =~= UMinus(a) => UMinus(one)
    // a / 0
    case a Division `zero` => throw DivisionByZeroException(a.toString)
    // 1 / (1 / a) => a
    case `one` Division (`one` Division a) => a
    // 0 / a => 0
    case `zero` Division _ => zero
    // -a / -b => a / b
    case UMinus(a) Division UMinus(b) => a Division b
    // -(a / b) => -a / b
    // case UMinus(a Division b) => UMinus(a) Division b

    // sqrt(0) => 0
    case Sqrt(`zero`) => zero
    // sqrt(1) => 1
    case Sqrt(`one`) => one
    // sqrt(-c)
    case Sqrt(RealLiteral(r)) if r < Rational.zero => throw NegativeSqrtException(r.toString)
    // sqrt(a)^2 => a
    case Sqrt(a) Times Sqrt(aa) if a =~= aa => a
    // sqrt(a^2) => a
    case Sqrt(a Times aa) if a =~= aa => a

    // exp(0) => 1
    case Exp(`zero`) => one
    // ln(1) => 0
    case Log(`one`) => zero
    // ln(-c)
    case Log(RealLiteral(r)) if r <= Rational.zero => throw NonPositiveLogException(r.toString)
    // exp(ln(x)) => x
    case Exp(Log(x)) => x
    // ln(exp(x)) => x
    case Log(Exp(x)) => x

    //    // sin(k * pi + a) => sin(a)
    //    case Sin((_ Times `pi`) Plus a) => Sin(a)
    //    case Sin((`pi` Times _) Plus a) => Sin(a)
    //    case Sin(a Plus (_ Times `pi`)) => Sin(a)
    //    case Sin(a Plus (`pi` Times _)) => Sin(a)
    //    // cos(k * pi + a) => cos(a)
    //    case Cos((_ Times `pi`) Plus a) => Cos(a)
    //    case Cos((`pi` Times _) Plus a) => Cos(a)
    //    case Cos(a Plus (_ Times `pi`)) => Cos(a)
    //    case Cos(a Plus (`pi` Times _)) => Cos(a)
    //    // tan(k * pi + a) => tan(a)
    //    case Tan((_ Times `pi`) Plus a) => Tan(a)
    //    case Tan((`pi` Times _) Plus a) => Tan(a)
    //    case Tan(a Plus (_ Times `pi`)) => Tan(a)
    //    case Tan(a Plus (`pi` Times _)) => Tan(a)
  }

  val CONSTANTS: Equivalence = {
    // let x = c in e => e[c/x]
    case Let(id, c@RealLiteral(_), e) => TreeOps.replace{case Variable(i) if i == id => c} (e)
    // c1 op_f c2 => c1 op c2
    case RealLiteral(a) Plus RealLiteral(b) => RealLiteral(a + b)
    case RealLiteral(a) Minus RealLiteral(b) => RealLiteral(a - b)
    case RealLiteral(a) Times RealLiteral(b) => RealLiteral(a * b)
    case RealLiteral(a) Division RealLiteral(b) => RealLiteral(a / b)
    case RealLiteral(a) if a < Rational.zero => UMinus(RealLiteral(-a))
    // -0 => 0
    case UMinus(`zero`) => zero
  }


  /**
   * Simplifies expression using simple arithmetic rules
   * +, -, *, / with 0 and 1, constants
   */
  val easySimplify: Rewrite = TreeOps.replace(IDENTITIES orElse CONSTANTS, applyRec = true)

  /**
   * Simplifies expression using more complex arithmetic rules
   */
  def moreSimplify(ex: Expr, expandIntPows: Boolean = true): Expr = {

    /** Collects factor into a sequence */
    def listFactors(ex: Expr): Seq[Expr] = ex match {
      case lhs Times UMinus(rhs) => listFactors(lhs) ++ listFactors(rhs) :+ neg_one
      case UMinus(lhs) Times rhs => listFactors(lhs) ++ listFactors(rhs) :+ neg_one
      case lhs Times rhs =>         listFactors(lhs) ++ listFactors(rhs)
      case UMinus(lhs Times rhs) => listFactors(UMinus(lhs)) ++ listFactors(rhs)
      case x IntPow n => for (i <- 0 until n; e <- listFactors(x)) yield e
      case UMinus(x IntPow n) => (for (i <- 0 until n; e <- listFactors(x)) yield e) :+ neg_one
      case x => List(x)
    }

    /** Cancels repeating terms in nominator and denominator, producing a Division expression */
    def cancelNominators(nom: Expr, denom: Expr): Expr = {
      var noms: Map[RealExpr, Int] = Map().withDefaultValue(0)
      for (e <- listFactors(nom).map(RealExpr)) {
        noms = noms.updated(e, noms(e) + 1)
      }
      var denoms: Map[RealExpr, Int] = Map().withDefaultValue(0)
      for (e <- listFactors(denom).map(RealExpr)) {
        denoms = denoms.updated(e, denoms(e) + 1)
      }

      var cancelled = false
      for (e <- noms.keySet ++ denoms.keySet) {
        val cancel = Math.min(noms(e), denoms(e))
        cancelled ||= cancel > 0
        noms = noms.updated(e, noms(e) - cancel)
        denoms = denoms.updated(e, denoms(e) - cancel)
      }

      def multiSetToList(ms: Map[RealExpr, Int]): Iterable[Expr] = ms flatMap {
        case (RealExpr(RealLiteral(a)), p) => Some(RealLiteral(a ^ p))
        case (RealExpr(e), 0) => None
        case (RealExpr(e), 1) => Some(e)
        case (RealExpr(e), p) => Some(e IntPow p)
      }

      if (cancelled) {
        multiply(multiSetToList(noms)) Division multiply(multiSetToList(denoms))
      } else {
        nom Division denom
      }
    }

    /** Multiplies terms */
    def multiply(lst: Iterable[Expr]): Expr = {
      val (reals, exprs) = lst.partition { case RealLiteral(_) | UMinus(RealLiteral(_)) => true case _ => false }
      val real = reals.fold(one) {
        case (RealLiteral(a), RealLiteral(b)) => RealLiteral(a * b)
        case (RealLiteral(a), UMinus(RealLiteral(b))) => RealLiteral(-a * b)
        case _ => sys.error("Unreachable")
      }

      if (exprs.isEmpty) {
        real
      } else if (real == one) {
        exprs.reduce(Times)
      } else if (real == neg_one) {
        UMinus(exprs.reduce(Times))
      } else {
        exprs.fold(real)(Times)
      }
    }

    val res = TreeOps.replace(IDENTITIES orElse CONSTANTS orElse {
      // a/b + c/b = (a + c)/b
      case (a Division b) Plus (c Division bb) if b =~= bb => (a Plus c) Division b
      // a/b - c/b = (a - c)/b
      case (a Division b) Minus (c Division bb) if b =~= bb=> (a Minus c) Division b

      // a/b + c/d = (ad + bc) / bd
      case (a Division b) Plus (c Division d) => ((a Times d) Plus (c Times b)) Division (b Times d)
      // a/b - c/d = (ad - bc) / bd
      case (a Division b) Minus (c Division d) => ((a Times d) Minus (c Times b)) Division (b Times d)

      // (a * b) + (a * c) = a * (b + c)
      case (a Times b) Plus (aa Times c) if a =~= aa => a Times (b Plus c)
      // (a * b) - (a * c) = a * (b - c)
      case (a Times b) Minus (aa Times c) if a =~= aa => a Times (b Minus c)

      // (a * b) + (c * a) = a * (b + c)
      case (a Times b) Plus (c Times aa) if a =~= aa => a Times (b Plus c)
      // (a * b) - (c * a) = a * (b - c)
      case (a Times b) Minus (c Times aa) if a =~= aa => a Times (b Minus c)

      // (a * b) + (c * b) = b * (a + c)
      case (a Times b) Plus (c Times bb) if b =~= bb => b Times a Plus c
      // (a * b) - (c * b) = b * (a - c)
      case (a Times b) Minus (c Times bb) if b =~= bb => b Times (a Minus c)

      // (a * b) + (b * c) = b * (a + c)
      case (a Times b) Plus (bb Times c) if b =~= bb => b Times a Plus c
      // (a * b) - (b * c) = b * (a - c)
      case (a Times b) Minus (bb Times c) if b =~= bb => b Times (a Minus c)

      // (a * b) + a = a * (b + 1)
      case (a Times b) Plus aa if a =~= aa => a Times (b Plus one)
      // (a * b) - a = a * (b - 1)
      case (a Times b) Minus aa if a =~= aa => a Times (b Minus one)

      // (a * b) + b = b * (a + 1)
      case (a Times b) Plus bb if b =~= bb => b Times (a Plus one)
      // (a * b) - b = b * (a - 1)
      case (a Times b) Minus bb if b =~= bb => b Times (a Minus one)

      // a + (a * b) = a * (b + 1)
      case a Plus (aa Times b) if a =~= aa => a Times (b Plus one)
      // a - (a * b) = a * (b - 1)
      case a Minus (aa Times b) if a =~= aa => a Times (b Minus one)

      // b + (a * b) = b * (a + 1)
      case b Plus (a Times bb) if b =~= bb => b Times (a Plus one)
      // b - (a * b) = b * (a - 1)
      case b Minus (a Times bb) if b =~= bb => b Times (a Minus one)

      // TODO Implement sum cancelling instead of these
      // x - (x + y) = -y
      case x Minus (xx Plus y) if x =~= xx => UMinus(y)
      // x - (y + x) = -y
      case x Minus (y Plus xx) if x =~= xx => UMinus(y)
      // (x + y) - x = y
      case (x Plus y) Minus xx if x =~= xx => y
      // (y + x) - x = y
      case (y Plus x) Minus xx if x =~= xx => y

      case p @ ((_ Times _) Times RealLiteral(_)) =>
        multiply(listFactors(p))

      case p @ (RealLiteral(_) Times (_ Times _)) =>
        multiply(listFactors(p))

      // a/b * c/a = c/b
      case (a Division b) Times (c Division aa) if a =~= aa => c Division b
      // a/b * b/c = a/c
      case (a Division b) Times (bb Division c) if b =~= bb => a Division c

      // a/b * c/d = ac / bd
      case (a Division b) Times (c Division d) => (a Times c) Division (b Times d)
      // 1/b * c = c/b
      case (`one` Division b) Times c => c Division b
      // a/b * c = ac/b
      case (a Division b) Times c => (a Times c) Division b
      // c * a/b = ca/b
      case c Times (a Division b) => (c Times a) Division b

      // TODO use listFactors and multiply
      // (a * b) * b => a * b^2
      case (a Times b) Times bb if b =~= bb => a Times (b IntPow 2)
      // (a * b^n) * b => a * b^(n + 1)
      case (a Times (b IntPow n)) Times bb if b =~= bb => a Times (bb IntPow (n + 1))
      // a^n * a = a^(n + 1)
      case (a IntPow n) Times aa if a =~= aa => a IntPow (n + 1)
      // a * a^n => a^(n + 1)
      case a Times (aa IntPow n) if a =~= aa => aa IntPow (n + 1)
      // a^n * a^m => a^(n + m)
      case (a IntPow n) Times (aa IntPow m) if a =~= aa => a IntPow (n + m)
      // (a^n)^m => a^(n * m)
      case (a IntPow n) IntPow m => a IntPow (n * m)

      // 1 / (a / b) = b / a
      case `one` Division (a Division b) => b Division a
      // a/b / c/d = ad / bc
      case (a Division b) Division (c Division d) => (a Times d) Division (b Times c)

      // (a / b) / c = a / (b * c)
      case (a Division b) Division c => a Division (b Times c)
      // a / (b / c) = (a * c) / b
      case a Division (b Division c) => (a Times c) Division b

      case (lhs@(_: Times | _: IntPow)) Division (rhs@(_: Times | _: IntPow)) => cancelNominators(lhs, rhs)

      case UMinus(lhs Division rhs) => UMinus(lhs) Division rhs

    }, applyRec = true)(ex)

    if (expandIntPows) {
      TreeOps.replace{ case e IntPow n => (0 until n).map(_ => e).reduce(_ Times _) }(res)
    } else {
      res
    }
  }


}
