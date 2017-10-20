// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package opt

import scala.collection.immutable.Seq
import lang.Trees._
import tools.Rational._
import lang.Extractors.ArithOperator

import scala.util.Random

trait RewritingOps {

  val cfg: Config

  // NOTE: this is mutable (so that the value can be set by the user with command-line)
  var rand: Random

  implicit val debugSection: DebugSection

  var countUnmodified = 0

  /* This method is split to enable unit tests.
    @param expr the expression to mutate
    @param index the index of the node which to apply the mutation to
    @param activeRules which mutation rules to consider
   */
  def _mutate(expr: Expr, _index: Int, activeRules: Seq[Expr => Option[Expr]]): Expr = {
    var index = _index
    var mutationSuccess = false
    cfg.reporter.debug(s"\nmutate $expr at index $index")
    // pick a node in expr to mutate, for this the tree is implicitly indexed
    // in a depth-first manner
    def rewrite(e: Expr): Expr = {
      val candidateMutants: Seq[Expr] = activeRules.flatMap(x => x(e))

      if (candidateMutants.length == 0) { // picked a node that cannot be rewritten
        // cfg.reporter.warning("picked node with nothing to mutate")
        // cfg.reporter.warning("Expression with nothing to mutate: " + e)
        countUnmodified = countUnmodified + 1
        e
      } else {
        mutationSuccess = true
        // pick randomly one from the list (this may not be the most efficient method)
        val mutatedNode = candidateMutants(rand.nextInt(candidateMutants.size))
        cfg.reporter.debug("mutatedNode: " + mutatedNode)

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

          } else if (es.size == 2) { // binary operator

            val (lhs, countLhs) = mapNth(es(0), count + 1)
            val (rhs, countRhs) = mapNth(es(1), countLhs)
            (builder(Seq(lhs, rhs)), countRhs)

          } else {
            cfg.reporter.error("found more than binary operator, don't know what to do with it")
            null
          }

        }
    }
    var res = mapNth(expr, 0)._1

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



  val simplifyRules = List(
    idReduce0(_),
    idReduce1(_),
    idReduce2(_),
    idReduce3(_),
    idReduce4(_),
    idReduce5(_),
    idReduce6(_),
    idReduce7(_),
    idReduce8(_),
    idReduce9(_),
    idReduce10(_),
    idReduce11(_))

  val commRules = List(
    commutativity0(_),
    commutativity1(_))

  val assocRules = List(
    associativity0(_),
    associativity1(_),
    associativity2(_),
    associativity3(_),
    associativity4(_),
    associativity5(_),
    associativity6(_),
    associativity7(_),
    associativity8(_),
    associativity9(_),
    associativity10(_),
    associativity11(_),
    associativity12(_),
    associativity13(_),
    associativity14(_),
    associativity15(_))

  val distRules = List(
    distributivity0(_),
    distributivity1(_),
    distributivity2(_),
    distributivity3(_),
    distributivity4(_),
    distributivity5(_),
    distributivity6(_),
    distributivity7(_),
    distributivity8(_),
    distributivity9(_),
    distributivity10(_),
    distributivity11(_),
    distributivity12(_),
    distributivity13(_),
    distributivity14(_),
    distributivity15(_),
    distributivity16(_),
    distributivity17(_),
    distributivity18(_),
    distributivity19(_))

  val diffSqCanonRules = List(
    diffOfSqCanon0(_),
    diffOfSqCanon1(_))

  val diffSqFlipRules = List(
    diffOfSqFlip0(_),
    diffOfSqFlip1(_))

  val idReduceRules = List(
    idReduce0(_),
    idReduce1(_),
    idReduce2(_),
    idReduce3(_),
    idReduce4(_),
    idReduce5(_),
    idReduce6(_),
    idReduce7(_),
    idReduce8(_),
    idReduce9(_),
    idReduce10(_),
    idReduce11(_))

  val fracTransRules = List(
    fracTrans0(_),
    fracTrans1(_),
    fracTrans2(_),
    fracTrans3(_),
    fracTrans4(_))

  val fracDistRules = List(
    fracDist0(_),
    fracDist1(_))

  // =============================================
  // Beginning of mutation rules
  // =============================================

  // ===========
  // COMMUTATIVITY
  // ===========
  def commutativity0 (expr: Expr): Option[Expr] = {
    // +-commutative
    expr match {
      case Plus(a, b) => Some(Plus(b, a))
      case _ => None
    }
  }

  def commutativity1 (expr: Expr): Option[Expr] = {
    // *-commutative
    expr match {
      case Times(a, b) => Some(Times(b, a))
      case _ => None
    }
  }

  // =============
  // ASSOCIATIVITY
  // =============
  def associativity0 (expr: Expr): Option[Expr] = {
    // associate-+r+
    expr match {
      case Plus(a, Plus(b, c)) => Some(Plus(Plus(a, b), c))
      case _ => None
    }
  }

  def associativity1 (expr: Expr): Option[Expr] = {
    // associate-+l+
    expr match {
      case Plus(Plus(a, b), c) => Some(Plus(a, Plus(b, c)))
      case _ => None
    }
  }

  def associativity2 (expr: Expr): Option[Expr] = {
    // associate-+r-
    expr match {
      case Plus(a, Minus(b, c)) => Some(Minus(Plus(a, b), c))
      case _ => None
    }
  }

  def associativity3 (expr: Expr): Option[Expr] = {
    // associate-+l-
    expr match {
      case Plus(Minus(a, b), c) => Some(Minus(a, Minus(b, c)))
      case _ => None
    }
  }

  def associativity4 (expr: Expr): Option[Expr] = {
    // associate--r+
    expr match {
      case Minus(a, Plus(b, c)) => Some(Minus(Minus(a, b), c))
      case _ => None
    }
  }

  def associativity5 (expr: Expr): Option[Expr] = {
    // associate--l+
    expr match {
      case Minus(Plus(a, b), c) => Some(Plus(a, Minus(b, c)))
      case _ => None
    }
  }

  def associativity6 (expr: Expr): Option[Expr] = {
    // associate--l-
    expr match {
      case Minus(Minus(a, b), c) => Some(Minus(a, Plus(b, c)))
      case _ => None
    }
  }

  def associativity7 (expr: Expr): Option[Expr] = {
    // associate--r-
    expr match {
      case Minus(a, Minus(b, c)) => Some(Plus(Minus(a, b), c))
      case _ => None
    }
  }

  def associativity8 (expr: Expr): Option[Expr] = {
    // associate-*r*
    expr match {
      case Times(a, Times(b, c)) => Some(Times(Times(a, b), c))
      case _ => None
    }
  }

  def associativity9 (expr: Expr): Option[Expr] = {
    // associate-*l*
    expr match {
      case Times(Times(a, b), c) => Some(Times(a, Times(b, c)))
      case _ => None
    }
  }

  def associativity10 (expr: Expr): Option[Expr] = {
    // associate-*r/
    expr match {
      case Times(a, Division(b, c)) => Some(Division(Times(a, b), c))
      case _ => None
    }
  }

  def associativity11 (expr: Expr): Option[Expr] = {
    // associate-*l/
    expr match {
      case Times(Division(a, b), c) => Some(Division(Times(a, c), b))
      case _ => None
    }
  }

  def associativity12 (expr: Expr): Option[Expr] = {
    // associate-/r*
    expr match {
      case Division(a, Times(b, c)) => Some(Division(Division(a, b), c))
      case _ => None
    }
  }

  def associativity13 (expr: Expr): Option[Expr] = {
    // associate-/l*
    expr match {
      case Division(Times(b, c), a) => Some(Division(b, Division(a, c)))
      case _ => None
    }
  }

  def associativity14 (expr: Expr): Option[Expr] = {
    // associate-/r/
    expr match {
      case Division(a, Division(b, c)) => Some(Times(Division(a, b), c))
      case _ => None
    }
  }

  def associativity15 (expr: Expr): Option[Expr] = {
    // associate-/l/
    expr match {
      case Division(Division(b, c), a) => Some(Division(b, Times(a, c)))
      case _ => None
    }
  }

  // ==============
  // DISTRIBUTIVITY
  // ==============
  def distributivity0 (expr: Expr): Option[Expr] = {
    // distribute-lft-in
    expr match {
      case Times(a, Plus(b, c)) => Some(Plus(Times(a, b), Times(a, c)))
      case _ => None
    }
  }

  def distributivity1 (expr: Expr): Option[Expr] = {
    // distribute-rgt-in
    expr match {
      case Times(a, Plus(b, c)) => Some(Plus(Times(b, a), Times(c, a)))
      case _ => None
    }
  }

  def distributivity2 (expr: Expr): Option[Expr] = {
    // distribute-lft-out
    expr match {
      case Plus(Times(a, b), Times(a1, c)) if (a==a1) => Some(Times(a, Plus(b, c)))
      case _ => None
    }
  }

  def distributivity3 (expr: Expr): Option[Expr] = {
    // distribute-lft-out--
    expr match {
      case Minus(Times(a, b), Times(a1, c)) if (a==a1) => Some(Times(a, Minus(b, c)))
      case _ => None
    }
  }

  def distributivity4 (expr: Expr): Option[Expr] = {
    // distribute-rgt-out
    expr match {
      case Plus(Times(b, a), Times(c, a1)) if (a==a1) => Some(Times(a, Plus(b, c)))
      case _ => None
    }
  }

  def distributivity5 (expr: Expr): Option[Expr] = {
    // distribute-rgt-out--
    expr match {
      case Minus(Times(b, a), Times(c, a1)) if (a==a1) => Some(Times(a, Minus(b, c)))
      case _ => None
    }
  }

  def distributivity6 (expr: Expr): Option[Expr] = {
    // distribute-lft1-in
    expr match {
      case Plus(Times(b, a), a1) if (a==a1) => Some(Times(Plus(b, RealLiteral(one)), a))
      case _ => None
    }
  }

  def distributivity7 (expr: Expr): Option[Expr] = {
    // distribute-rgt1-in
    expr match {
      case Plus(a1, Times(c, a)) if (a==a1) => Some(Times(Plus(c, RealLiteral(one)), a))
      case _ => None
    }
  }

  def distributivity8 (expr: Expr): Option[Expr] = {
    // distribute-lft-neg-in
    expr match {
      case UMinus(Times(a, b)) => Some(Times(UMinus(a), b))
      case _ => None
    }
  }

  def distributivity9 (expr: Expr): Option[Expr] = {
    // distribute-rgt-neg-in
    expr match {
      case UMinus(Times(a, b)) => Some(Times(a, UMinus(b)))
      case _ => None
    }
  }

  def distributivity10 (expr: Expr): Option[Expr] = {
    // distribute-lft-neg-out
    expr match {
      case Times(UMinus(a), b) => Some(UMinus(Times(a, b)))
      case _ => None
    }
  }

  def distributivity11 (expr: Expr): Option[Expr] = {
    // distribute-rgt-neg-out
    expr match {
      case Times(a, UMinus(b)) => Some(UMinus(Times(a, b)))
      case _ => None
    }
  }

  def distributivity12 (expr: Expr): Option[Expr] = {
    // distribute-neg-in
    expr match {
      case UMinus(Plus(a, b)) => Some(Plus(UMinus(a), UMinus(b)))
      case _ => None
    }
  }

  def distributivity13 (expr: Expr): Option[Expr] = {
    // distribute-neg-out
    expr match {
      case Plus(UMinus(a), UMinus(b)) => Some(UMinus(Plus(a, b)))
      case _ => None
    }
  }

  def distributivity14 (expr: Expr): Option[Expr] = {
    // distribute-inv-in
    expr match {
      case Division(RealLiteral(o), Times(a, b)) if (o==one)=>
        Some(Times(Division(RealLiteral(1), a), Division(RealLiteral(1), b)))
      case _ => None
    }
  }

  def distributivity15 (expr: Expr): Option[Expr] = {
    // distribute-inv-out
    expr match {
      case Times(Division(RealLiteral(o), a), Division(RealLiteral(o2), b)) if (o==one && o2==one) =>
        Some(Division(RealLiteral(1), Times(a, b)))
      case _ => None
    }
  }

  def distributivity16 (expr: Expr): Option[Expr] = {
    // distribute-inv-neg
    expr match {
      case Division(RealLiteral(o), UMinus(a)) if (o==one) => Some(UMinus(Division(RealLiteral(1), a)))
      case _ => None
    }
  }

  def distributivity17 (expr: Expr): Option[Expr] = {
    // distribute-neg-inv
    expr match {
      case UMinus(Division(RealLiteral(o), a)) if (o==one) => Some(Division(RealLiteral(1), UMinus(a)))
      case _ => None
    }
  }

  def distributivity18 (expr: Expr): Option[Expr] = {
    // distribute-frac-neg
    expr match {
      case Division(UMinus(a), b) => Some(UMinus(Division(a, b)))
      case _ => None
    }
  }

  def distributivity19 (expr: Expr): Option[Expr] = {
    // distribute-neg-frac
    expr match {
      case UMinus(Division(a, b)) => Some(Division(UMinus(a), b))
      case _ => None
    }
  }

  // ==================================
  // DIFFERENCE OF SQUARES CANONICALIZE
  // ==================================
  def diffOfSqCanon0 (expr: Expr): Option[Expr] = {
    // difference-of-squares
    expr match {
      case Minus(Times(a1, a), Times(b1, b)) if (b==b1 && a==a1) => Some(Times(Plus(a, b), Minus(a, b)))
      case _ => None
    }
  }

  def diffOfSqCanon1 (expr: Expr): Option[Expr] = {
    // difference-of-sqr-1
    expr match {
      case Minus(Times(a1, a), RealLiteral(o)) if (a==a1 && o==one) =>
        Some(Times(Plus(a, RealLiteral(one)), Minus(a, RealLiteral(one))))
      case _ => None
    }
  }

  // ==========================
  // DIFFERENCE OF SQUARES FLIP
  // ==========================
  def diffOfSqFlip0 (expr: Expr): Option[Expr] = {
    // flip-+
    expr match {
      case Plus(a, b) => Some(Division(Minus(Times(a, a), Times(b, b)),
          Minus(a, b)))
      case _ => None
    }
  }

  def diffOfSqFlip1 (expr: Expr): Option[Expr] = {
    // flip--
    expr match {
      case Minus(a, b) => Some(Division(Minus(Times(a, a), Times(b, b)),
          Plus(a, b)))
      case _ => None
    }
  }

  // ===============
  // IDENTITY REDUCE
  // ===============
  def idReduce0 (expr: Expr): Option[Expr] = {
    // +-lft-identity
    expr match {
      case Plus(RealLiteral(z), a) if (z==zero) => Some(a)
      case _ => None
    }
  }

  def idReduce1 (expr: Expr): Option[Expr] = {
    // +-rgt-identity
    expr match {
      case Plus(a, RealLiteral(z)) if (z==zero) => Some(a)
      case _ => None
    }
  }

  def idReduce2 (expr: Expr): Option[Expr] = {
    // +-inverses
    expr match {
      case Minus(a1, a) if (a==a1) => Some(RealLiteral(zero))
      case _ => None
    }
  }

  def idReduce3 (expr: Expr): Option[Expr] = {
    // sub0-neg
    expr match {
      case Minus(RealLiteral(z), b) if (z==zero) => Some(UMinus(b))
      case _ => None
    }
  }

  def idReduce4 (expr: Expr): Option[Expr] = {
    // remove-double-neg
    expr match {
      case UMinus(UMinus(a)) => Some(a)
      case _ => None
    }
  }

  def idReduce5 (expr: Expr): Option[Expr] = {
    // *-lft-identity
    expr match {
      case Times(RealLiteral(o), a) if (o==one) => Some(a)
      case _ => None
    }
  }

  def idReduce6 (expr: Expr): Option[Expr] = {
    // *-rgt-identity
    expr match {
      case Times(a, RealLiteral(o)) if (o==one) => Some(a)
      case _ => None
    }
  }

  def idReduce7 (expr: Expr): Option[Expr] = {
    // *-inverses
    expr match {
      case Division(a1, a) if (a==a1) => Some(RealLiteral(one))
      case _ => None
    }
  }

  def idReduce8 (expr: Expr): Option[Expr] = {
    // remove-double-div
    expr match {
      case Division(RealLiteral(o), Division(RealLiteral(o2), a)) if (o==one && o2==one) => Some(a)
      case _ => None
    }
  }

  def idReduce9 (expr: Expr): Option[Expr] = {
    // div0
    expr match {
      case Division(RealLiteral(z), a) if (z==zero) => Some(RealLiteral(zero))
      case _ => None
    }
  }

  def idReduce10 (expr: Expr): Option[Expr] = {
    // mul0
    expr match {
      case Times(RealLiteral(z), a) if (z==zero) => Some(RealLiteral(zero))
      case _ => None
    }
  }

  def idReduce11 (expr: Expr): Option[Expr] = {
    // mul-1-neg
    expr match {
      case Times(UMinus(RealLiteral(o)), a) if (o==one) => Some(UMinus(a))
      case _ => None
    }
  }

  // ========================
  // FRACTIONS TRANSFORMATION
  // ========================
  def fracTrans0 (expr: Expr): Option[Expr] = {
    // sub-div
    expr match {
      case Minus(Division(a, c), Division(b, c1)) if (c==c1) => Some(Division(Minus(a, b), c))
      case _ => None
    }
  }

  def fracTrans1 (expr: Expr): Option[Expr] = {
    // frac-add
    expr match {
      case Plus(Division(a, b), Division(c, d)) => Some(Division(Plus(Times(a, d), Times(b, c)),
          Times(b, d)))
      case _ => None
    }
  }

  def fracTrans2 (expr: Expr): Option[Expr] = {
    // frac-sub
    expr match {
      case Minus(Division(a, b), Division(c, d)) => Some(Division(Minus(Times(a, d), Times(b, c)),
          Times(b, d)))
      case _ => None
    }
  }

  def fracTrans3 (expr: Expr): Option[Expr] = {
    // frac-times
    expr match {
      case Times(Division(a, b), Division(c, d)) => Some(Division(Times(a, c), Times(b, d)))
      case _ => None
    }
  }

  def fracTrans4 (expr: Expr): Option[Expr] = {
    // frac-2neg
    expr match {
      case Division(a, b) => Some(Division(UMinus(a), UMinus(b)))
      case _ => None
    }
  }

  // ====================
  // FRACTIONS DISTRIBUTE
  // ====================
  def fracDist0 (expr: Expr): Option[Expr] = {
    // div-sub
    expr match {
      case Division(Minus(a, b), c) => Some(Minus(Division(a, c), Division(b, c)))
      case _ => None
    }
  }

  def fracDist1 (expr: Expr): Option[Expr] = {
    // times-frac
    expr match {
      case Division(Times(a, b), Times(c, d)) => Some(Times(Division(a, c), Division(b, d)))
      case _ => None
    }
  }

}
