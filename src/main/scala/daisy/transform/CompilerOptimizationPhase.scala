// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.transform

import daisy.lang.Types
import daisy.lang.Identifiers
import daisy.lang.Trees
import daisy.lang.Trees._
import daisy.lang.TreeOps
import daisy._
import daisy.opt.NewRewritingOps
import daisy.tools.FinitePrecision.{Float32, Float64, Precision}
import RealLiteral.{neg_one, zero, one}
import scala.collection.immutable.Seq
import scala.collection.immutable.ListMap

object CompilerOptimizationPhase extends DaisyPhase with NewRewritingOps {
  override val name: String = "Compiler Optimization"
  override val shortName: String = "compopt"
  override val description: String = "Applies compiler optimizations"
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    MultiStringChoiceOption(
      "comp-opts",
      Seq("fma", "inline", "simplify", "cse" ,"divToInv", "horner"),
      "Which compiler optimizations to apply before analysis"
    )
  )
  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>
      val oldFnc = fnc
      val newFnc = fnc.copy(body = Some(
        ctx.option[List[String]]("comp-opts").foldLeft(fnc.body.get)({case (currExpr, option) =>
          option match {
            case "inline" => letInline(currExpr)
            case "simplify" => moreSimplify(currExpr)
            case "fma" =>
              if (ctx.option[Precision]("precision") == Float64 || ctx.option[Precision]("precision") == Float32) {
                introduceFMAs(currExpr)
              } else {
                currExpr
              }
            case "cse" => commonSubexpressionElimination(currExpr)
            case "divToInv" => divisionToReciprocal(currExpr)
            case "horner" =>  hornerScheme(currExpr)
          }
        })))
      newFnc
    }

    (ctx, Program(prg.id, newDefs))
  }

  def introduceFMAs(e: Expr): Expr = {
      TreeOps.replace {
        case Plus(Times(a, b), c) => FMA(a, b, c)
        case Plus(c, Times(a, b)) => FMA(a, b, c)
        case Minus(Times(a, b), c) => FMA(a, b, UMinus(c))
        case Minus(c, Times(a, b)) => FMA(UMinus(a), b, c)
      }(e)
  }

  // a/b -> a*1/b
  def divisionToReciprocal(e: Expr): Expr = {
    TreeOps.replace{
      case Division(a, b) => Times(Division(one, b),a)
    }(e)
  }

  // Returns all repeated sub-expressions in an expression
  def repeatedExpr(e: Expr): Seq[Expr] = {
    val list = TreeOps.getSubExpr(e)
    val newList = list.toSet.toSeq
    val rep = list.diff(newList)
    rep.distinct
  }

  def commonSubexpressionElimination(e: Expr): Expr = {
    val list = repeatedExpr(e).sortWith(TreeOps.size(_) > TreeOps.size(_))
    var map: Map [Expr, Variable] = Map()
    list.fold(e)({
      case (currExpr, subExpr) =>
        val v = Trees.Variable(Identifiers.FreshIdentifier("v" + list.indexOf(subExpr), Types.RealType))
        currExpr match {
          case let @ Let(a, b, c) if (TreeOps.exists{case x if x == (subExpr) => true}(b)) =>
            if(!map.contains(subExpr)) {
              map += (subExpr -> v)
              Trees.Let(v.id, subExpr, TreeOps.replace{Map(subExpr -> v)}(let))
            }else {
              TreeOps.replace { Map(subExpr -> v) }(let)
            }
          case let @ Let(a,b,c) =>
            c match {
              case Let(_,_,_) => let
              case last if(TreeOps.exists{case x if x == (subExpr) => true}(c)) =>
                if(!map.contains(subExpr)) {
                  map += (subExpr -> v)
                  Trees.Let(a,b,Trees.Let(v.id, subExpr, TreeOps.replace{Map(subExpr -> v)}(c)))
                }else {
                  Trees.Let(a,b,TreeOps.replace { Map(subExpr -> v) }(c))
                }
              case _ => c
            }
          case last if(last == TreeOps.getLastExpression(currExpr) && TreeOps.exists{case x if x == (subExpr) => true}(last))=>
           Trees.Let(v.id, subExpr, TreeOps.replace{Map(subExpr -> v)}(currExpr))
        }
    })
  }

  // maps the power to the coefficient
  def powerToCoef(e: Expr): Map[Int,Expr] = {
    e match {

      case Times(a, IntPow(b,c)) =>  Map(c -> a)
      case Times(a, Variable(b)) => Map(1 -> a)

      case Division(IntPow(b,c), a) => Map(c -> Division(one, a))
      case Division(Variable(b), a) => Map(1 -> Division(one, a))

      case IntPow(a,b) => Map(b -> one)
      case Variable(a) => Map(1 -> one)

      case UMinus(IntPow(a,b)) => Map(b -> neg_one)
      case UMinus(Variable(a)) => Map(1 -> neg_one)
      case _ => Map()
    }
  }

  // gets the coef of every power in desc order
  def coefficients(list: List[Expr]): List[Expr] = {
    val coef = ListMap(list.foldLeft(Map[Int,Expr]())({ case (currMap, expr) =>
      val map = powerToCoef(expr).head
      if (currMap.contains(map._1)) {
        currMap + (map._1 -> Plus(map._2, currMap(map._1)))
      } else {
        currMap + map
      }}).toList.sortWith(_._1 >_._1):_* )

    val size = coef.head._1
    var res = List[Expr]()
    for(i<- 0 to size){
      res ::= coef.getOrElse(i, zero)
    }
     res
  }


  // 5*(x+y) => 5*x + 5*y
  def distributivity(e: Expr): Expr = {
   TreeOps.replace({
      case Times(a,Plus(b,c)) if TreeOps.freeVariablesOf(a).isEmpty  => Plus(Times(a,b), Times(a,c))
      case Times(a,Minus(b,c)) if TreeOps.freeVariablesOf(a).isEmpty => Minus(Times(a,b), Times(a,c))

      case Division(Plus(a,b),c) if TreeOps.freeVariablesOf(c).isEmpty => Plus(Division(a,c), Division(b,c))
      case Division(Minus(a,b),c) if TreeOps.freeVariablesOf(c).isEmpty => Minus(Division(a,c), Division(b,c))

      case Division(Times(a,b),c) => Times(Division(a,c),b)

      case UMinus(Times(a, b @ (Variable(_) | IntPow(_,_)))) => Times(UMinus(a),b)
      case UMinus(Division(a @ (Variable(_) | IntPow(_,_)),b)) => Division(a,UMinus(b))

      case Times(a, UMinus(b)) => Times(UMinus(a),b)
      case Division(UMinus(a), b) => Division(a,UMinus(b))
     // case UMinus(a) => UMinus(distributivity(a))

    },true)(e)
  }

  // isolate each subexpr by removing the plus and minus operators
  def separateExpr(e: Expr):Seq[Expr] ={
    e match{
      case Plus(a, b) => separateExpr(a) ++ separateExpr(b)
      case Minus(a, b) => separateExpr(a) ++ separateExpr(UMinus(b))
      case UMinus(a) => val l = separateExpr(a)
        var res = Seq[Expr]()
        for(elem <- l) res = res :+ UMinus(elem)
        res
      case _ => Seq(e)
    }
  }

  // map every variable to a list of all subExprs containing only this variable
  def separateVariables(seq: Seq[Expr]): (List[Expr], Map[Variable, List[Expr]]) = {
    var map = Map[Variable, List[Expr]]().withDefaultValue(List())
    var list = List[Expr]()
    for(expr <- seq) {
      val variables = TreeOps.freeVariablesOf(expr).toList
      variables match{
        case Nil => list = list ++ List(expr)
        case (x::Nil) =>  val v = Variable(x)
          val newExpr = PowTransformerPhase.timesToPow(distributivity(moreSimplify(expr,false)))
          newExpr match {
            case Times(a,b @ (Variable(_) | IntPow(Variable(_),_))) if TreeOps.freeVariablesOf(a).isEmpty  =>
              map = map + (v -> (List(newExpr) ++ map(v)))
            case Division(a @ (Variable(_) | IntPow(Variable(_),_)),b) if TreeOps.freeVariablesOf(b).isEmpty  =>
              map = map + (v -> (List(newExpr) ++ map(v)))
            case Variable(_) |
                 UMinus(Variable(_)) |
                 IntPow(Variable(_),_) |
                 UMinus(IntPow(Variable(_),_)) => map = map + (v -> (List(newExpr) ++ map(v)))
            case _ => list = list ++ List(newExpr)
          }
        case (x::xs) => list = list ++ List(expr)
      }
    }
    (list,map)
  }

  // the same as PowTransformerPhase.timesToPow but it makes the accuracy worse
  def timesToPow(e: Expr): Expr = {
    TreeOps.replace({
      case Times(Times(a, x), y) if x == y => Times(a, IntPow(x, 2))
      case Times(Times(a, IntPow(x, n)), y) if x == y => Times(a, IntPow(x, n + 1))

      case Times(x, y) if x == y => IntPow(x, 2)
      case Times(x, IntPow(y, n)) if x == y => IntPow(x, n + 1)
      case Times(IntPow(y, n), x) if x == y => IntPow(x, n + 1)

      case Times(UMinus(x), y) if x == y => UMinus(IntPow(x, 2))
      case Times(UMinus(x), IntPow(y, n)) if x == y => UMinus(IntPow(x, n + 1))
      case Times(UMinus(IntPow(y, n)), x) if x == y => UMinus(IntPow(x, n + 1))
    }, true)(e)
  }


  // x^4 + 3x^2 + 1 => x * (x *( x* x) + 3) + 1
  def hornerScheme(e: Expr): Expr = {
    val newExpr = timesToPow(e)
    newExpr match {
      case Let(a, b, c) => Let(a, hornerScheme(b), hornerScheme(c))
      case _ =>
        val (list, map) = separateVariables(separateExpr(distributivity(newExpr)))
        if(!map.isEmpty) {
          val (v, l) = map.head
          val coef = coefficients(l)
          val expr = coef.tail.foldLeft(coef.head)({ case (currExpr, elem) => Plus(Times(v, currExpr), elem) })

          val res = PowTransformerPhase.powToTimes(easySimplify(list.fold(
            map.tail.foldLeft(expr)({ case (currExpr, (v, l)) =>
              val coef = coefficients(l)
              Plus(currExpr, coef.tail.foldLeft(coef.head)({ case (currExpr, elem) => Plus(Times(v, currExpr), elem) }))
            }))(Plus(_, _))))
         if (TreeOps.size(e) < TreeOps.size(res)){
           e
         } else {
           res
         }

        } else {
          PowTransformerPhase.powToTimes(newExpr)
        }
    }
  }

}
