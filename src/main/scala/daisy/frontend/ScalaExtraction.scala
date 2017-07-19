

package daisy.frontend

import scala.collection.immutable.Seq

import leon.purescala._
import Definitions.UnitDef

import daisy.lang.Trees
import daisy.utils.Rational

/**
  This is the old frontend using Leon.
*/
object ScalaExtraction {

  case class UnsupportedFragmentException(msg: String) extends Exception(msg)

  implicit val debugSection = daisy.DebugSectionFrontend


  def run(ctx: daisy.Context): Trees.Program = {
    println("Extracting program")
    val timer = ctx.timers.frontend.start

    val leonOpts = List[String]()

    val pipeline = leon.frontends.scalac.ExtractionPhase

    val reporter    = new leon.DefaultReporter(Set())
    val im          = new leon.utils.InterruptManager(reporter)

    val leonContext = leon.Main.processOptions(leonOpts).copy(reporter = reporter, interruptManager = im)

    //println("files: " + (ctx.files ++ ctx.libFiles))

    val (_, pgm) = pipeline.run(leonContext, ctx.libFiles.toList ++ ctx.files.toList)

    val mainUnit = pgm.units.filter(_.isMainUnit)

    //println("Main unit:")
    //println(mainUnit)

    val daisyProgram = extractDaisyProgram(mainUnit)

    timer.stop
    ctx.reporter.debug("Extracted program: " + daisyProgram)

    daisyProgram
    //println("Program: ")
    //println(daisyProgram)

  }

  private def getPosition(p: leon.utils.Positioned): daisy.utils.Position = p.getPos match {
    case leon.utils.OffsetPosition(line, col, point, file) =>
      daisy.utils.OffsetPosition(line, col, point, file)

    case leon.utils.RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file) =>
      daisy.utils.RangePosition(lineFrom, colFrom, pointFrom, lineTo, colTo, pointTo, file)

    case leon.utils.NoPosition => daisy.utils.NoPosition
  }


  private def extractDaisyProgram(units: List[UnitDef]): Trees.Program = {

    assert(units.length == 1, "More than one top-level unit found.")
    assert(units.head.modules.length == 1, "More that one module found.")

    val module = units.head.modules.head

    // TODO: positions are not copied at this point!

    val fncs: Seq[Trees.FunDef] = Seq( module.definedFunctions.map(f => {
      Trees.FunDef(
        mapId(f.id),
        mapType(f.returnType),
        Seq( f.params.map(v => Trees.ValDef(mapId(v.id))) :_* ),  // we have immutable Seq's
        f.precondition.map(mapExpr(_)),
        f.body.map(mapExpr(_)),
        f.postcondition.map(mapExpr(_)),
        f.canBeField).setPos(getPosition(f))
      }) :_* )


    Trees.Program(mapId(module.id), fncs)

  }

  private def mapType(tpe: leon.purescala.Types.TypeTree): daisy.lang.Types.TypeTree = tpe match {
    case Types.Untyped => daisy.lang.Types.Untyped
    case Types.BooleanType => daisy.lang.Types.BooleanType
    case Types.UnitType => daisy.lang.Types.UnitType
    case Types.IntegerType => daisy.lang.Types.IntegerType
    case Types.Int32Type => daisy.lang.Types.Int32Type
    case Types.RealType => daisy.lang.Types.RealType
    case Types.FunctionType(from, to) =>
      daisy.lang.Types.FunctionType(
        Seq(from.map(mapType(_)):_*), mapType(to))
    case _ => throw new UnsupportedFragmentException("Unsupported: " + tpe)
  }


  def mapExpr(expr: Expressions.Expr): Trees.Expr = expr match {
    case Expressions.NoTree(tpe) => Trees.NoTree(mapType(tpe))
    case Expressions.Error(tpe, desc) => Trees.Error(mapType(tpe), desc).setPos(getPosition(expr))
    case Expressions.Require(pred, body) => Trees.Require(mapExpr(pred), mapExpr(body)).setPos(getPosition(expr))
    case Expressions.Ensuring(body, pred) => Trees.Ensuring(mapExpr(body), mapExpr(pred)).setPos(getPosition(expr))
    case Expressions.Assert(pred, err, body) => Trees.Assert(mapExpr(pred), err, mapExpr(body)).setPos(getPosition(expr))
    case Expressions.Variable(id) => Trees.Variable(mapId(id)).setPos(getPosition(expr))

    case Expressions.Let(binder, value, body) =>
      Trees.Let(mapId(binder), mapExpr(value), mapExpr(body)).setPos(getPosition(expr))
    case Expressions.FunctionInvocation(fd, args) =>
      Trees.FunctionInvocation(
        mapId(fd.id),
        Seq(fd.params.map(vd => Trees.ValDef(mapId(vd.id))):_*),
        Seq(args.map(mapExpr(_)):_*),
        mapType(fd.returnType)).setPos(getPosition(expr))
    case Expressions.IfExpr(cond, thenn, elze) =>
      Trees.IfExpr(mapExpr(cond), mapExpr(thenn),mapExpr(elze)).setPos(getPosition(expr))
    case Expressions.Lambda(args, body) =>
      Trees.Lambda(
        Seq(args.map(v => Trees.ValDef(mapId(v.id))):_*),

        mapExpr(body)).setPos(getPosition(expr))

    case Expressions.IntLiteral(v) => Trees.Int32Literal(v).setPos(getPosition(expr))
    case Expressions.InfiniteIntegerLiteral(v) => Trees.IntegerLiteral(v).setPos(getPosition(expr))
    case Expressions.BooleanLiteral(b) => Trees.BooleanLiteral(b).setPos(getPosition(expr))
    case Expressions.UnitLiteral() => Trees.UnitLiteral().setPos(getPosition(expr))
    case Expressions.RealLiteral(s) =>
      val tmp = Trees.RealLiteral(Rational.fromString(s)).setPos(getPosition(expr))
      tmp.stringValue = s
      tmp

    case Expressions.Equals(lhs, rhs) => Trees.Equals(mapExpr(lhs), mapExpr(rhs)).setPos(getPosition(expr))
    case Expressions.And(exprs) => Trees.And(Seq(exprs.map(mapExpr(_)):_*)).setPos(getPosition(expr))
    case Expressions.Or(exprs) => Trees.Or(Seq(exprs.map(mapExpr(_)):_*)).setPos(getPosition(expr))
    case Expressions.Implies(l, r) => Trees.Implies(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.Not(t) => Trees.Not(mapExpr(t)).setPos(getPosition(expr))
    // there are also integer versions of this: Plus, Minus, etc.
    case Expressions.RealPlus(l, r) => Trees.Plus(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.RealMinus(l, r) => Trees.Minus(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.RealTimes(l, r) => Trees.Times(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.RealDivision(l, r) => Trees.Division(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.RealPow(l, r) => Trees.Pow(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.RealUMinus(t) => Trees.UMinus(mapExpr(t)).setPos(getPosition(expr))
    case Expressions.RealSqrt(t) => Trees.Sqrt(mapExpr(t)).setPos(getPosition(expr))
    case Expressions.RealSin(t) => Trees.Sin(mapExpr(t)).setPos(getPosition(expr))
    case Expressions.RealCos(t) => Trees.Cos(mapExpr(t)).setPos(getPosition(expr))
    case Expressions.RealTan(t) => Trees.Tan(mapExpr(t)).setPos(getPosition(expr))
    case Expressions.RealExp(t) => Trees.Exp(mapExpr(t)).setPos(getPosition(expr))
    case Expressions.RealLog(t) => Trees.Log(mapExpr(t)).setPos(getPosition(expr))
    case Expressions.LessThan(l, r) => Trees.LessThan(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.GreaterThan(l, r) => Trees.GreaterThan(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.LessEquals(l, r) => Trees.LessEquals(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.GreaterEquals(l, r) => Trees.GreaterEquals(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))
    case Expressions.AbsError(l, r) => Trees.AbsError(mapExpr(l), mapExpr(r)).setPos(getPosition(expr))

  }

  private var idMap: Map[Common.Identifier, daisy.lang.Identifiers.Identifier] = Map.empty

  def mapId(id: Common.Identifier): daisy.lang.Identifiers.Identifier = idMap.get(id) match {
    case Some(idd) => idd
    case None =>
      val fresh = daisy.lang.Identifiers.FreshIdentifier(id.name, mapType(id.getType))
      idMap = idMap + (id -> fresh)
      fresh
  }
}