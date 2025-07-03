// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.collection.immutable.{Map, Seq}
import scala.util.parsing.combinator._
import lang.Trees._
import tools.{DSAbstraction, FinitePrecision, Index, Interval, MPFRInterval, MatrixIndex, PartialInterval, Rational, VectorIndex}
import FinitePrecision._
import daisy.lang.Types.{MatrixType, RealType, VectorType}
import lang.Identifiers._
import lang.Extractors._
import lang.TreeOps.allVariablesOf
import lang.Trees.Program
import lang.Types._

/**
  This phase extracts information from the specifications (requires, ensuring)
  and puts it somewhere useful.

  Note:
  - extract upper and lower bounds as well as absolute errors on the precondition
  - strictly greater than is extracted as greater-equal
  - if a bound is defined twice, then any of the bounds can be recorded, i.e.
    there is no checking for weakest or strongest
  - absolute error can be only a single rational number, i.e. in the form x +/- 5
  - does not check for completeness of spec
  - assumes fnc. body exists (does not check)
  - ignores functions without precondiion

  Prerequisite:
    None
 */
object SpecsProcessingPhase extends DaisyPhase with PrecisionsParser {
  override val name = "Specs processing"
  override val description = "Processes the specifications for later phases."
  override implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter

    val trackInitialErrs = !ctx.hasFlag("noInitialErrors")
    val trackRoundoffErrs = !ctx.hasFlag("noRoundoff")
    val defaultPrecision = ctx.option[Precision]("precision")

    // By default, each function's input variables are given defaultPrecision
    var inputPrecision: Map[Identifier, Map[Identifier, Precision]] =
      Map.empty.withDefaultValue(Map.empty.withDefaultValue(defaultPrecision))

    // By default, each function will return with defaultPrecision
    var resultPrecisions: Map[Identifier, Precision] =
      Map.empty.withDefaultValue(defaultPrecision)

    ctx.option[Option[String]]("mixed-precision") match {
      case Some(file) =>

        // a mixed-precision assignment file has been provided, parse it
        val precMapsParsed = parseMixedPrecisionFile(file, prg)

        prg.defs.foreach(fnc => {
          (precMapsParsed.get(fnc.id), fnc.body) match {
            // no spec is given, so use default precision
            case (None, _) =>

            case (Some(precMap), None) =>
              inputPrecision += fnc.id -> precMap

            case (Some(precMap), Some(body)) =>
              val (completePrecMap, returnPrec) = typecheck(body, precMap, defaultPrecision)
              inputPrecision += fnc.id -> completePrecMap
              resultPrecisions += fnc.id -> returnPrec

          }
        })

      case None =>
    }

    // ctx.reporter.info("precision map:")
    // ctx.reporter.info(precisionMap)

    var allRanges: Map[Identifier, Map[Identifier, Interval]] = Map()
    var allErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
    var allInitErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
    var resRanges: Map[Identifier, PartialInterval] = Map()
    var resErrors: Map[Identifier, Rational] = Map()
    var additionalConst: Map[Identifier, Expr] = Map()
    var resIds: Map[Identifier, Seq[Identifier]] = Map()
    // var requiredOutputRanges: Map[Identifier, Map[Identifier, PartialInterval]] = Map()
    // var requiredOutputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
    var dsAbstractions: Map[Identifier, Map[Expr, DSAbstraction]] = Map()

    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>
      fnc.precondition match {
        case Some(pre) =>
          // TODO: additional constraints
          val (ranges, errors, addConds, dsa) = extractPreCondition(pre)

          val missingKeys = fnc.params.map(_.id).diff(ranges.keys.toSeq)
          if (missingKeys.nonEmpty) {
            reporter.fatalError("Incomplete or missing range for " + missingKeys.mkString(", "))
          }

          dsa match {
            case Some(dsas) => dsAbstractions += fnc.id -> dsas
            case None => ;
          }

          allRanges += (fnc.id -> ranges.map(x => (x._1 -> Interval(x._2._1, x._2._2))))
          // If we track both input and roundoff errors, then we pre-compute
          // the roundoff errors for those variables that do not have a user-defined
          // error, in order to keep correlations.
          allErrors += (fnc.id -> ({
            // get the possibly mixed-precision map
            val precisionMap = inputPrecision(fnc.id)

            if (trackInitialErrs && trackRoundoffErrs){

              val allIDs = fnc.params.map(_.id).toSet
              val missingIDs = allIDs -- errors.keySet
              errors ++ missingIDs.map(id => if(id.getType == Int32Type) (id -> Rational.zero)
                                                else (id -> precisionMap(id).absRoundoff(allRanges(fnc.id)(id))))

            } else if (trackInitialErrs) {

              val allIDs = fnc.params.map(_.id).toSet
              val missingIDs = allIDs -- errors.keySet
              errors ++ missingIDs.map(id => (id -> Rational.zero))

            } else if (trackRoundoffErrs) {

              val allIDs = fnc.params.map(_.id)
              allIDs.map(id => (id -> precisionMap(id).absRoundoff(allRanges(fnc.id)(id)))).toMap

            } else {

              val allIDs = fnc.params.map(_.id)
              allIDs.map(id => (id -> Rational.zero)).toMap

            }}))
          allInitErrors += (fnc.id -> ({
            val allIDs = fnc.params.map(_.id).toSet
            val missingIDs = allIDs -- errors.keySet
            errors ++ missingIDs.map(id => (id -> Rational.zero))
          }))
          addConds match {
            case Seq() => additionalConst += (fnc.id -> BooleanLiteral(true)) // no additional constraints
            case Seq(x) => additionalConst += (fnc.id -> x)
            case _ => additionalConst += (fnc.id -> And(addConds))
          }


          // annotate the variables in the function body directly
          // preTraversal ({
          //   case x @ Variable(id) =>
          //     ranges.get(id) match {
          //       case Some((a, b)) => x.interval = Interval(a, b)
          //       case None => ;
          //     }
          //     errors.get(id) match {
          //       case Some(err) => x.absError = err
          //       case None => ;
          //     }
          //   case _ => ;
          //   })(fnc.body.get)

        case _ =>
      }

      // If there is a partial post condition, there will be an empty map for range or error
      // If there is no post condition, there will be no map at all.
      // This allows us to differentiate between these two cases - for future use

      fnc.postcondition match {
        case Some(post) =>
          val (ranges, errors) = extractPostCondition(post)
          assert(ranges.size <= 1)
          assert(errors.size <= 1)

          // no range given in postcondition
          if (ranges.size == 1) {
            val tmp = ranges.values.head
            resRanges += (fnc.id -> PartialInterval(tmp._1, tmp._2))
          } else if (ranges.size > 1) {
            ctx.reporter.error("Tuples are not supported.")
          }

          if (errors.size == 1) {
            val tmp = errors.values.head
            resErrors += (fnc.id -> tmp)
          } else if (errors.size > 1) {
            ctx.reporter.error("Tuples are not supported.")
          }

          // requiredOutputRanges += (fnc.id -> ranges.map( x =>
          //  (x._1 -> PartialInterval(x._2._1, x._2._2) )))
          // requiredOutputErrors += (fnc.id -> errors)



        case _ =>
          // ctx.reporter.info("No post-condition found\n")

      }


      // if the function returns a tuple, untuple it
      fnc.returnType match {
        case lang.Types.TupleType(_) =>
          fnc.copy(body = Some(lang.TreeOps.replace { 
            case Tuple(args) => 
              val (newBody, resIdsTmp) = untuple(args)
              resIds += (fnc.id -> resIdsTmp)
              newBody 
          } (fnc.body.get)))
        case _ => fnc
      }
    }

    (ctx.copy(
      specInputRanges = allRanges,
      specInputErrors = allErrors,
      specInitErrors  = allInitErrors,
      specResultRangeBounds = resRanges,
      specResultErrorBounds = resErrors,
      specInputPrecisions = inputPrecision,
      specResultPrecisions = resultPrecisions,
      specAdditionalConstraints = additionalConst,
      resultTupleIds = resIds,
      originalProgram = Program(prg.id, newDefs),
      dsAbstractions = dsAbstractions),
      Program(prg.id, newDefs))
  }

  // turns a tuple into a Let statement with fresh variables
  private def untuple(args: Seq[Expr]): (Expr, Seq[Identifier]) = {
    if (args.size == 1) {
      val fresh = FreshIdentifier("#res1", args.head.getType)
      (Let(fresh, args.head, Variable(fresh)), Seq(fresh))
    } else {
      val fresh = FreshIdentifier("#res" + args.size, args.last.getType)
      val (tmp, list) = untuple(args.init) 
      (Let(fresh, args.last, tmp), list :+ fresh)
    }
  }

  def extractPreCondition(expr: Expr): (Map[Identifier, (Rational, Rational)],
    Map[Identifier, Rational], Seq[Expr], Option[Map[Expr, DSAbstraction]]) = {

    val (lowerBound, upperBound, absError, addCond, dsSizes, preAbstractions) = extractCondition(expr)

    // only return complete ranges, check for completeness is done later
    val bounds = (lowerBound.keySet.intersect(upperBound.keySet)).map(k =>
      (k, (lowerBound(k), upperBound(k)))).toMap
    val dsAbstractions = extractAbstraction(lowerBound, upperBound, dsSizes, preAbstractions)

    (bounds, absError, addCond, dsAbstractions)
  }

  def extractPostCondition(expr: Expr): (Map[Identifier, (Option[Rational], Option[Rational])],
    Map[Identifier, Rational]) = {

    val (lowerBound, upperBound, absError, _, _, _) = extractCondition(expr)

    val lowerMap = lowerBound.map({
      case (x, r) => (x, (Some(r), upperBound.get(x)))
    })

    val upperMap = upperBound.map({
      case (x, r) => (x, (lowerBound.get(x), Some(r)))
    })

    val resultMap = (lowerMap ++ upperMap)
    (resultMap, absError)

  }

  def extractCondition(e: Expr): (Map[Identifier, Rational],
    Map[Identifier, Rational], Map[Identifier, Rational], Seq[Expr], Map[Expr, Index], Map[Expr, DSAbstraction]) = {

    var lowerBound: Map[Identifier, Rational] = Map.empty
    var upperBound: Map[Identifier, Rational] = Map.empty
    var absError: Map[Identifier, Rational] = Map.empty
    var additionalCond: Seq[Expr] = Seq.empty
    var dsSizes: Map[Expr, Index] = Map.empty
    var preAbstractions: Map[Expr, DSAbstraction] = Map.empty

    def extract(e: Expr): Unit = e match {
      case Lambda(_,body) => extract(body)
      case AbsError(Variable(id), RealLiteral(r)) => absError += (id -> r)

      case And(es) => es.foreach(extract)
      case GreaterThan(Variable(id), RealLiteral(r)) => lowerBound += (id -> r)
      case GreaterEquals(Variable(id), RealLiteral(r)) => lowerBound += (id -> r)

      case LessThan(RealLiteral(r), Variable(id)) => lowerBound += (id -> r)
      case LessEquals(RealLiteral(r), Variable(id)) => lowerBound += (id -> r)

      case LessThan(Variable(id), RealLiteral(r)) => upperBound += (id -> r)
      case LessEquals(Variable(id), RealLiteral(r)) => upperBound += (id -> r)

      case GreaterThan(RealLiteral(r), Variable(id)) => upperBound += (id -> r)
      case GreaterEquals(RealLiteral(r), Variable(id)) => upperBound += (id -> r)
      // DS ranges and size
      case SizeLessEquals(ds, cols, rows) => ds.getType match {
        case VectorType(_) => dsSizes += (ds -> VectorIndex(cols))
        case MatrixType(_) => dsSizes += (ds -> MatrixIndex(cols, rows))
      }
      case VectorRange(v, fromI, toI, RealLiteral(lo), RealLiteral(up)) =>
        val interval = MPFRInterval(Interval(lo, up))
        // check that the indices are inside the main range
        if (dsSizes.contains(v)) {
          val max = dsSizes(v) match { case VectorIndex(i) => i}
          val newIndices =
            if (toI >= max)
              if (fromI >= max)
                Set[Index]()
              else
                Set.range(fromI,Math.max(max-1,fromI+1)).map(x=>VectorIndex(x).asInstanceOf[Index])
            else
              Set.range(fromI,toI+1).map(x=>VectorIndex(x).asInstanceOf[Index])
          if (newIndices.nonEmpty)
            if (preAbstractions.contains(v)) {
              preAbstractions += v -> preAbstractions(v).addSpec(newIndices, interval)
            } else {
              preAbstractions += v -> DSAbstraction(Map(newIndices -> interval))
            }
          else
            reporter.info(s"The specified range is for indices out of bounds: ($fromI, $toI) not in (0,${dsSizes(v).asInstanceOf[VectorIndex].i-1}.\n Ignoring the spec.")
        } else {
          // todo does this happen often? disallow?
          val indices = Set.range(fromI,toI+1).map(x => VectorIndex(x).asInstanceOf[Index])
          if (preAbstractions.contains(v)) {
            preAbstractions += v -> preAbstractions(v).addSpec(indices, interval)
          } else {
            preAbstractions += v -> DSAbstraction(Map(indices -> interval))
          }
        }

      case MatrixRange(m, indices, RealLiteral(lo), RealLiteral(up)) =>
        if (dsSizes.contains(m)) {
          // check that the indices are inside the main range
          val bounds = dsSizes(m) match {
            case MatrixIndex(i,j) => (i,j)
          }
          val inds = indices.filter({ case (i, j) => i < bounds._1 && j < bounds._2 })
            .map({ case (i, j) => MatrixIndex(i, j).asInstanceOf[Index] }).toSet
          if (inds.nonEmpty) {
            val interval = MPFRInterval(Interval(lo, up))
            if (preAbstractions.contains(m)) {
              preAbstractions += m -> preAbstractions(m).addSpec(inds, interval)
            } else {
              preAbstractions += m -> DSAbstraction(Map(inds -> interval))
            }
          } else
            reporter.info(s"The specified range is for indices out of bounds: ($indices) not in the matrix ${bounds._1}x${bounds._2}.\n Ignoring the spec.")
        } else {
          val inds = indices.map({ case (i, j) => MatrixIndex(i, j).asInstanceOf[Index] }).toSet
          val interval = MPFRInterval(Interval(lo, up))
          if (preAbstractions.contains(m)) {
            preAbstractions += m -> preAbstractions(m).addSpec(inds, interval)
          } else {
            preAbstractions += m -> DSAbstraction(Map(inds -> interval))
          }
        }
      case _ => ;
        additionalCond = additionalCond :+ e
    }

    extract(e)

    (lowerBound, upperBound, absError, additionalCond, dsSizes, preAbstractions)
  }

  def extractAbstraction(lowerBounds: Map[Identifier, Rational], upperBounds: Map[Identifier, Rational], sizes: Map[Expr, Index], preAbstractions: Map[Expr, DSAbstraction] ): Option[Map[Expr, DSAbstraction]] = {
    val dsTypes = lowerBounds.filter({ case (id,_) => id.getType == VectorType(Seq(RealType)) || id.getType == MatrixType(Seq(RealType)) })
    if (dsTypes.isEmpty)
      return None

    val testBounds = lowerBounds.keySet.diff(upperBounds.keySet)
    assert(testBounds.isEmpty, s"Ranges for some input vectors/matrices are incomplete or ambiguous: $testBounds")
    def crossProduct(xs:Seq[Int], ys:Seq[Int]): Seq[Index] = for {x <- xs; y <- ys} yield MatrixIndex(x,y)

    val dsInputsLB = lowerBounds.filter({ case (dsId, _) => dsId.getType match {
      case VectorType(_) => true
      case MatrixType(_) => true
      case _ => false
    }})
    val wholeAbstractions: Map[Expr, DSAbstraction] = dsInputsLB.map({ case (dsId, lb) => dsId.getType match {
      case VectorType(_) =>
        val ub = upperBounds(dsId)
        val v = VectorLiteral(dsId)
        val indexRange = if (sizes.contains(v)) {
          sizes(v) match { case VectorIndex(i) => Set.from(0 until i).map(x => VectorIndex(x).asInstanceOf[Index]) }
        } else
          Set(VectorIndex(0).asInstanceOf[Index], VectorIndex(-1).asInstanceOf[Index]) // todo do we allow unspecified length of DS?

        if (preAbstractions.contains(v))
          v -> preAbstractions(v).addSpecNoUpdate(indexRange, MPFRInterval(Interval(lb,ub)), debug = reporter.isDebugEnabled)
        else
          v -> DSAbstraction(Map(indexRange -> MPFRInterval(Interval(lb,ub))))
      case MatrixType(_) =>
        val ub = upperBounds(dsId)
        val m = MatrixLiteral(dsId)
        val indexRange = if (sizes.contains(m)) {
          sizes(m) match { case MatrixIndex(i,j) => crossProduct(Seq.from(0 until i), Seq.from(0 until j))}
        } else Seq(MatrixIndex(0, 0), MatrixIndex(-1, -1)) // todo do we allow unspecified length of DS?
        if (preAbstractions.contains(m))
          m -> preAbstractions(m).addSpecNoUpdate(indexRange.toSet, MPFRInterval(Interval(lb,ub)), debug = reporter.isDebugEnabled)
        else
          m -> DSAbstraction(Map(indexRange.toSet -> MPFRInterval(Interval(lb,ub))))
    }})
    Some(wholeAbstractions)
  }

  private def parseMixedPrecisionFile(f: String, prg: Program): Map[Identifier, Map[Identifier, Precision]] = {

    val fncIdMap: Map[String, Identifier] = prg.defs.map(fnc =>
      fnc.id.toString -> fnc.id
    ).toMap
    val varIdMap: Map[String, Map[String, Identifier]] = prg.defs.map(fnc =>
      fnc.id.toString -> allVariablesOf(fnc.body.get).map(id => id.toString -> id).toMap
    ).toMap

    def treeToMap(p: List[FunAssign]): Map[Identifier, Map[Identifier, Precision]] = {

      def typeAssignsToMap(l: List[TypeAssign], funcId: String): Map[Identifier, Precision] = {
        l.foldLeft[Map[Identifier, Precision]](Map())((m, tA) => tA match {
          case TypeAssign(varName, precision) => m + (varIdMap(funcId)(varName) -> precision) })
      }

      p.foldLeft[Map[Identifier, Map[Identifier, Precision]]](Map())((m, fA) => fA match {
        case FunAssign(funName, types) => m + (fncIdMap(funName) -> typeAssignsToMap(types, funName))
      })

    }


    val bufferedSource = io.Source.fromFile(f)
    val sourceText = bufferedSource.mkString

    bufferedSource.close
    parse(overallFunction, sourceText) match {
      case Success(precisionsTree, _) => treeToMap(precisionsTree)
      case Failure(msg, _) => reporter.fatalError("Failure during the parsing of the type assignment file: " + msg)
      case Error(msg, _) => reporter.fatalError("Error during the parsing of the type assignment file: " + msg)
    }
  }


  def typecheck(expr: Expr, precisionMap: Map[Identifier, Precision],
    defaultPrecision: Precision): (Map[Identifier, Precision], Precision) = {

    def eval(e: Expr, precMap: Map[Identifier, Precision]): (Map[Identifier, Precision], Precision) = e match {

      case Variable(id) =>
        precMap.get(id) match {
          case Some(m) => (precMap, m)
          case None => (precMap + (id -> defaultPrecision), defaultPrecision)
        }
      case RealLiteral(r) => (precMap, defaultPrecision)

      case ArithOperator(Seq(l, r), recons) =>
        val (mapLeft, precLeft) = eval(l, precMap)
        val (mapRight, precRight) = eval(r, mapLeft)

        val precResult = getUpperBound(precLeft, precRight)

        (mapRight, precResult)

      case ArithOperator(Seq(u), recons) =>
        eval(u, precMap)

      case Let(id, value, body) =>
        val (mapValue, precValue) = eval(value, precMap)

        precMap.get(id) match {
          // precision of assigned variable is given
          case Some(m) =>
            eval(body, mapValue)

          case None =>
            eval(body, mapValue + (id -> precValue))
        }
    }
    eval(expr, precisionMap)
  }

}

trait PrecisionsParser extends RegexParsers with JavaTokenParsers {
  def identifier: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ {x: String => x }

  def float256: Parser[Precision] = "QuadDouble" ^^ { case _ => QuadDouble }
  def float128: Parser[Precision] = ("DoubleDouble" ||| "Quad") ^^ { case _ => DoubleDouble }
  def float64: Parser[Precision] = ("Float64" ||| "Double") ^^ { case _ => Float64 }
  def float32: Parser[Precision] = ("Float32" ||| "Float") ^^ { case _ => Float32 }
  def float16: Parser[Precision] = "Float16" ^^ { case _ => Float16 }

  def typeVar: Parser[TypeAssign] = identifier ~ ":" ~ (float16 | float256 | float128 | float64 | float32) ^^ {
    case i ~ _ ~ p => TypeAssign(i, p)
  }
  def typemapFunction: Parser[FunAssign] = identifier ~ "=" ~ "{" ~ rep1(typeVar) ~ "}" ^^ {
    case i ~ _ ~ _ ~ types ~ _ => FunAssign(i, types)
  }

  def overallFunction: Parser[List[FunAssign]] = rep1(typemapFunction) ^^ { case l => l }
}

abstract class Precisions
case class TypeAssign(f: String, p: Precision) extends Precisions
case class FunAssign(f: String, s: List[TypeAssign]) extends Precisions