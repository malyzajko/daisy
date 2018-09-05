// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.collection.immutable.{Map, Seq}
import scala.util.parsing.combinator._

import lang.Trees._
import tools.{Interval, PartialInterval, Rational, FinitePrecision}
import FinitePrecision._
import lang.Identifiers._
import lang.Extractors._
import lang.TreeOps.allVariablesOf
import lang.Trees.Program

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
  override val shortName = "specs"
  override val description = "Processes the specifications for later phases."

  implicit val debugSection = DebugSectionAnalysis

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
    var resRanges: Map[Identifier, PartialInterval] = Map()
    var resErrors: Map[Identifier, Rational] = Map()
    var additionalConst: Map[Identifier, Expr] = Map()
    var resIds: Map[Identifier, Seq[Identifier]] = Map()
    // var requiredOutputRanges: Map[Identifier, Map[Identifier, PartialInterval]] = Map()
    // var requiredOutputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()

    val newDefs = transformConsideredFunctions(ctx, prg){ fnc =>
      fnc.precondition match {
        case Some(pre) =>
          // TODO: additional constraints
          val (ranges, errors, addConds) = extractPreCondition(pre)

          val missingKeys = fnc.params.map(_.id).diff(ranges.keys.toSeq)
          if (!missingKeys.isEmpty) {
            reporter.fatalError("Incomplete or missing range for " + missingKeys.mkString(", "))
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
              errors ++ missingIDs.map(id => (id -> precisionMap(id).absRoundoff(allRanges(fnc.id)(id))))

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

    ctx.reporter.debug("range bounds: " + resRanges.mkString("\n"))
    ctx.reporter.debug("error bounds: " + resErrors.mkString("\n"))

    (ctx.copy(
      specInputRanges = allRanges,
      specInputErrors = allErrors,
      specResultRangeBounds = resRanges,
      specResultErrorBounds = resErrors,
      specInputPrecisions = inputPrecision,
      specResultPrecisions = resultPrecisions,
      specAdditionalConstraints = additionalConst,
      resultTupleIds = resIds),  
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
    Map[Identifier, Rational], Seq[Expr]) = {

    val (lowerBound, upperBound, absError, addCond) = extractCondition(expr)

    // only return complete ranges, check for completeness is done later
    val bounds = (lowerBound.keySet.intersect(upperBound.keySet)).map(k =>
      (k, (lowerBound(k), upperBound(k)))).toMap

    (bounds, absError, addCond)
  }

  def extractPostCondition(expr: Expr): (Map[Identifier, (Option[Rational], Option[Rational])],
    Map[Identifier, Rational]) = {

    val (lowerBound, upperBound, absError, _) = extractCondition(expr)

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
    Map[Identifier, Rational], Map[Identifier, Rational], Seq[Expr]) = {

    var lowerBound: Map[Identifier, Rational] = Map.empty
    var upperBound: Map[Identifier, Rational] = Map.empty
    var absError: Map[Identifier, Rational] = Map.empty
    var additionalCond: Seq[Expr] = Seq.empty

    def extract(e: Expr): Unit = e match {
      case Lambda(args,body) => extract(body)
      case AbsError(Variable(id), RealLiteral(r)) => absError += (id -> r)

      case And(es) => es.foreach(extract(_))
      case GreaterThan(Variable(id), RealLiteral(r)) => lowerBound += (id -> r)
      case GreaterEquals(Variable(id), RealLiteral(r)) => lowerBound += (id -> r)

      case LessThan(RealLiteral(r), Variable(id)) => lowerBound += (id -> r)
      case LessEquals(RealLiteral(r), Variable(id)) => lowerBound += (id -> r)

      case LessThan(Variable(id), RealLiteral(r)) => upperBound += (id -> r)
      case LessEquals(Variable(id), RealLiteral(r)) => upperBound += (id -> r)

      case GreaterThan(RealLiteral(r), Variable(id)) => upperBound += (id -> r)
      case GreaterEquals(RealLiteral(r), Variable(id)) => upperBound += (id -> r)

      case _ => ;
        additionalCond = additionalCond :+ e
    }

    extract(e)

    (lowerBound, upperBound, absError, additionalCond)
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

  def typeVar: Parser[TypeAssign] = identifier ~ ":" ~ (float256 | float128 | float64 | float32) ^^ {
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