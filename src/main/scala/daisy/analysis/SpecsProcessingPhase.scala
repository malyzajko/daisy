// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package analysis

import scala.util.parsing.combinator._

import lang.Trees._
import tools.{Interval, PartialInterval, Rational, FinitePrecision}
import FinitePrecision._
import lang.Identifiers._
import lang.Extractors._
import lang.TreeOps.{preTraversal, allVariablesOf}
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
  override val description = "Processes the specifications for later phases."
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  val defaultPrecision = Float64

  def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    // reporter.info("\nStarting specs preprocessing phase")
    val timer = ctx.timers.preprocess.start


    //
    val precisionMap: Map[Identifier, (Map[Identifier, Precision], Precision)] =
      ctx.findOption(Main.optionMixedPrecFile) match {

      case Some(file) =>
        // maps from fnc/variable names to their respective identifiers
        val (fncIdMap, varIdMap) = buildIdentifierMap(prg)

        // a mixed-precision assignment file has been provided, parse it
        val precMapsParsed = parseMixedPrecisionFile(file, fncIdMap, varIdMap)
        prg.defs.map(fnc => {
          (precMapsParsed.get(fnc.id), fnc.body) match {
            case (None, None) =>
              (fnc.id -> (Map[Identifier, Precision](), defaultPrecision))

            case (None, Some(body)) =>
              // no spec is given, so assign default precision
              (fnc.id -> (allVariablesOf(body).map(v => (v -> defaultPrecision)).toMap,
                defaultPrecision))

            case (Some(precMap), None) =>
              (fnc.id -> (precMap, defaultPrecision))

            case (Some(precMap), Some(body)) =>
              val (completePrecMap, returnPrec) = typecheck(body, precMap, defaultPrecision)
              (fnc.id -> (completePrecMap, returnPrec))

          }
        }).toMap
      case None =>
        Map()
    }

    // println("precision map:")
    // println(precisionMap)

    var allRanges: Map[Identifier, Map[Identifier, Interval]] = Map()
    var allErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
    var resRanges: Map[Identifier, PartialInterval] = Map()
    var resErrors: Map[Identifier, Rational] = Map()
    // var requiredOutputRanges: Map[Identifier, Map[Identifier, PartialInterval]] = Map()
    // var requiredOutputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()

    for (fnc <- prg.defs) {

      fnc.precondition match {
        case Some(pre) =>
          // TODO: additional constraints
          val (ranges, errors) = extractPreCondition(pre)

          allRanges += (fnc.id -> ranges.map(x => (x._1 -> Interval(x._2._1, x._2._2))))
          allErrors += (fnc.id -> errors)

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
            reporter.error("Tuples are not supported.")
          }

          if (errors.size == 1) {
            val tmp = errors.values.head
            resErrors += (fnc.id -> tmp)
          } else if (errors.size > 1) {
            reporter.error("Tuples are not supported.")
          }

          // requiredOutputRanges += (fnc.id -> ranges.map( x =>
          //  (x._1 -> PartialInterval(x._2._1, x._2._2) )))
          // requiredOutputErrors += (fnc.id -> errors)



        case _ =>
          // reporter.info("No post-condition found\n")

      }

    }

    timer.stop
    // reporter.info("Finished specs preprocessing phase\n")

    reporter.debug("range bounds: " + resRanges.mkString("\n"))
    reporter.debug("error bounds: " + resErrors.mkString("\n"))

    (ctx.copy(specInputRanges = allRanges, specInputErrors = allErrors,
      specResultRangeBounds = resRanges, specResultErrorBounds = resErrors,
      specMixedPrecisions = precisionMap.mapValues(_._1),
      specInferredReturnTypes = precisionMap.mapValues(_._2)), prg)
  }

  def extractPreCondition(expr: Expr): (Map[Identifier, (Rational, Rational)],
    Map[Identifier, Rational]) = {

    val (lowerBound, upperBound, absError) = extractCondition(expr)

    (lowerBound.map({
      case (x, r) => (x, (r, upperBound(x))) // assumes that it exists
      }),
      absError)
  }

  def extractPostCondition(expr: Expr): (Map[Identifier, (Option[Rational], Option[Rational])],
    Map[Identifier, Rational]) = {

    val (lowerBound, upperBound, absError) = extractCondition(expr)

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
    Map[Identifier, Rational], Map[Identifier, Rational]) = {

    var lowerBound: Map[Identifier, Rational] = Map.empty
    var upperBound: Map[Identifier, Rational] = Map.empty
    var absError: Map[Identifier, Rational] = Map.empty

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
        // reporter.warning(s"Unexpected expression in spec: $e.")
    }

    extract(e)

    (lowerBound, upperBound, absError)
  }

  def buildIdentifierMap(prg: Program): (Map[String, Identifier], Map[String, Map[String, Identifier]]) = {
    def buildIdentifierMapFunction(f: FunDef): Map[String, Identifier] = {
      allVariablesOf(f.body.get).map(id => ((id.toString) -> id)).toMap
    }

    (prg.defs.map(fnc => (fnc.id.name -> fnc.id)).toMap,
      prg.defs.map(fnc => (fnc.id.name -> buildIdentifierMapFunction(fnc))).toMap)
  }


  private def parseMixedPrecisionFile(f: String, fncIdMap: Map[String, Identifier],
    varIdMap: Map[String, Map[String, Identifier]]): Map[Identifier, Map[Identifier, Precision]] = {

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
    defaultPrec: Precision): (Map[Identifier, Precision], Precision) = {

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
  def float128: Parser[Precision] = "DoubleDouble" ^^ { case _ => DoubleDouble }
  def float64: Parser[Precision] = "Double" ^^ { case _ => Float64 }
  def float32: Parser[Precision] = "Float" ^^ { case _ => Float32 }

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