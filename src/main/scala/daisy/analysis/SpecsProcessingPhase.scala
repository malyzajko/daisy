/* Copyright 2015 MPI-SWS, Saarbruecken */


package daisy
package analysis

import lang.Trees._
import utils.{Interval, PartialInterval, Rational}
import lang.Identifiers._
import lang.TreeOps.preTraversal
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
object SpecsProcessingPhase extends DaisyPhase {

  override val name = "Specs processing"
  override val description = "Processes the specifications for later phases."
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  def run(ctx:Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info("\nStarting specs preprocessing phase")
    val timer = ctx.timers.preprocess.start

    var allRanges: Map[Identifier, Map[Identifier, Interval]] = Map()
    var allErrors: Map[Identifier, Map[Identifier, Rational]] = Map()
    var resRanges: Map[Identifier, PartialInterval] = Map()
    var resErrors: Map[Identifier, Rational] = Map()
    //var requiredOutputRanges: Map[Identifier, Map[Identifier, PartialInterval]] = Map()
    //var requiredOutputErrors: Map[Identifier, Map[Identifier, Rational]] = Map()

    for (fnc <- prg.defs) {

      fnc.precondition match {
        case Some(pre) =>
          // TODO: additional constraints
          val (ranges, errors) = extractPreCondition(pre)

          allRanges += (fnc.id -> ranges.map( x => (x._1 -> Interval(x._2._1, x._2._2) )))
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

          //requiredOutputRanges += (fnc.id -> ranges.map( x =>
          //  (x._1 -> PartialInterval(x._2._1, x._2._2) )))
          //requiredOutputErrors += (fnc.id -> errors)



        case _ =>
          //reporter.info("No post-condition found\n")

      }

    }

    timer.stop
    reporter.info("Finished specs preprocessing phase\n")

    reporter.debug(lang.RangePrinter(prg))
    reporter.debug("range bounds: " + resRanges.mkString("\n"))
    reporter.debug("error bounds: " + resErrors.mkString("\n"))

    (ctx.copy(specInputRanges = allRanges, specInputErrors = allErrors,
      specResultRangeBounds = resRanges, specResultErrorBounds = resErrors), prg)
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
                                  Map[Identifier, Rational],
                                  Map[Identifier, Rational]) = {

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
        //reporter.warning(s"Unexpected expression in spec: $e.")
    }

    extract(e)

    (lowerBound, upperBound, absError)
  }





}