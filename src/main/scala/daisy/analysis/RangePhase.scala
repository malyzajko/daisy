
package daisy
package analysis

import lang.Identifiers._
import lang.Trees._
import utils.Interval
import utils.AffineForm
import utils.SMTRange


/**
  This phase annotates every numerical expression with an interval
  of the real values the expression can take, according to the contract.
  Several options for how to perform this analysis are available:
  - classic intervals with rational bounds
  - classic affine arithmetic over rationals

  Note:
    - assumes the input variables have ranges annotated already, or else
      have already been assigned
    - does not support sqrt (for now)
    - assumes the body is only an arithmetic expression with let stmts,
      but without if-then-else, loops, etc.
    - uses the same method for all expressions


  Prerequisite:
    - SpecsProcessingPhase
*/
object RangePhase extends DaisyPhase {

  override val name = "Range phase"
  override val description = "Computes the ranges of intermediate expressions."
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    ChoiceOptionDef("rangeMethod", "Method for range analysis.",
    Set("affine", "interval", "smt", "subdiv") ,"interval")
    )

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  override def run(ctx:Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info("\nStarting range phase")
    val timer = ctx.timers.ranges.start

    // default
    var rangeMethod: String = "interval"

    // Process relevant options.
    for (opt <- ctx.options) opt match {
      case ChoiceOption("rangeMethod", s) => s match {
        case "interval" | "affine" | "smt" | "subdiv" =>
          rangeMethod = s
          reporter.info(s"using $s")
        case _ =>
          reporter.warning(s"Unknown range method: $s, choosing default (interval)!")
      }
      case _ =>
    }

    for (fnc <- prg.defs) if (!fnc.precondition.isEmpty && !fnc.body.isEmpty){
      rangeMethod match {
        case "interval" =>
          evaluateInterval(fnc.body.get, Map.empty)
        case "affine" =>
          evaluateAffine(fnc.body.get, Map.empty)
        case "smt" =>
          evaluateSMT(fnc.body.get, Map.empty)
        case "subdiv" =>
          evaluateSubdiv(fnc.body.get, ctx.specInputRanges(fnc.id), Map.empty)

      }

    }

    timer.stop
    ctx.reporter.info("Finished range phase\n")

    reporter.debug(lang.RangePrinter(prg))

    (ctx, prg)
  }

  def evaluateSubdiv(expr: Expr, inputRanges: Map[Identifier, Interval], _valMap: collection.immutable.Map[Identifier, AffineForm] = Map.empty): Unit = {
    val numSplits = 10

    //val inputRanges: Map[Identifier, Interval] = context.inputRanges(fncId)

    val inputsSubdiv: Seq[Map[Identifier, Interval]] =  inputRanges.foldLeft(Seq(Map[Identifier, Interval]()))({
      case (currSeq: Seq[Map[Identifier, Interval]], (id, intrvl)) =>
        val xlo = intrvl.xlo
        val splitWidth = intrvl.width / numSplits
        val splits: Seq[Interval] = (0 until numSplits).map(i =>
          Interval(xlo + i * splitWidth, xlo + (i+1) * splitWidth))

        currSeq.flatMap( m =>
            splits.map(i => m + (id -> i))
        )
    })

    // map of maximum seen ranges of all intermediate expressions
    var currentRanges: Map[Expr, Interval] = Map()


    def evalInterval(e: Expr, valMap: Map[Identifier, Interval]): Interval = (e: @unchecked) match {

      case x @ Variable(id) =>
        valMap(id)

      case x @ RealLiteral(r) =>
        Interval(r)

      case x @ Plus(lhs, rhs) =>
        val intrvl = evalInterval(lhs, valMap) + evalInterval(rhs, valMap)
        currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
        intrvl

      case x @ Minus(lhs, rhs) =>
        val intrvl = evalInterval(lhs, valMap) - evalInterval(rhs, valMap)
        currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
        intrvl

      case x @ Times(lhs, rhs) =>
        val intrvl = evalInterval(lhs, valMap) * evalInterval(rhs, valMap)
        currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
        intrvl

      case x @ Division(lhs, rhs) =>
        try {
          val intrvl = evalInterval(lhs, valMap) / evalInterval(rhs, valMap)
          currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
          intrvl
        } catch {
          case e: utils.DivisionByZeroException =>
            reporter.fatalError(x.getPos, "possible division by zero")
        }

      case x @ UMinus(t) =>
        val intrvl = - evalInterval(t, valMap)
        currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
        intrvl

      case x @ Sqrt(t) =>
        try {
          val intrvl = evalInterval(t, valMap).squareRoot
          currentRanges = currentRanges + (x -> currentRanges(x).union(intrvl))
          intrvl
        } catch {
          case e: utils.NegativeSqrtException =>
            reporter.fatalError(x.getPos, "possible negative square root")
        }

      case x @ Let(id, value, body) =>
        val intrvl = evalInterval(value, valMap)
        currentRanges = currentRanges + (Variable(id) -> currentRanges(Variable(id)).union(intrvl))
        val newValMap = valMap + (id -> intrvl)
        evalInterval(body, newValMap)
    }

    // assumes an empty currentRanges Map and simply adds the first interval
    // instead of union-ing
    def evalIntervalFirst(e: Expr, valMap: Map[Identifier, Interval]): Interval = (e: @unchecked) match {

      case x @ Variable(id) =>
        valMap(id)

      case x @ RealLiteral(r) =>
        Interval(r)

      case x @ Plus(lhs, rhs) =>
        val intrvl = evalIntervalFirst(lhs, valMap) + evalIntervalFirst(rhs, valMap)
        currentRanges = currentRanges + (x -> intrvl)
        intrvl

      case x @ Minus(lhs, rhs) =>
        val intrvl = evalIntervalFirst(lhs, valMap) - evalIntervalFirst(rhs, valMap)
        currentRanges = currentRanges + (x -> intrvl)
        intrvl

      case x @ Times(lhs, rhs) =>
        val intrvl = evalIntervalFirst(lhs, valMap) * evalIntervalFirst(rhs, valMap)
        currentRanges = currentRanges + (x -> intrvl)
        intrvl

      case x @ Division(lhs, rhs) =>
        try {
          val intrvl = evalIntervalFirst(lhs, valMap) / evalIntervalFirst(rhs, valMap)
          currentRanges = currentRanges + (x -> intrvl)
          intrvl
        } catch {
          case e: utils.DivisionByZeroException =>
            reporter.fatalError(x.getPos, "possible division by zero")
        }

      case x @ UMinus(t) =>
        val intrvl = - evalIntervalFirst(t, valMap)
        currentRanges = currentRanges + (x -> intrvl)
        intrvl

      case x @ Sqrt(t) =>
        try {
          val intrvl = evalIntervalFirst(t, valMap).squareRoot
          currentRanges = currentRanges + (x -> intrvl)
          intrvl
        } catch {
          case e: utils.NegativeSqrtException =>
            reporter.fatalError(x.getPos, "possible negative square root")
        }

      case x @ Let(id, value, body) =>
        val intrvl = evalIntervalFirst(value, valMap)
        currentRanges = currentRanges + (Variable(id) -> intrvl)
        val newValMap = valMap + (id -> intrvl)
        evalIntervalFirst(body, newValMap)
    }

    // there is probably a helper function for this
    def attachInterval(e: Expr): Unit = (e: @unchecked) match {

      case x @ Variable(id) =>
        if (!x.hasInterval) {
          x.interval = currentRanges(x)
        }
      return

      case x @ RealLiteral(r) =>
        x.interval = Interval(r)
        return

      case x @ Plus(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ Minus(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ Times(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ Division(lhs, rhs) =>
        attachInterval(lhs)
        attachInterval(rhs)
        x.interval = currentRanges(x)

      case x @ UMinus(t) =>
        attachInterval(t)
        x.interval = currentRanges(x)

      case x @ Sqrt(t) =>
        attachInterval(t)
        x.interval = currentRanges(x)

      // TODO: we may be missing intervals on the let-defined vars
      case x @ Let(id, value, body) =>
        attachInterval(value)
        attachInterval(body)
    }

    // collect possible intervals
    evalIntervalFirst(expr, inputsSubdiv.head)
    inputsSubdiv.tail.foreach( x => evalInterval(expr, x))

    // traverse the tree once more and attach the correct ranges
    attachInterval(expr)
  }

  /*
    Performs the analysis in affine arithmetic, but saves the interval.
  */
  def evaluateAffine(expr: Expr, _valMap: collection.immutable.Map[Identifier, AffineForm] = Map.empty): Unit = {

    var valMap = _valMap

    def evalAffine(e: Expr): AffineForm = (e: @unchecked) match {

      case x @ Variable(id) if x.hasInterval =>
        AffineForm(x.interval)

      case x @ Variable(id) =>
        val aform = valMap(id)
        x.interval = aform.toInterval
        aform

      case x @ RealLiteral(r) =>
        val aform = AffineForm(r)
        x.interval = aform.toInterval
        aform

      case x @ Plus(lhs, rhs) =>
        val aform = evalAffine(lhs) + evalAffine(rhs)
        x.interval = aform.toInterval
        aform

      case x @ Minus(lhs, rhs) =>
        val aform = evalAffine(lhs) - evalAffine(rhs)
        x.interval = aform.toInterval
        aform

      case x @ Times(lhs, rhs) =>
        val aform = evalAffine(lhs) * evalAffine(rhs)
        x.interval = aform.toInterval
        aform

      case x @ Division(lhs, rhs) =>
        try {
          val aform = evalAffine(lhs) / evalAffine(rhs)
          x.interval = aform.toInterval
          aform
        } catch {
          case e: utils.DivisionByZeroException =>
            reporter.fatalError(x.getPos, "possible division by zero")
        }

      case x @ UMinus(t) =>
        val aform = - evalAffine(t)
        x.interval = aform.toInterval
        aform

      case x @ Sqrt(t) =>
        try {
          val aform = evalAffine(t).squareRoot
          x.interval = aform.toInterval
          aform
        } catch {
          case e: utils.NegativeSqrtException =>
            reporter.fatalError(x.getPos, "possible negative square root")
        }

      case Let(id, value, body) =>
        val aform = evalAffine(value)
        valMap += (id -> aform)
        evalAffine(body)
    }

    evalAffine(expr)

  }


  /*
    Performs the analysis in interval arithmetic.
   */
  def evaluateInterval(expr: Expr, _intMap: collection.immutable.Map[Identifier, Interval] = Map.empty): Unit = {

    var intMap = _intMap

    def evalInterval(e: Expr): Interval = (e: @unchecked) match {

      case x @ Variable(id) if x.hasInterval =>
        x.interval

      case x @ Variable(id) =>
        val intrvl = intMap(id)
        x.interval = intrvl
        intrvl

      case x @ RealLiteral(r) =>
        x.interval = Interval(r)
        x.interval

      case x @ Plus(lhs, rhs) =>
        val intrvl = evalInterval(lhs) + evalInterval(rhs)
        x.interval = intrvl
        intrvl

      case x @ Minus(lhs, rhs) =>
        val intrvl = evalInterval(lhs) - evalInterval(rhs)
        x.interval = intrvl
        intrvl

      case x @ Times(lhs, rhs) =>
        val intrvl = evalInterval(lhs) * evalInterval(rhs)
        x.interval = intrvl
        intrvl

      case x @ Division(lhs, rhs) =>
        try {
          val intrvl = evalInterval(lhs) / evalInterval(rhs)
          x.interval = intrvl
          intrvl
        } catch {
          case e: utils.DivisionByZeroException =>
            reporter.fatalError(x.getPos, "possible division by zero")
        }

      case x @ UMinus(t) =>
        val intrvl = - evalInterval(t)
        x.interval = intrvl
        intrvl

      case x @ Sqrt(t) =>
        try {
          val intrvl = evalInterval(t).squareRoot
          x.interval = intrvl
          intrvl
        } catch {
          case e: utils.NegativeSqrtException =>
            reporter.fatalError(x.getPos, "possible negative square root")
        }

      case Let(id, value, body) =>
        val intrvl = evalInterval(value)
        intMap += (id -> intrvl)
        evalInterval(body)
    }
    evalInterval(expr)


  }


  /*
    Performs the analysis with the help of an SMT solver, and saves
    the resulting interval in the trees.
    TODO: additional constraints
   */
  def evaluateSMT(expr: Expr, _intMap: collection.immutable.Map[Identifier, SMTRange] = Map.empty): Unit = {

    var intMap = _intMap

    def evalSMT(e: Expr): SMTRange = e match {

      case x @ Variable(id) if x.hasInterval =>
        SMTRange(x, x.interval)

      case x @ Variable(id) =>
        val smtRange = intMap(id)
        x.interval = smtRange.toInterval
        smtRange

      case x @ RealLiteral(r) =>
        val smtRange = SMTRange(r)
        x.interval = smtRange.toInterval
        smtRange

      case x @ Plus(lhs, rhs) =>
        val smtRange = evalSMT(lhs) + evalSMT(rhs)
        x.interval = smtRange.toInterval
        smtRange

      case x @ Minus(lhs, rhs) =>
        val smtRange = evalSMT(lhs) - evalSMT(rhs)
        x.interval = smtRange.toInterval
        smtRange

      case x @ Times(lhs, rhs) =>
        val smtRange = evalSMT(lhs) * evalSMT(rhs)
        x.interval = smtRange.toInterval
        smtRange

      case x @ Division(lhs, rhs) =>
        try {
          val smtRange = evalSMT(lhs) / evalSMT(rhs)
          x.interval = smtRange.toInterval
          smtRange
        } catch {
          case e: utils.DivisionByZeroException =>
            reporter.fatalError(x.getPos, "possible division by zero")
        }

      case x @ UMinus(t) =>
        val smtRange = - evalSMT(t)
        x.interval = smtRange.toInterval
        smtRange

      case x @ Sqrt(t) =>
        try {
          val smtRange = evalSMT(t).squareRoot
          x.interval = smtRange.toInterval
          smtRange
        } catch {
          case e: utils.NegativeSqrtException =>
            reporter.fatalError(x.getPos, "possible negative square root")
        }

      case Let(id, value, body) =>
        val smtRange = evalSMT(value)
        intMap += (id -> smtRange)
        evalSMT(body)
    }
    evalSMT(expr)

  }
}