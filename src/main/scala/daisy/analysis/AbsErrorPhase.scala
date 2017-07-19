package daisy
package analysis

import utils.FinitePrecision._
import lang.Trees._
import lang.NumAnnotation
import lang.Identifiers._
import utils.{AffineForm, Interval, Rational}
import utils.Interval._
import Rational._

/**
  This phase computes roundoff errors for a given precision and data type
  and attaches the information to the trees.

  TODO: make parametric in data type

  Note:
    - if we want to use separation of errors, then the range computation needs
    to compute the actual (but real-valued) ranges, as
    |f(\tl{x} - \tl{f}(\tl{x}) | is over \tl{x} and hence the actual range.
    - For now, we use affine arithmetic to track initial as well as roundoff
    errors. Via options one can switch off tracking of either.
    - if we are tracking initial errors, and one is present, do NOT add an
    additional roundoff error

  Prerequisites:
    - SpecsProcessingPhase
    - RangePhase (check you are computing the correct ranges)
 */
object AbsErrorPhase extends DaisyPhase {

  override val name = "roundoff phase"
  override val description = "Computes roundoff errors"
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set(
    FlagOptionDef("noInitialErrors", "do not track initial errors specified by user"),
    FlagOptionDef("noRoundoff", "do not track roundoff errors")
    )

  implicit val debugSection = DebugSectionAnalysis

  var reporter: Reporter = null

  override def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.roundoff.start

    var trackInitialErrs = true
    var trackRoundoffErrs = true

    /* Process relevant options */
    for (opt <- ctx.options) opt match {
      case FlagOption("noInitialErrors") => trackInitialErrs = false
      case FlagOption("noRoundoff") => trackRoundoffErrs = false
      case _ => ;
    }

    for (fnc <- prg.defs) if (!fnc.precondition.isEmpty && !fnc.body.isEmpty){

      computeAbsError(fnc.body.get, Map.empty, trackInitialErrs, trackRoundoffErrs)

    }

    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, prg)
  }


  def computeAbsError(expr: Expr, _valMap: Map[Identifier, AffineForm],
    trackInitialErrs: Boolean, trackRoundoffErrs: Boolean): Unit = {

    var valMap = _valMap

    def evalRoundoff(e: Expr): AffineForm = e match {

      // if x has been evaluated before (via let, or simply inside the expression)
      // we need to cache the latter ones, or else we are loosing all correlations.
      case x @ Variable(id) if (valMap.isDefinedAt(id)) =>
        val aform = valMap(id)
        if (!x.hasError) {  // happens for roundoff only, i.e. when no error is given by precondition
          x.absError = maxAbs(aform.toInterval)
        }
        aform

      // if x has an interval, then it's a fnc parameter
      case x @ Variable(id) if x.hasInterval =>
        val aform = if (trackInitialErrs && x.hasError) {
          // error is already attached to x, do not add roundoff on top
          AffineForm.fromError(x.absError)

        } else if (trackRoundoffErrs) {
          // new error based on interval and data type/precision
          val rndoff = Float64.absRoundoff(x.interval)
          x.absError = rndoff
          AffineForm.fromError(rndoff)

        } else {
          // do nothing to x (which may get us inconsistent state,
          // if x has already attached error )
          if (x.hasError) reporter.warning("Node has errors attached," +
            "but you are asking me to ignore them.")
          else {
            x.absError = zero // this is just for consistency
          }
          AffineForm.zero
        }
        valMap += (id -> aform)
        aform

      // the recording can potentially be done here too, but it's not clear
      // whether the cost of checking won't offset this...
      case x @ Plus(lhs, rhs) =>
        // propagated error
        val aform: AffineForm = evalRoundoff(lhs) + evalRoundoff(rhs)

        val actualRange: Interval = x.interval + aform.toInterval
        val newError: AffineForm = if (trackRoundoffErrs) {
          val rndoff: Rational =  Float64.absRoundoff(actualRange)
          aform :+ rndoff
        } else {
          aform
        }
        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ RealLiteral(r) =>
        if (isExactInFloats(r, Float64)) {
          AffineForm.zero
        } else {
          AffineForm.fromError(Float64.absRoundoff(r))
        }

      case x @ Minus(lhs, rhs) =>
        // propagated error
        val aform: AffineForm = evalRoundoff(lhs) - evalRoundoff(rhs)

        val actualRange: Interval = x.interval + aform.toInterval
        val newError: AffineForm = if (trackRoundoffErrs) {
          val rndoff: Rational = Float64.absRoundoff(actualRange)
          aform :+ rndoff
        } else {
          aform
        }
        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ Times(lhs, rhs) =>
        // propagated error
        val lAA = AffineForm(lhs.asInstanceOf[NumAnnotation].interval)
        val rAA = AffineForm(rhs.asInstanceOf[NumAnnotation].interval)
        val lErr = evalRoundoff(lhs)
        val rErr = evalRoundoff(rhs)

        var aform: AffineForm = lAA*rErr + rAA*lErr + lErr*rErr

        // new error
        val actualRange: Interval = x.interval + aform.toInterval
        val newError: AffineForm = if (trackRoundoffErrs) {
          val rndoff: Rational = Float64.absRoundoff(actualRange)
          aform :+ rndoff
        } else {
          aform
        }
        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ Division(lhs, rhs) =>
        // propagated error

        // we will not call the SMT solver here. This is conservative
        // and probably does not make a big difference for the accuracy of the analysis
        val rhsInterval = rhs.asInstanceOf[NumAnnotation].interval
        val rErr = evalRoundoff(rhs)  //yErr

        val rInt = rhsInterval + rErr.toInterval // the actual interval, incl errors
        val a = min(abs(rInt.xlo), abs(rInt.xhi))
        val errorMultiplier = -one / (a*a)


        val invErr = rErr * AffineForm(errorMultiplier)

        val lAA = AffineForm(lhs.asInstanceOf[NumAnnotation].interval)

        val inverse: Interval = rhsInterval.inverse

        val kAA = AffineForm(inverse)

        val lErr = evalRoundoff(lhs)  //xErr

        // multiplication with inverse
        var aform = lAA*invErr + kAA*lErr + lErr*invErr


        // new error
        val actualRange: Interval = x.interval + aform.toInterval
        val newError: AffineForm = if (trackRoundoffErrs) {
          val rndoff: Rational = Float64.absRoundoff(actualRange)
          aform :+ rndoff
        } else {
          aform
        }
        x.absError = maxAbs(newError.toInterval)
        newError

      case x @ UMinus(t) =>
        val newError = -evalRoundoff(t)   // no additional roundoff error
        x.absError = maxAbs(newError.toInterval)  // PERF: skip the recomputation
        newError

      case x @ Sqrt(t) =>

        // TODO: Not supported for fixed-points, add exception
        //case FPPrecision(_) => throw UnsupportedRealFragmentException("Sqrt not supported for fixed-points.")

        val tInterval = t.asInstanceOf[NumAnnotation].interval
        val tError = evalRoundoff(t)

        // propagated existing errors
        val a = min(abs(tInterval.xlo), abs(tInterval.xhi))
        val errorMultiplier = Rational(1L, 2L) / sqrtDown(a)
        val propErr = tError * AffineForm(errorMultiplier)

        // new roundoff
        val actualRange: Interval = x.interval + propErr.toInterval
        val newError: AffineForm = if (trackRoundoffErrs) {
          val rndoff: Rational = Float64.absRoundoff(actualRange)
          propErr :+ rndoff
        } else {
          propErr
        }
        x.absError = maxAbs(newError.toInterval)
        newError


      case Let(id, value, body) =>
        // here one could do an error split, and use error propagation

        val aform = evalRoundoff(value)
        valMap += (id -> aform)
        evalRoundoff(body)

    }
    evalRoundoff(expr)

  }

}

