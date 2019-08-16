// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package opt

import java.io.{BufferedWriter, File, FileWriter}
import java.util.concurrent.TimeUnit

import daisy.frontend.CASTExtractor
import daisy.lang.Extractors._
import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.Trees._
import daisy.lang.Types.{FinitePrecisionType, RealType, TypeTree}
import daisy.tools.FinitePrecision.{FixedPrecision, Precision}
import daisy.tools._

import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, TimeoutException}
import scala.language.postfixOps
import scala.sys.process._

/**
  Replaces transcendental functions with approximation polynomials

  Prerequisites:
    - SpecsProcessingPhase
    - DataflowPhase
 */
object ApproxPhase extends DaisyPhase with MetalibmUtils with CASTExtractor with opt.CostFunctions {
  override val name = "Approximation Generation"
  override val shortName = "approximation"
  override val description = "Replaces transcendental functions with their approximations"

  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    FlagOption(
      "no-refinement",
      "Disable refinement loop for local error budget distribution."),
    FlagOption(
      "polyUniform",
      "Use uniform precision for polynomials."),
    FlagOption(
      "polyMixed",
      "Use mixed precision for polynomials.")
  )

  type TypeConfig = Map[Identifier, Precision]
  type RangeMap = Map[(Expr, Seq[Expr]), Interval]

  implicit val debugSection: DebugSection = DebugSectionOptimization

  override def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    val (updatedFncs, approxContextUpdates, seqMapApprox) = prg.defs.map({ fnc => {

      val (funFP, ctxFP) = if (fnc.returnType == RealType && fnc.body.isDefined) {
        // TODO: Assign precision in Dataflow Phase and remove this code
        val uniformPrecision = ctx.uniformPrecisions(fnc.id)
        val (updFnc, newRangeMap) = applyFinitePrecisionToFunDef(fnc, Map().withDefaultValue(uniformPrecision), ctx.intermediateRanges(fnc.id))
        (updFnc,
          ctx.copy(intermediateRanges = ctx.intermediateRanges + (fnc.id -> newRangeMap)))
      } else {
        (fnc, ctx)
      }

      findReplace(funFP, ctxFP)
    }}).unzip3

    // TODO: better way to enforce immutable.Seq
    val extDefs = Seq() ++ approxContextUpdates.flatten.toMap.values.flatten.map(_.approx).toSeq ++ updatedFncs
    val extPrg = Program(prg.id, extDefs)

    val mapApprox = seqMapApprox.flatten.toMap
    val tmpCtx = updateContextWithApprox(ctx, prg.defs, approxContextUpdates.flatten.toMap, mapApprox)
    val cctx = approxContextUpdates.flatten.toMap.values.flatten.foldLeft(tmpCtx)({case (acc, x) => x.applyToContext(acc)})
    (cctx, extPrg)
  }

  def updateContextWithApprox(ctx: Context, funDefs: Seq[FunDef], updates: Map[Identifier, Seq[ContextUpdate]], mapApprox: Map[(Expr, PathCond), ApproxPoly]): Context = {
    val updIntermRanges = ctx.intermediateRanges.map(fnc => {
      if (updates.contains(fnc._1)) {
        val updList = updates(fnc._1)
        fnc._1 -> updList.foldLeft(fnc._2)((accmap, upd) => accmap.map({ x =>
          val ((expr, path), v) = x

          def replace(e: Expr, p: PathCond): Expr = {
            (e, p) match {
            case (node@ElemFnc(_), path) if mapApprox.contains(node, path) && mapApprox(node, path) == upd.newNode => upd.newNode // plug in Approx node
            case (node@ApproxPoly(_,_,_,_), path) if mapApprox.contains(node.original, path) && mapApprox(node.original, path) == upd.newNode => upd.newNode // plug in Approx node
            case (node@Cast(x@ElemFnc(_),_), path) if mapApprox.contains(x, path) && mapApprox(x, path) == upd.newNode => upd.newNode
            case (node@Cast(x@ApproxPoly(_,_,_,_),_), path) if mapApprox.contains(x, path) && mapApprox(x, path) == upd.newNode => upd.newNode
            case (z@ArithOperator(args, recons), path) =>
              val es = args.map(replace(_, path))
              recons(es)
            case (z@Let(id, value, body), path) => Let(id, replace(value, path), replace(body, path))
            case z => z._1
          }}

          val newFnc = replace(expr, path)
          val newV = if (newFnc == expr) v else upd.resultRealRange
          (newFnc, path) -> newV
        }))
      }

      else fnc
    })
    val updIntermAbsErrors = ctx.intermediateAbsErrors.map(fnc => {
      if (updates.contains(fnc._1)) {
        val updList = updates(fnc._1)
        fnc._1 -> updList.foldLeft(fnc._2)((accmap, upd) => accmap.map({ x =>
          val ((expr, path), v) = x

          def replace(e: Expr, p: PathCond): Expr = (e, p) match {
            case (node@ApproxPoly(_,_,_,_), path) if mapApprox.contains(node.original, path) && mapApprox(node.original, path) == upd.newNode => upd.newNode // plug in Approx node
            case (z@ApproxPoly(_,_,_,_), _) => z
            case (z@ArithOperator(args, recons), path) =>
              val es = args.map(replace(_, path))
              recons(es)
            case (z@Let(id, value, body), path) => Let(id, replace(value, path), replace(body, path))
            case z => z._1
          }

          val newFnc = replace(expr, path)
          val newV = if (newFnc == expr) v else upd.resultAbsErrors
          (newFnc, path) -> newV
        }))
      }
      else fnc
    })
      ctx.copy(intermediateAbsErrors = updIntermAbsErrors, intermediateRanges = updIntermRanges)
  }

  def findReplace(fnc: FunDef, context: Context): (FunDef, Map[Identifier, Seq[ContextUpdate]], Map[(Expr, PathCond), ApproxPoly]) = {
    val expr = fnc.body.get

    /**
     * returns updated tree (elementary fncs are replaced by function calls)
     * and approximations as separate functions
     * @param e expression from the original program
     * @param p path to expressions inside branches, consists of conditionals checked in if statement
     * @return triple of:
      *        1) function body with elementary fnc calls replaced by [[ApproxPoly]] nodes
      *        2) new function definitions for polynomial approximations generated by Metalibm
      *        3) updated context
     */
    def find(e: Expr, p: PathCond, ctx: Context, mapDuplicatesApprox: Map[(String, Interval, TypeTree), ApproxPoly], mapApprox: Map[(Expr, PathCond), ApproxPoly], ctxUpdates: Map[Identifier, Seq[ContextUpdate]]): (Expr, Map[Identifier, Seq[ContextUpdate]], Map[(String, Interval, TypeTree), ApproxPoly], Map[(Expr, PathCond), ApproxPoly]) = (e, p) match {
      case x @ (RealLiteral(_), _ ) => (x._1, ctxUpdates, mapDuplicatesApprox, mapApprox)
      case x @ (FinitePrecisionLiteral(_, _, _), _) => (x._1, ctxUpdates, mapDuplicatesApprox, mapApprox)
      case x @ (Variable(_), _) => (x._1, ctxUpdates, mapDuplicatesApprox, mapApprox)

      case x @ (Let(id, value, body), path) =>
        val (updVal, ctxUpdatesV, updDuplMapApprox, updMapApprox) = find(value, path, ctx, mapDuplicatesApprox, mapApprox, ctxUpdates)
        val (updBody, ctxUpdatesB, updDuplMapApprox2, updMapApprox2) = find(body, path, ctx, updDuplMapApprox, updMapApprox, ctxUpdatesV)
        (Let(id, updVal, updBody), ctxUpdatesB, updDuplMapApprox2, updMapApprox2)

      // Elementary functions
      case x @ (castOfElem @ Cast(elemFnc @ ElemFnc(arg), tpe), path) =>
        ctx.reporter.result(s"approximating fnc: ${fnc.id} subexpression $elemFnc")
        val argRange = ctx.intermediateRanges(fnc.id)(arg, path)

        if (mapDuplicatesApprox.contains(getElemFncName(elemFnc), argRange, tpe)) {
          val approx = mapDuplicatesApprox(getElemFncName(elemFnc), argRange, tpe)
          (approx, ctxUpdates, mapDuplicatesApprox, mapApprox + ((elemFnc, path) -> approx)) // replace current node with Approx from mapDuplicatesApprox
        } else {
          val errBudget = getLocalErrorBudget(tpe, ctx.intermediateRanges(fnc.id)(x), ctx.uniformPrecisions.getOrElse(fnc.id, null)) // compute from assigned precision
          ctx.reporter.result(s"Function call $elemFnc. Assigned precision $tpe")
          val res = transformElementaryFunction(errBudget, ctx, castOfElem, path, arg)
          val updContexts = if (ctxUpdates.contains(fnc.id)) ctxUpdates + (fnc.id -> (ctxUpdates(fnc.id) :+ res._2)) else Map(fnc.id -> Seq(res._2))
          (res._1, updContexts, mapDuplicatesApprox + ((getElemFncName(elemFnc), argRange, tpe) -> res._1), mapApprox + ((elemFnc, path) -> res._1))
        }

      case x @ (elemFnc @ ElemFnc(arg), path) =>
        ctx.reporter.result(s"approximating fnc: ${fnc.id} subexpression $elemFnc")

        /* ------ Local Error Budgeting ------ */
        val elemFncType = elemFnc.getType
        val argRange = ctx.intermediateRanges(fnc.id)(arg, path)
        // TODO: also check for duplicates inside refinement loop
        if (mapDuplicatesApprox.contains(getElemFncName(elemFnc), argRange, elemFncType)) {
          val approx = mapDuplicatesApprox(getElemFncName(elemFnc), argRange, elemFncType)
          (approx, ctxUpdates, mapDuplicatesApprox, mapApprox + ((elemFnc, path) -> approx)) // replace current node with Approx from mapDuplicatesApprox
        }
        else {
          val errBudget = getLocalErrorBudget(elemFncType, ctx.intermediateRanges(fnc.id)(elemFnc, path), ctx.uniformPrecisions.getOrElse(fnc.id, null)) // compute from assigned precision
          ctx.reporter.result(s"Function call $elemFnc. Assigned precision ${elemFnc.getType}")
          val res = transformElementaryFunction(errBudget, ctx, elemFnc, path, arg)

          val updContexts = if (ctxUpdates.contains(fnc.id)) ctxUpdates + (fnc.id -> (ctxUpdates(fnc.id) :+ res._2)) else Map(fnc.id -> Seq(res._2))
          (res._1, updContexts, mapDuplicatesApprox + ((getElemFncName(elemFnc), argRange, elemFncType) -> res._1), mapApprox + ((elemFnc, path) -> res._1))
        }

      case x @ (ArithOperator(es_old, recons), path) =>
        val first = find(es_old.head, path, ctx, mapDuplicatesApprox, mapApprox, ctxUpdates)
        val init = (Seq(first._1), first._2, first._3, first._4)
        val (es, ctxNewUpdates, updDuplMapApprox, updMapsApprox) = es_old.drop(1).foldLeft(init)((acc, arg) => {
          val tmp = find(arg, path, ctx, acc._3, acc._4, acc._2)
          val ctxUpdMap = if (acc._2.contains(fnc.id)) acc._2 + (fnc.id -> (acc._2(fnc.id) ++ tmp._2(fnc.id))) else tmp._2
          (acc._1 :+ tmp._1, ctxUpdMap.mapValues(_.distinct), acc._3 ++ tmp._3, acc._4 ++ tmp._4)
        })
        (recons(es), ctxNewUpdates, updDuplMapApprox, updMapsApprox)

      case x @ (Cast(t, tpe), path) =>
        val (updT, ctxT, updDuplMapApprox, updMapApprox) = find(t, path, ctx, mapDuplicatesApprox, mapApprox, ctxUpdates)
        (Cast(updT, tpe), ctxT, updDuplMapApprox, updMapApprox)

      case x => throw new Exception(s"Unknown expression $x")
    }


    def transformElementaryFunction(errBudget: Rational, ctx: Context, transFnc: Expr, path: PathCond, arg: Expr): (ApproxPoly, ContextUpdate) = {
      val MAX_STEPS = 5 // max number of steps in each direction

      val intermRanges = ctx.intermediateRanges(fnc.id)
      val x = (transFnc, path)
      val elemFncName = getElemFncName(transFnc)
      val elemFncDomain = ctx.intermediateRanges(fnc.id)(arg, path)

      var precisionAssignmentPipeline: Pipeline[Program, Program] =
        transform.TACTransformerPhase >>
          transform.ConstantTransformerPhase

      // choose precision assignment method for polynomials
      val uniform = (!ctx.hasFlag("mixed-tuning") && !ctx.hasFlag("polyMixed")) || ctx.hasFlag("polyUniform")
      if (uniform)
        precisionAssignmentPipeline >>= analysis.DataflowPhase
      else {
        precisionAssignmentPipeline >>= analysis.RangePhase >>
          opt.MixedPrecisionOptimizationPhase >> analysis.AbsErrorPhase
      }

      def feedbackLoop(oldCost: Option[Rational], oldRoundoff: Option[Rational], i: Int, op: Direction[Rational], previous: Option[(ApproxPoly, ContextUpdate)], firstDirection: Boolean = true): (ApproxPoly, ContextUpdate) = {
        // define delta for redistribution
        val delta = errBudget / Rational.powerTwo(i + 1)
        // get the new error distribution
        val (approxError, rndErrBudget) = distributeErrorBudget(errBudget, oldRoundoff, delta, op, intermRanges(transFnc, path))
        val settings = new MetalibmSettings (elemFncName,
          elemFncDomain,
          approxError,
          nameFunction(elemFncName, elemFncDomain, approxError), // polynomial approximation function name
          rndErrBudget
        )
        ctx.reporter.info(s"Assigned approximation error budget $approxError, roundoff error budget $rndErrBudget")
        // get the new polynomial approximation

        /* ------ Call Metalibm ------ */
        val (approx, reportedError) = callMetalibm(settings, transFnc.getType) match {
          case (Some(e), err) => (e, err)
          case (None, _ )=>
            if (previous.isDefined) {
              ctx.reporter.warning("Metalibm failed or timed out. Returning previous polynomial")
              return previous.get
            } else
              throw new Exception(s"Failed to get an approximation for $x")
        }

        /* ------ Update Context ------ */
        val input = approx.params.head.id // we only have unary functions
        val newInputError = ctx.intermediateAbsErrors.get(fnc.id) match {
          case None => Rational.zero  // after mixed precision opt phase intermediateAbsErrors map is empty
          case Some(map) => map.getOrElse((arg, path), Rational.zero) // put here abs error of arg
        }

        val inputDomain: Map[Identifier, Interval] = Map(input -> intermRanges(arg, path))
        val defaultPrec = ctx.specResultPrecisions.getOrElse(fnc.id, FixedPrecision(32)) // todo other default value?

        /* ------ Create New Node ------ */
        val newNode = ApproxPoly(transFnc, arg, approx.id, errBudget)
        val absError = ctx.intermediateAbsErrors.get(fnc.id) match {
          case None => Rational.zero  // after mixed precision opt phase intermediateAbsErrors map is empty
          case Some(map) => map.getOrElse(x, Rational.zero) // put here abs error of arg
        }
        val resultErrorsMetalibm: Map[Identifier, Map[Expr, Rational]] = Map(fnc.id -> (ctx.approxReportedErrors.getOrElse(fnc.id, Map()) ++ Map(newNode -> reportedError)))

        val tmpCtx = ctx.copy(
          specInputErrors = ctx.specInputErrors + (approx.id -> Map(input -> newInputError)),
          specInputRanges = ctx.specInputRanges + (approx.id -> inputDomain),
          specResultErrorBounds = ctx.specResultErrorBounds + (approx.id -> rndErrBudget),
          specAdditionalConstraints = ctx.specAdditionalConstraints + (approx.id -> BooleanLiteral(true)),
          specInputPrecisions = ctx.specInputPrecisions + (approx.id -> ctx.specInputPrecisions(fnc.id).withDefaultValue(defaultPrec)),
          options = context.options + ("functions" -> List(approx.id.name)),
          originalProgram = Program(FreshIdentifier("approx"), Seq(approx)),
          recomputeAbsErrors = !uniform
        )

        // run the pipeline and get new context and use it in cost computation
        val (newCtx, newPrg) = precisionAssignmentPipeline.run(tmpCtx, Program(FreshIdentifier("approx"), Seq(approx)))
        val fpBody = newPrg.defs.find(_.id == approx.id).get.body.get

        val cost = ctx.option[String]("cost") match {
          case "area" => areaBasedCostFunction(approx.body.get, newCtx.assignedPrecisions(approx.id))
          case "ml" => mlRegressionCostFunction(approx.body.get, fpBody, newCtx.assignedPrecisions(approx.id))
          case "combined" => combinedCost(approx.body.get, fpBody, newCtx.assignedPrecisions(approx.id))
        }

        ctx.reporter.result(s"$transFnc,$i,$op,$approxError,$rndErrBudget,$cost")

        // replace intermediate range for Approx() node with resulting range of the polynomial approximation
        val updRangeKey = Map[(Expr, PathCond), Interval]((newNode, path) -> newCtx.resultRealRanges(approx.id))

        val ctxUpdate = new ContextUpdate(
          approx = newPrg.defs.find(_.id == approx.id).get, // take modified FunDef
          newNode = newNode,
          path = path,
          specInputError =  Map(input -> newInputError),
          specInputeRange = inputDomain,
          specInputPrecision = newCtx.specInputPrecisions(approx.id),
          assignedPrecisions = newCtx.assignedPrecisions(approx.id),
          specResultErrorBounds = rndErrBudget,
          intermediateAbsErrors = newCtx.intermediateAbsErrors(approx.id),
          approxReportedErrors = resultErrorsMetalibm,
          functionToConsider = approx.id.name,
          intermediateRanges = newCtx.intermediateRanges(approx.id),
          resultRealRange = newCtx.resultRealRanges(approx.id),
          resultAbsErrors = newCtx.resultAbsoluteErrors(approx.id),
          uniformPrecision = if (newCtx.uniformPrecisions.contains(approx.id)) newCtx.uniformPrecisions(approx.id) else FixedPrecision(64) // default value for the uniform prec (apparently here we use the mixed-precision so it won't be affecting the results)
        )
        if (oldCost.isEmpty) // nothing to compare with yet
          if (ctx.hasFlag("no-refinement")) {
            ctx.reporter.warning("Refinement loop is disabled. Equal split of local budget")
            (newNode, ctxUpdate)
          }
          else
            feedbackLoop(Some(cost), Some(rndErrBudget), i + 1, op, Some(newNode, ctxUpdate), firstDirection)
        else if (oldCost.get > cost) {
           if (i+1 > MAX_STEPS) {
             (newNode, ctxUpdate) //stop
           } else {
             // continue in the same direction
             feedbackLoop(Some(cost), Some(rndErrBudget), i + 1, op, Some(newNode, ctxUpdate), firstDirection)
           }
        } else if (i == 1 && oldCost.get == cost) { // give it another try
          // continue in the same direction
          feedbackLoop(Some(cost), Some(rndErrBudget), i + 1, op, Some(newNode, ctxUpdate), firstDirection)
        } else {
          // if after one step we notice, the initial choice of direction was bad - choose opposite direction, otherwise stop
           if (i == 1 && firstDirection) {
             val (_, oldRndErrBudget) = distributeErrorBudget(errBudget, None, delta, op, intermRanges(transFnc, path))
             feedbackLoop(oldCost, Some(oldRndErrBudget), 1, op.opposite, previous, firstDirection = false) // as we go the opposite direction, the "previous" result is the one obtained at the half-half split
           } else
           previous match {
             case Some(p) => p // stop
             case None => throw new DaisyFatalError(Some("Something really bad happened. Cannot return approximation"))
           }
        }

      }

      feedbackLoop(None, None, 0, Direction.increase, None)
    }

    val (newTree, contextUpdates, _, mapApprox) = find(expr, List(), context, Map(), Map(), Map())

    (fnc.copy(body = Some(newTree)),
      contextUpdates,
      mapApprox)

  }

  def getElemFncName(expr: Expr): String = expr match {
    case x: Sqrt => "sqrt"
    case x: Log => "log"
    case x: Sin => "sin"
    case x: Cos => "cos"
    case x: Tan => "tan"
    case x: Exp => "exp"
    case Cast(x, _) => getElemFncName(x)
    case x => throw new Exception(s"Expression is not an elementary function: $x")
  }

  def distributeErrorBudget(total: Rational, interval: Interval): (Rational, Rational) = {
    val approxAbsPortion = total / Rational.two
    val rndAbsPortion = total - approxAbsPortion
    val approxError = approxAbsPortion / Rational.max(Interval.maxAbs(interval), Rational.one)
    (approxError, rndAbsPortion)
  }
  def distributeErrorBudget(total: Rational, roundoff: Option[Rational], delta: Rational, op: Direction[Rational], interval: Interval): (Rational, Rational) = {
    // in-/decrease roundoff error budget by delta
    val rndAbsPortion = roundoff match {
      case Some(x) => op.operation(x, delta)
      case None => total / Rational.two
    }
    val approxAbsPortion = total - rndAbsPortion
    val approxError = approxAbsPortion / Rational.max(Interval.maxAbs(interval), Rational.one)
    (approxError, rndAbsPortion)
  }

  def getLocalErrorBudget(prec: TypeTree, i: Interval, uniformPrec: Precision): Rational = prec match {
    case FinitePrecisionType(x) => x.absTranscendentalRoundoff(i)
    case RealType => uniformPrec.absTranscendentalRoundoff(i) // after DataFlow Phase assigned precision is in the context only
  }

  def joinContexts(ctx: Context, cons: Seq[Context]): Context = {
    if (cons.size > 1) {

      cons.foldLeft(cons.head)((x, ct) => {
        ct.copy(specInputRanges = ct.specInputRanges ++ x.specInputRanges,
          specInputErrors = ct.specInputErrors ++ x.specInputErrors,
          specInputPrecisions = ct.specInputPrecisions ++ x.specInputPrecisions,
          specResultPrecisions = ct.specResultPrecisions ++ x.specResultPrecisions,
          specAdditionalConstraints = ct.specAdditionalConstraints ++ x.specAdditionalConstraints,
          specResultRangeBounds = ct.specResultRangeBounds ++ x.specResultRangeBounds,
          specResultErrorBounds = ct.specResultErrorBounds ++ x.specResultErrorBounds,
          resultTupleIds = ct.resultTupleIds ++ x.resultTupleIds,
          uniformPrecisions = ct.uniformPrecisions ++ x.uniformPrecisions,
          resultAbsoluteErrors = ct.resultAbsoluteErrors ++ x.resultAbsoluteErrors,
          resultRealRanges = ct.resultRealRanges ++ x.resultRealRanges,
          resultRelativeErrors = ct.resultRelativeErrors ++ x.resultRelativeErrors,
          intermediateAbsErrors = ct.intermediateAbsErrors ++ x.intermediateAbsErrors,
          intermediateRanges = ct.intermediateRanges ++ x.intermediateRanges)
      })
    } else if (cons.isEmpty) {
      ctx // this should not happen
    } else {
      cons.head
    }
  }

  def nameFunction(fnc: String, domain: Interval, target: Rational): String = {

    def reformat(r: Rational): String = r.toString.replace('.', '_').replace('-', 'm')

    fnc + "_" +
      reformat(domain.xlo) + "to" +
      reformat(domain.xhi) + "_err" +
      reformat(target)
  }

  def callMetalibm(settings: MetalibmSettings, expectedReturnType: TypeTree): (Option[FunDef], Rational) = {

    val problemFile = new File(settings.problemdefFile)
    val buffer = new BufferedWriter(new FileWriter(problemFile))
    buffer.write(settings.toProblemDef)
    buffer.close()

    val f: Future[String] = Future(Process(s"metalibm.sollya ${settings.problemdefFile}") !!)
    try {
      val metalibmOutput = Await.result(f, Duration(50, TimeUnit.SECONDS))

      // TODO: Add a separate timer for Metalibm

      val outputPath = new File(settings.problemdefFile).getParentFile
      // delete gappa files
      if (outputPath.isDirectory) // always a dir
        outputPath.listFiles.filter(_.getName.contains("gappa")).foreach(_.delete())

      val src = Extractor.getApproxFilename(metalibmOutput) // program file
      val reportedError = Extractor.getReportedError(metalibmOutput) // final error (approx + double precision roundoff)
      (CodeParser.getFunctionDef(src, settings, expectedReturnType), reportedError)
    } catch {
      case _: TimeoutException => throw new DaisyFatalError(Some(s"Metalibm took too long to generate a polynomial for ${settings.functionName}() on ${settings.domain}"))
    }
  }

  def applyFinitePrecisionToFunDef(fnc: FunDef, typeConfig: TypeConfig, currRanges: RangeMap): (FunDef, RangeMap) = {
    val (updatedBody, newRangeMap) = opt.MixedPrecisionOptimizationPhase.applyFinitePrecision(fnc.body.get, typeConfig, currRanges)
    val updatedParams = fnc.params.map(valDef => ValDef(valDef.id.changeType(FinitePrecisionType(typeConfig(valDef.id)))))
    (fnc.copy(body = Some(updatedBody), params = updatedParams, returnType = updatedBody.getType), newRangeMap)
  }
}

trait Direction[T] {
  val opposite: Direction[T]
  val operation: (T, T) => T
}

object Direction {

  final case object increase extends Direction[Rational] {
    override val opposite: Direction[Rational] = decrease
    override val operation: (Rational, Rational) => Rational = (a: Rational, b: Rational) => a + b
  }

  final case object decrease extends Direction[Rational] {
    override val opposite: Direction[Rational] = increase
    override val operation: (Rational, Rational) => Rational = (a: Rational, b: Rational) => a - b
  }
}

class ContextUpdate(val approx: FunDef,
                    val newNode: ApproxPoly,
                    val path: PathCond,
                    specInputError: Map[Identifier, Rational],
                    specInputeRange: Map[Identifier, Interval],
                    specResultErrorBounds: Rational,
                    specInputPrecision: Map[Identifier, Precision],
                    intermediateAbsErrors: Map[(Expr, PathCond), Rational],
                    intermediateRanges: Map[(Expr, PathCond), Interval],
                    val approxReportedErrors: Map[Identifier, Map[Expr, Rational]],
                    functionToConsider: String,
                    val resultRealRange: Interval,
                    val resultAbsErrors: Rational,
                    assignedPrecisions: Map[Identifier, Precision],
                    uniformPrecision: Precision = FixedPrecision(64)
                   ) {

  def applyToContext(ctx: Context): Context = ctx.copy(
              specInputErrors = ctx.specInputErrors + (approx.id -> specInputError),
              specInputRanges = ctx.specInputRanges + (approx.id -> specInputeRange),
              specResultErrorBounds = ctx.specResultErrorBounds + (approx.id -> specResultErrorBounds),
              specAdditionalConstraints = ctx.specAdditionalConstraints + (approx.id -> BooleanLiteral(true)),
              specInputPrecisions = ctx.specInputPrecisions + (approx.id -> specInputPrecision),
              approxReportedErrors = updateValues(ctx.approxReportedErrors, approxReportedErrors),
              options = ctx.options + ("functions" -> (ctx.option[List[String]]("functions") :+ functionToConsider)),
              intermediateAbsErrors = ctx.intermediateAbsErrors + (approx.id -> intermediateAbsErrors),
              intermediateRanges = ctx.intermediateRanges + (approx.id -> intermediateRanges),
              resultRealRanges = ctx.resultRealRanges + (approx.id -> resultRealRange),
              resultAbsoluteErrors = ctx.resultAbsoluteErrors + (approx.id -> resultAbsErrors),
              uniformPrecisions = ctx.uniformPrecisions + (approx.id -> uniformPrecision),
              assignedPrecisions = ctx.assignedPrecisions + (approx.id -> assignedPrecisions)
            )
  def updateValues(toUpdate: Map[Identifier, Map[Expr, Rational]], newVals: Map[Identifier, Map[Expr, Rational]]): Map[Identifier, Map[Expr, Rational]] =
    newVals.foldLeft(toUpdate)((acc, x) => {
      val (id, newVal) = x
      if (acc.contains(id)){
        acc + (id -> (acc(id) ++ newVal))
      } else
        acc + x
    })
}