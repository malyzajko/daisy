// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import scala.collection.immutable.Seq

import daisy.lang.{ScalaPrinter, PrettyPrinter}
import lang.Trees._
import lang.TreeOps.allVariablesOf
import tools.FinitePrecision._
import lang.Types._
import lang.Extractors.ArithOperator
import tools.{Rational, Interval}
import lang.Identifiers.Identifier

object CodeGenerationPhase extends DaisyPhase {

  override val name = "codegen"
  override val description = "Generates (executable) code."

  // TODO: have this generate C code
  override val definedOptions: Set[CmdLineOptionDef[Any]] = Set()

  implicit val debugSection = DebugSectionBackend

  var reporter: Reporter = null

  def run(ctx: Context, prg: Program): (Context, Program) = {
    reporter = ctx.reporter
    reporter.info(s"\nStarting $name")
    val timer = ctx.timers.rangeError.start

    var mixedPrecision: Boolean = false
    var uniformPrecision: Precision = Float64

    /* Process relevant options */
    for (opt <- ctx.options) opt match {
      // this option is defined in RangeErrorPhase. This is not ideal,
      // but since for now, Codegen will only be called after RangeErrorPhase
      // it should be OK for now.
      case ChoiceOption("precision", s) => s match {
        case "Float32" => uniformPrecision = Float32
        case "Float64" => uniformPrecision = Float64
        case "DoubleDouble" => uniformPrecision = DoubleDouble
        case "QuadDouble" => uniformPrecision = QuadDouble
        case "Fixed16" => uniformPrecision = Fixed(16)
        case "Fixed32" => uniformPrecision = Fixed(32)
        case _ =>
          ctx.reporter.warning(s"Unknown precision specified: $s, choosing default ($uniformPrecision)!")
      }
      case ParamOption("mixed-precision", _) => mixedPrecision = true
      case _ => ;
    }



    val newProgram = if (ctx.fixedPoint) {
      if (mixedPrecision) {
        reporter.error("Mixed-precision code generation is currently not supported for fixed-points.")
      }

      (uniformPrecision: @unchecked) match {

        case Fixed(b) =>
          // if we have fixed-point code, we need to generate it first
          val newDefs = prg.defs.map(fnc => if (!fnc.body.isEmpty && !fnc.precondition.isEmpty) {
            val newBody = toFixedPointCode(fnc.body.get, Fixed(b),
              ctx.intermediateRanges(fnc.id), ctx.intermediateAbsErrors(fnc.id))
            val valDefType = if (b == 16) Int32Type else Int64Type
            fnc.copy(
              params = fnc.params.map(vd => ValDef(vd.id.changeType(valDefType))),
              body = Some(newBody),
              returnType = valDefType)
          } else {
            fnc
          })

          val newPrg = Program(prg.id, newDefs)
          val fileLocation = "./output/" + prg.id + ".scala"
          ctx.reporter.info("generating code in " + fileLocation)
          writeScalaFile(fileLocation, newPrg)
          newPrg
      }

    } else {
      val precisionMap: Map[Identifier, Map[Identifier, Precision]] = if (mixedPrecision) {
        ctx.specMixedPrecisions
      } else {
        prg.defs.map(fnc =>
          if (fnc.body.isEmpty) {
            (fnc.id -> Map[Identifier, Precision]())
          } else {
            (fnc.id -> allVariablesOf(fnc.body.get).map(id => (id -> uniformPrecision)).toMap)
          }
        ).toMap
      }
      val returnPrecisionMap: Map[Identifier, Precision] = if (mixedPrecision) {
        ctx.specInferredReturnTypes
      } else {
        prg.defs.map(fnc => (fnc.id -> uniformPrecision)).toMap
      }

      // if we have floating-point code, we need to just change the types
      val fileLocation = "./output/" + prg.id + ".scala"
      ctx.reporter.info("generating code in " + fileLocation)

      val typedPrg = assignFloatType(prg, precisionMap, returnPrecisionMap, uniformPrecision)

      writeScalaFile(fileLocation, typedPrg)
      typedPrg
    }



    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, newProgram)
  }

  private def writeScalaFile(filename: String, prg: Program): Unit = {
    import java.io.FileWriter
    import java.io.BufferedWriter
    val fstream = new FileWriter(filename)
    val out = new BufferedWriter(fstream)
    out.write(ScalaPrinter.apply(prg))
    out.close
  }

  private def assignFloatType(prg: Program, typeMaps: Map[Identifier, Map[Identifier, Precision]],
    returnTypes: Map[Identifier, Precision], defaultPrecision: Precision): Program = {

    def changeType(e: Expr, tpeMap: Map[Identifier, Precision]): (Expr, Precision) = e match {

      case Variable(id) =>
        (Variable(id.changeType(FinitePrecisionType(tpeMap(id)))), tpeMap(id))

      case x @ RealLiteral(r) =>
        val tmp = FinitePrecisionLiteral(r, defaultPrecision)
        tmp.stringValue = x.stringValue
        (tmp, defaultPrecision)

      case ArithOperator(Seq(l, r), recons) =>
        val (eLeft, pLeft) = changeType(l, tpeMap)
        val (eRight, pRight) = changeType(r, tpeMap)

        val prec = getUpperBound(pLeft, pRight)
        (recons(Seq(eLeft, eRight)), prec)

      case ArithOperator(Seq(t), recons) =>
        val (e, p) = changeType(t, tpeMap)
        (recons(Seq(e)), p)

      case Let(id, value, body) =>
        val (eValue, valuePrec) = changeType(value, tpeMap)
        val (eBody, bodyPrec) = changeType(body, tpeMap)

        val idPrec = tpeMap(id)

        if (idPrec >= valuePrec) {
          (Let(id.changeType(FinitePrecisionType(tpeMap(id))), eValue, eBody), bodyPrec)
        } else {
          val newValue = Downcast(eValue, FinitePrecisionType(idPrec))
          (Let(id.changeType(FinitePrecisionType(tpeMap(id))), newValue, eBody), bodyPrec)
        }
    }

    val newDefs = prg.defs.map({
      case FunDef(id, returnType, params, pre, body, post, isField) =>

        FunDef(id, FinitePrecisionType(returnTypes(id)),
          params.map(vd => ValDef(vd.id.changeType(FinitePrecisionType(typeMaps(id)(vd.id))))),
          // this should really be changed too
          pre,
          body.map(changeType(_, typeMaps(id))._1),
          post,
          isField
        )
      })


    Program(prg.id, newDefs)

  }

  /*
   * Expects code to be already in SSA form.
   * @param fixed the (uniform) fixed-point precision to use
   * TODO: we also need to adjust the types, no?
   */
  def toFixedPointCode(expr: Expr, format: Fixed, intermRanges: Map[Expr, Interval],
    intermAbsErrors: Map[Expr, Rational]): Expr = {

    @inline
    def getFractionalBits(e: Expr): Int = {
      // the overall interval is the real-valued range +/- absolute errors
      val interval = intermRanges(e) +/- intermAbsErrors(e)
      format.fractionalBits(interval)
    }


    def _toFPCode(e: Expr): Expr = (e: @unchecked) match {
      case x @ Variable(id) => format match {
        case Fixed(16) => Variable(id.changeType(Int32Type))
        case Fixed(32) => Variable(id.changeType(Int64Type))
      }

      case RealLiteral(r) => // TODO: translate constant
        val f = format.fractionalBits(r)
        format match {
          case Fixed(16) => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
          case Fixed(32) => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
        }

      case UMinus(t) => UMinus(_toFPCode(t))

      case Sqrt(t) =>
        throw new Exception("Sqrt is not supported for fixed-points!")
        null

      case x @ Plus(lhs, rhs) =>
        val fLhs = getFractionalBits(lhs)
        val fRhs = getFractionalBits(rhs)

        // determine how much to shift left or right
        val fAligned = math.max(fLhs, fRhs)
        val newLhs =
          if (fLhs < fAligned) {
            LeftShift(_toFPCode(lhs), (fAligned - fLhs))
          } else {
            _toFPCode(lhs)
          }
        val newRhs =
          if (fRhs < fAligned) {
            LeftShift(_toFPCode(rhs), (fAligned - fRhs))
          } else {
            _toFPCode(rhs)
          }

        // fractional bits result
        val fRes = getFractionalBits(x)
        // shift result
        if (fAligned == fRes) {
          Plus(newLhs, newRhs)
        } else if (fRes < fAligned) {
          RightShift(Plus(newLhs, newRhs), (fAligned - fRes))
        } else { // (fAligned < fRes) {
          // TODO: this sounds funny. does this ever happen?
          reporter.warning("funny shifting condition is happening")
          LeftShift(Plus(newLhs, newRhs), (fRes - fAligned))

        }

      case x @ Minus(lhs, rhs) =>
        // fractional bits from lhs
        val fLhs = getFractionalBits(lhs)
        val fRhs = getFractionalBits(rhs)

        // determine how much to shift left or right
        val fAligned = math.max(fLhs, fRhs)
        val newLhs =
          if (fLhs < fAligned) {
            LeftShift(_toFPCode(lhs), (fAligned - fLhs))
          } else {
            _toFPCode(lhs)
          }
        val newRhs =
          if (fRhs < fAligned) {
            LeftShift(_toFPCode(rhs), (fAligned - fRhs))
          } else {
            _toFPCode(rhs)
          }

        // fractional bits result
        val fRes = getFractionalBits(x)
        // shift result
        if (fAligned == fRes) {
          Minus(newLhs, newRhs)
        } else if (fRes < fAligned) {
          RightShift(Minus(newLhs, newRhs), (fAligned - fRes))
        } else { // (fAligned < fRes) {
          // TODO: this sounds funny. does this ever happen?
          reporter.warning("funny shifting condition is happening")
          LeftShift(Minus(newLhs, newRhs), (fRes - fAligned))
        }

      case x @ Times(lhs, rhs) =>

        val mult = Times(_toFPCode(lhs), _toFPCode(rhs))
        val fMult = getFractionalBits(lhs) + getFractionalBits(rhs)

        // fractional bits result
        val fRes = getFractionalBits(x)
        // shift result
        if (fMult == fRes) {
          mult
        } else if (fRes < fMult) {
          RightShift(mult, (fMult - fRes))
        } else { // (fAligned < fRes) {
          // TODO: this sounds funny. does this ever happen?
          reporter.warning("funny shifting condition is happening")
          LeftShift(mult, (fRes - fMult))
        }

      case x @ Division(lhs, rhs) =>
        val fLhs = getFractionalBits(lhs)
        val fRhs = getFractionalBits(rhs)

        val fRes = getFractionalBits(x)
        val shift = fRes + fRhs - fLhs
        Division(LeftShift(_toFPCode(lhs), shift), _toFPCode(rhs))

      case Let(id, value, body) =>
        Let(id, _toFPCode(value), _toFPCode(body))
    }

    _toFPCode(expr)
  }
}
