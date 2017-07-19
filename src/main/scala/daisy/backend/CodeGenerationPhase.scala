
package daisy
package backend

import daisy.lang.{ScalaPrinter, PrettyPrinter}
import lang.Trees._
import utils.FinitePrecision._
import lang.Types._
import lang.Extractors.ArithOperator
import utils.Rational
import lang.NumAnnotation

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
      case _ => ;
    }

    val newProgram = uniformPrecision match {

      case Fixed(b) =>
        // if we have fixed-point code, we need to generate it first
        val newDefs = prg.defs.map(fnc => if (!fnc.body.isEmpty && !fnc.precondition.isEmpty) {
            val newBody = toFixedPointCode(fnc.body.get, Fixed(b))
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

      case _ =>
        // if we have floating-point code, we need to just change the types
        val fileLocation = "./output/" + prg.id + ".scala"
        ctx.reporter.info("generating code in " + fileLocation)
        val typedPrg = assignFloatType(prg, FinitePrecisionType(uniformPrecision))
        writeScalaFile(fileLocation, typedPrg)
        typedPrg
    }

    timer.stop
    ctx.reporter.info(s"Finished $name")
    (ctx, newProgram)
  }

  private def writeScalaFile(filename: String, prg: Program) {
    import java.io.FileWriter
    import java.io.BufferedWriter
    val fstream = new FileWriter(filename)
    val out = new BufferedWriter(fstream)
    out.write(ScalaPrinter.apply(prg))
    out.close
  }

  private def assignFloatType(prg: Program, tpe: FinitePrecisionType): Program = {

    def changeType(e: Expr): Expr = e match {
      case Variable(id) => Variable(id.changeType(tpe))
      case x @ RealLiteral(r) =>
        val tmp = FinitePrecisionLiteral(r, tpe.prec)
        tmp.stringValue = x.stringValue
        tmp
      case ArithOperator(args, recons) =>
        recons(args.map(changeType))

      case Let(id, value, body) =>
        Let(id.changeType(tpe), changeType(value), changeType(body))
    }

    val newDefs = prg.defs.map({
      case FunDef(id, returnType, params, pre, body, post, isField) =>

        FunDef(id, tpe,
          params.map(vd => ValDef(vd.id.changeType(tpe))),
          // this should really be changed too
          pre,
          body.map(changeType(_)),
          post,
          isField
        )
      })


    Program(prg.id, newDefs)

  }

  /*
   * Expects code to be already in SSA form.
   * @param fixed the (uniform) fixed-point precision to use
   // TODO: we also need to adjust the types, no?
   */
  def toFixedPointCode(expr: Expr, fixed: Fixed): Expr = (expr: @unchecked) match {
    case x @ Variable(id) => fixed match {
      case Fixed(16) => Variable(id.changeType(Int32Type))
      case Fixed(32) => Variable(id.changeType(Int64Type))
    }

    case RealLiteral(r) => // TODO: translate constant
      val f = fixed.fractionalBits(r)
      fixed match {
        case Fixed(16) => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
        case Fixed(32) => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
      }

    case UMinus(t) => UMinus(toFixedPointCode(t, fixed))

    case Sqrt(t) =>
      throw new Exception("Sqrt is not supported for fixed-points!")
      null

    case x @ Plus(lhs, rhs) =>
      // fractional bits from lhs
      val fLhs = fixed.fractionalBits(lhs.asInstanceOf[NumAnnotation].interval)
      val fRhs = fixed.fractionalBits(rhs.asInstanceOf[NumAnnotation].interval)

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toFixedPointCode(lhs, fixed), (fAligned - fLhs))
        else toFixedPointCode(lhs, fixed)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toFixedPointCode(rhs, fixed), (fAligned - fRhs))
        else toFixedPointCode(rhs, fixed)

      // fractional bits result
      val fRes = fixed.fractionalBits(x.interval)
      // shift result
      if (fAligned == fRes) {
        Plus(newLhs, newRhs)
      } else if(fRes < fAligned) {
        RightShift(Plus(newLhs, newRhs), (fAligned - fRes))
      } else { //(fAligned < fRes) {
        // TODO: this sounds funny. does this ever happen?
        reporter.warning("funny shifting condition is happening")
        LeftShift(Plus(newLhs, newRhs), (fRes - fAligned))

      }

    case x @ Minus(lhs, rhs) =>
      // fractional bits from lhs
      val fLhs = fixed.fractionalBits(lhs.asInstanceOf[NumAnnotation].interval)
      val fRhs = fixed.fractionalBits(rhs.asInstanceOf[NumAnnotation].interval)

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toFixedPointCode(lhs, fixed), (fAligned - fLhs))
        else toFixedPointCode(lhs, fixed)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toFixedPointCode(rhs, fixed), (fAligned - fRhs))
        else toFixedPointCode(rhs, fixed)

      // fractional bits result
      val fRes = fixed.fractionalBits(x.interval)
      // shift result
      if (fAligned == fRes) {
        Minus(newLhs, newRhs)
      } else if(fRes < fAligned) {
        RightShift(Minus(newLhs, newRhs), (fAligned - fRes))
      } else { //(fAligned < fRes) {
        // TODO: this sounds funny. does this ever happen?
        reporter.warning("funny shifting condition is happening")
        LeftShift(Minus(newLhs, newRhs), (fRes - fAligned))
      }

    case x @ Times(lhs, rhs) =>

      val mult = Times(toFixedPointCode(lhs, fixed), toFixedPointCode(rhs, fixed))
      val fMult = fixed.fractionalBits(lhs.asInstanceOf[NumAnnotation].interval) +
        fixed.fractionalBits(rhs.asInstanceOf[NumAnnotation].interval)

      // fractional bits result
      val fRes = fixed.fractionalBits(x.interval)
      // shift result
      if (fMult == fRes) {
        mult
      } else if(fRes < fMult) {
        RightShift(mult, (fMult - fRes))
      } else { //(fAligned < fRes) {
        // TODO: this sounds funny. does this ever happen?
        reporter.warning("funny shifting condition is happening")
        LeftShift(mult, (fRes - fMult))
      }

    case x @ Division(lhs, rhs) =>
      val fLhs = fixed.fractionalBits(lhs.asInstanceOf[NumAnnotation].interval)
      val fRhs = fixed.fractionalBits(rhs.asInstanceOf[NumAnnotation].interval)

      val fRes = fixed.fractionalBits(x.interval)
      val shift = fRes + fRhs - fLhs
      Division(LeftShift(toFixedPointCode(lhs, fixed), shift), toFixedPointCode(rhs, fixed))

    case Let(id, value, body) =>
      Let(id, toFixedPointCode(value, fixed), toFixedPointCode(body, fixed))
  }

}
