// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import utils.CodePrinter
import lang.Trees._
import tools.FinitePrecision
import FinitePrecision._
import tools.FinitePrecision._
import lang.Types._
import lang.Extractors.ArithOperator
import tools.{Interval, Rational}
import lang.Identifiers.Identifier

object CodeGenerationPhase extends DaisyPhase {
  override val name = "Code Generation"
  override val shortName = "codegen"
  override val description = "Generates (executable) code."
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "lang",
      Set("C", "Scala", "FPCore"),
      "Scala",
      "Language for which to generate code"),
    FlagOption(
      "genMain",
      "Whether to generate a main method to run the code."),
    FlagOption(
      "apfixed",
      "Print C code for Xilinx with ap_fixed data type")
  )

  implicit val debugSection = DebugSectionBackend

  var reporter: Reporter = null

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val mixedPrecision = ctx.option[Option[String]]("mixed-precision").isDefined
    val uniformPrecision = ctx.option[Precision]("precision")
    val lang = ctx.option[String]("lang")

    reporter = ctx.reporter

    val newProgram =
      if (ctx.hasFlag("mixed-tuning")) {
        if (ctx.hasFlag("mixed-fixedpoint")) {

          // generate fixed-point code, types are already attached to the tree
          // TODO: keep the types in a map...

          val newDefs = prg.defs.map(fnc => if (!fnc.body.isEmpty && !fnc.precondition.isEmpty) {
            // TODO: check that these ranges are actually there, otehrwise,
            // we may need to do some copying in MixedTuning
            val newBody = toMixedFixedPointCode(fnc.body.get, ctx.intermediateRanges(fnc.id))
            val retType = fnc.returnType match {
              case FinitePrecisionType(FixedPrecision(16)) => Int32Type
              case FinitePrecisionType(FixedPrecision(32)) => Int64Type
            }
            fnc.copy(
              params = fnc.params.map(vd =>
                vd.id.getType match {
                  case FinitePrecisionType(FixedPrecision(16)) =>
                    ValDef(vd.id.changeType(Int32Type))
                  case FinitePrecisionType(FixedPrecision(32)) =>
                    ValDef(vd.id.changeType(Int64Type))
                }),
              body = Some(newBody),
              returnType = retType)
          } else {
            fnc
          })

          Program(prg.id, newDefs)

        } else { //floats
          // the types have already been changed and nothing more needs to be done
          // types are changed before, since the semantics of type assignments is special
          prg
        }
      } else {
        uniformPrecision match {
          case FixedPrecision(b) =>
            if (mixedPrecision) {
              ctx.reporter.error("Mixed-precision code generation is currently not supported for fixed-points.")
            }
            if (ctx.option[Boolean]("apfixed")) {
              val newDefs = functionsToConsider(ctx, prg).map(fnc => {
                val newBody = toAPFixedCode(fnc.body.get, b, ctx.intermediateRanges(fnc.id),
                  ctx.intermediateAbsErrors(fnc.id))
                val fncParams = fnc.params.map({
                  case ValDef(id) =>
                    val actualRange = ctx.intermediateRanges(fnc.id)(Variable(id)) +/- ctx.intermediateAbsErrors(fnc.id)(Variable(id))
                    val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
                    ValDef(id.changeType(APFixedType(b, intBits)))
                  })
                val actualRangeResult = ctx.resultRealRanges(fnc.id) +/- ctx.resultAbsoluteErrors(fnc.id)
                val intBitsResult = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRangeResult))
                val retType = APFixedType(b, intBitsResult)

                fnc.copy(
                  params = fncParams,
                  body = Some(newBody),
                  returnType = retType)
              })
              Program(prg.id, newDefs)
            } else {
              // if we have fixed-point code, we need to generate it first
              // TODO handle ignored functions
              val newDefs = transformConsideredFunctions(ctx,prg){ fnc =>
                val newBody = fnc.body.map(toFixedPointCode(_, FixedPrecision(b),
                  ctx.intermediateRanges(fnc.id), ctx.intermediateAbsErrors(fnc.id)))
                val valDefType = b match {
                  case 8 => Int16Type
                  case 16 => Int32Type
                  case 32 => Int64Type
                }
                fnc.copy(
                  params = fnc.params.map(vd => ValDef(vd.id.changeType(valDefType))),
                  body = newBody,
                  returnType = valDefType)
              }
              Program(prg.id, newDefs)
            }
          case up @ FloatPrecision(_) =>
            // if we have floating-point code, we need to just change the types
            Program(prg.id, prg.defs.map { fnc =>
              assignFloatType(fnc, ctx.specInputPrecisions(fnc.id), ctx.specResultPrecisions(fnc.id), up)
            })
        }
      }

    writeFile(newProgram, lang, ctx)

    (ctx, newProgram)
  }

  private def writeFile(prg: Program, lang: String, ctx: Context): Unit = {
    import java.io.FileWriter
    import java.io.BufferedWriter

    val filename = if (ctx.hasFlag("apfixed")) {
        System.getProperty("user.dir")+"/output/" + prg.id + ".cpp"
      } else {
        System.getProperty("user.dir")+"/output/" + prg.id + CodePrinter.suffix(lang)
      }
    ctx.codegenOutput.append(prg.id)
    val fstream = new FileWriter(filename)
    val out = new BufferedWriter(fstream)
    CodePrinter(prg, ctx, lang, out)
  }

  private def assignFloatType(fnc: FunDef, typeMap: Map[Identifier, Precision],
                              returnType: Precision, defaultPrecision: Precision): FunDef = {

    def changeType(e: Expr, tpeMap: Map[Identifier, Precision]): (Expr, Precision) = e match {

      case Variable(id) =>
        (Variable(id.changeType(FinitePrecisionType(tpeMap(id)))), tpeMap(id))

      case x @ RealLiteral(r) =>
        (FinitePrecisionLiteral(r, defaultPrecision, x.stringValue), defaultPrecision)

      case ArithOperator(es_old, recons) =>
        val (es, ps) = es_old.unzip(changeType(_, tpeMap))

        val prec = getUpperBound(ps: _*)
        (recons(es), prec)

      case Let(id, value, body) =>
        val (eValue, valuePrec) = changeType(value, tpeMap)
        val (eBody, bodyPrec) = changeType(body, tpeMap)

        val idPrec = tpeMap(id)

        if (idPrec >= valuePrec) {
          (Let(id.changeType(FinitePrecisionType(tpeMap(id))), eValue, eBody), bodyPrec)
        } else {
          val newValue = Cast(eValue, FinitePrecisionType(idPrec))
          (Let(id.changeType(FinitePrecisionType(tpeMap(id))), newValue, eBody), bodyPrec)
        }
    }

    fnc.copy(
      returnType = FinitePrecisionType(returnType),
      params = fnc.params.map(vd => ValDef(vd.id.changeType(FinitePrecisionType(typeMap(vd.id))))),
      // this should really be changed too
      body = fnc.body.map(changeType(_, typeMap)._1)
    )
  }

  /*
   * Expects code to be already in SSA form.
   * @param format the (uniform) fixed-point precision to use
   */
  def toFixedPointCode(expr: Expr, format: FixedPrecision, intermRanges: Map[Expr, Interval],
    intermAbsErrors: Map[Expr, Rational]): Expr = {
    val newType = format match {
      case FixedPrecision(8) => Int16Type
      case FixedPrecision(16) => Int32Type
      case FixedPrecision(32) => Int64Type
    }

    @inline
    def getFractionalBits(e: Expr): Int = {
      // the overall interval is the real-valued range +/- absolute errors
      val interval = intermRanges(e) +/- intermAbsErrors(e)
      format.fractionalBits(interval)
    }


    def _toFPCode(e: Expr): Expr = (e: @unchecked) match {
      case x @ Variable(id) => Variable(id.changeType(newType))

      case RealLiteral(r) => // TODO: translate constant
        val f = format.fractionalBits(r)
        format match {
          case FixedPrecision(8) => Int16Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
          case FixedPrecision(16) => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
          case FixedPrecision(32) => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
        }

      case UMinus(t) => UMinus(_toFPCode(t))

      case Sqrt(t) =>
        throw new Exception("Sqrt is not supported for fixed-points!")

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
          //reporter.warning("funny shifting condition is happening")
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
          //reporter.warning("funny shifting condition is happening")
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
          //reporter.warning("funny shifting condition is happening")
          LeftShift(mult, (fRes - fMult))
        }

      case x @ Division(lhs, rhs) =>
        val fLhs = getFractionalBits(lhs)
        val fRhs = getFractionalBits(rhs)

        val fRes = getFractionalBits(x)
        val shift = fRes + fRhs - fLhs
        Division(LeftShift(_toFPCode(lhs), shift), _toFPCode(rhs))

      case Let(id, value, body) =>
        Let(id.changeType(newType), _toFPCode(value), _toFPCode(body))
    }

    _toFPCode(expr)
  }

  /*
   * Generates code for the Vivado HLS hardware synthesis tool with the ap_fixed data type.
   * Expects code to be already in SSA form.
   */
  def toAPFixedCode(expr: Expr, totalBits: Int, intermRanges: Map[Expr, Interval],
    intermAbsErrors: Map[Expr, Rational]): Expr = {

    @inline
    def getIntegerBits(e: Expr): Int = {
      // the overall interval is the real-valued range +/- absolute errors
      val actualRange = intermRanges(e) +/- intermAbsErrors(e)
      FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
    }

    def _toFPCode(e: Expr): Expr = (e: @unchecked) match {
      case x @ Variable(id) => x //Variable(id.changeType(newType))

      case x @ RealLiteral(r) => x  // constants are handled automatically?

      case x @ ArithOperator(Seq(t: Terminal), recons) => x
      case x @ ArithOperator(Seq(lhs: Terminal, rhs: Terminal), recons) => x

      case Let(id, value, body) =>
        val idType = APFixedType(totalBits, getIntegerBits(value))

        Let(id.changeType(idType), _toFPCode(value), _toFPCode(body))


      // case UMinus(t) => UMinus(_toFPCode(t))

      // case Sqrt(t) =>
      //   throw new Exception("Sqrt is not supported for fixed-points!")
      //   null

    }

    _toFPCode(expr)
  }

  /*
   * Expects code to be already in SSA form.
   * Expects the types to be attached to the tree and expects the program
   * to be in SSA form.
   * TODO: check that this is sound, I think the range should be finite-precision
   * and not real-valued as it is most likely here...
   */
  def toMixedFixedPointCode(expr: Expr, rangeMap: Map[Expr, Interval]): Expr = (expr: @unchecked) match {

    case x @ Variable(id) => id.getType match {
      case FinitePrecisionType(FixedPrecision(16)) => Variable(id.changeType(Int32Type))
      case FinitePrecisionType(FixedPrecision(32)) => Variable(id.changeType(Int64Type))
    }

    // case Let(id, x @ RealLiteral(r), body) =>
    //   val tpe = id.getType.asInstanceOf[Fixed]
    //   val f = tpe.fractionalBits(r)
    //   tpe match {
    //     case FixedPrecision(16) =>
    //       val tmp = Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
    //       Let(id.changeType(Int32Type), tmp, toMixedFixedPointCode(body))

    //     case FixedPrecision(32) =>
    //       val tmp = Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
    //       Let(id.changeType(Int64Type), tmp, toMixedFixedPointCode(body))
    //   }

    case FinitePrecisionLiteral(r, prec @ FixedPrecision(_), strVal) =>
      val f = prec.fractionalBits(r)
      prec match {
        case FixedPrecision(16) => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
        case FixedPrecision(32) => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
      }

    // TODO: we may shift too much, i.e. (x >> 32) << 3, which can be optmixed/
    // cleaned up after the fact.
    // necessarily a down cast from 32 bit
    case Cast(e, FinitePrecisionType(FixedPrecision(16))) =>
      Cast(RightShift(toMixedFixedPointCode(e, rangeMap), 16), Int32Type)

    // necessarily an up cast from 16 bit, and not redundant (if all went well in mixed-opt)
    case Cast(e, FinitePrecisionType(FixedPrecision(32))) =>
      Cast(LeftShift(toMixedFixedPointCode(e, rangeMap), 16), Int64Type)

    // case Let(id, x @ UMinus(y @ Variable(t)), body) =>
    //   if (id.getType == t.getType) {
    //     UMinus(toMixedFixedPointCode(t))
    //   } else {
    //     assert(id.getType < t.getType)
    //     //downcast
    //     RightShift(UMinus(toMixedFixedPointCode(t)), 16)
    //   }

    case UMinus(t) => UMinus(toMixedFixedPointCode(t, rangeMap))

    case Sqrt(t) =>
      throw new Exception("Sqrt is not supported for fixed-points!")
      //null

    case x @ Plus(lhs, rhs) =>
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      // fractional bits from lhs
      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs))

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toMixedFixedPointCode(lhs, rangeMap), (fAligned - fLhs))
        else toMixedFixedPointCode(lhs, rangeMap)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toMixedFixedPointCode(rhs, rangeMap), (fAligned - fRhs))
        else toMixedFixedPointCode(rhs, rangeMap)

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x))
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
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      // fractional bits from lhs
      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs))

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toMixedFixedPointCode(lhs, rangeMap), (fAligned - fLhs))
        else toMixedFixedPointCode(lhs, rangeMap)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toMixedFixedPointCode(rhs, rangeMap), (fAligned - fRhs))
        else toMixedFixedPointCode(rhs, rangeMap)

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x))
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
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]

      val mult = Times(toMixedFixedPointCode(lhs, rangeMap), toMixedFixedPointCode(rhs, rangeMap))
      val fMult = lhsPrec.fractionalBits(rangeMap(lhs)) +
        rhsPrec.fractionalBits(rangeMap(rhs))

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x))
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
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]

      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs))

      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x))
      val shift = fRes + fRhs - fLhs
      Division(LeftShift(toMixedFixedPointCode(lhs, rangeMap), shift), toMixedFixedPointCode(rhs, rangeMap))

    case Let(id, value, body) =>
      val newId = id.changeType(id.getType match {
              case FinitePrecisionType(FixedPrecision(16)) => Int32Type
              case FinitePrecisionType(FixedPrecision(32)) => Int64Type
            })
      Let(newId, toMixedFixedPointCode(value, rangeMap), toMixedFixedPointCode(body, rangeMap))
  }

}
