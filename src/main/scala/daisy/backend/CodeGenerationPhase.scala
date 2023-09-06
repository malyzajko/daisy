// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package backend

import java.io.FileWriter
import java.io.BufferedWriter
import scala.collection.immutable.Seq
import utils.CodePrinter
import lang.Trees._
import tools.{FinitePrecision, Interval, Rational}
import FinitePrecision._
import daisy.lang.TreeOps.{allIDsOf, containsApproxNode, isMatrix, isVector}
import lang.Types._
import lang.Extractors.{ArithOperator, DSOperations}
import lang.Identifiers.{FreshIdentifier, Identifier}

object CodeGenerationPhase extends DaisyPhase {
  override val name = "Code generation"
  override val description = "Generates (executable) code."
  override val definedOptions: Set[CmdLineOption[Any]] = Set(
    StringChoiceOption(
      "lang",
      Set("C", "Scala", "FPCore", "Satire"),
      "Scala",
      "Language for which to generate code"),
    FlagOption(
      "genMain",
      "Whether to generate a main method to run the code."),
    FlagOption(
      "apfixed",
      "Print C code for Xilinx with ap_fixed data type"),
    FlagOption(
      "randomIns",
      "Generate random inputs for the functions. Automatically generates main()"),
    FlagOption(
      "inputsToJson",
      "Save randomply generated input values into a json file.")
  )

  override implicit val debugSection = DebugSectionBackend

  var reporter: Reporter = null

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {
    val uniformPrecisions = ctx.uniformPrecisions

    reporter = ctx.reporter

    val mixedTuning = ctx.hasFlag("mixed-tuning") || (ctx.hasFlag("approx") && ctx.hasFlag("polyMixed"))
    val fixedPrecision = ctx.fixedPoint
    val apfixedFormat = ctx.hasFlag("apfixed")

    val ctxCopy = if (ctx.hasFlag("approx")) ctx.copy(options = ctx.options + ("approx" -> false)) else ctx
    val newDefs = transformConsideredFunctions(ctxCopy, prg){ fnc =>

      // if the function returns tuples, we need to re-tuple them
      val _body = fnc.returnType match {
        case TupleType(_) => reTuple(fnc.body.get, ctx.resultTupleIds(fnc.id))
        case _ => fnc.body.get
      }

      // different scenarios:

      // CASE: mixed-tuning for fixed-point arithmetic, necessarily with apfixed
      if (mixedTuning && fixedPrecision) {
        // CASE: approx only: 1) fnc is polynomial approximation and has uniform precision; 2) fnc is top-level and has uniform precision
        if (ctx.hasFlag("approx") &&
          ((ctx.hasFlag("polyUniform") && !containsApproxNode(fnc.body.get)) ||
          (ctx.hasFlag("polyMixed") && containsApproxNode(fnc.body.get)))){
          val b = (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(a) => a}
          val newBody = toAPFixedCode(_body, b, ctx.intermediateRanges(fnc.id),
            ctx.intermediateAbsErrors(fnc.id))
          val fncParams = fnc.params.map({
            case ValDef(id) =>
              val actualRange = ctx.intermediateRanges(fnc.id)(Variable(id), emptyPath) +/- ctx.intermediateAbsErrors(fnc.id)(Variable(id), emptyPath)
              val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
              ValDef(id.changeType(APFixedType(b, intBits)))
          })
          val actualRangeResult = ctx.resultRealRanges(fnc.id) +/- ctx.resultAbsoluteErrors(fnc.id)
          val intBitsResult = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRangeResult))

          // TODO: C code with tuples???
          val retType = APFixedType(b, intBitsResult)

          fnc.copy(
            params = fncParams,
            body = Some(newBody),
            returnType = retType)

        } else {
          // number of bits given in precision flag
          val b = (ctx.option[Precision]("precision"): @unchecked) match {
            case FixedPrecision(a) => a
          }

          val newBody = toAPFixedCode(_body, b, ctx.intermediateRanges(fnc.id),
            ctx.intermediateAbsErrors(fnc.id))
          val fncParams = fnc.params.map({
            case ValDef(id) =>
              val actualRange = ctx.intermediateRanges(fnc.id)(Variable(id),
                emptyPath) +/- ctx.intermediateAbsErrors(fnc.id)(Variable(id), emptyPath)
              val parTotal = id.getType match {
                case FinitePrecisionType(FixedPrecision(a)) => a
              }
              val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
              ValDef(id.changeType(APFixedType(parTotal, intBits)))
          })
          // TODO: duplication here, refactor to compute return type given ranges and abs errors
          val actualRangeResult = ctx.resultRealRanges(fnc.id) +/- ctx.resultAbsoluteErrors(fnc.id)
          val intBitsResult = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRangeResult))
          val retTotal = fnc.returnType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
          }
          val retType = APFixedType(retTotal, intBitsResult)

          fnc.copy(
            params = fncParams,
            body = Some(newBody),
            returnType = retType)

        }
      // CASE: mixed-tuning with floats
      } else if (mixedTuning) {
        // types are changed before, since the semantics of type assignments is special
        fnc

      } else if (fixedPrecision && apfixedFormat){
        val b = (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(b) => b}
        val newBody = toAPFixedCode(_body, b, ctx.intermediateRanges(fnc.id),
          ctx.intermediateAbsErrors(fnc.id))
        val fncParams = fnc.params.map({
          case ValDef(id) =>
            val actualRange = ctx.intermediateRanges(fnc.id)(Variable(id), emptyPath) +/- ctx.intermediateAbsErrors(fnc.id)(Variable(id), emptyPath)
            val intBits = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
            ValDef(id.changeType(APFixedType(b, intBits)))
        })
        val actualRangeResult = ctx.resultRealRanges(fnc.id) +/- ctx.resultAbsoluteErrors(fnc.id)
        val intBitsResult = FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRangeResult))

        // TODO: C code with tuples???
        val retType = APFixedType(b, intBitsResult)

        fnc.copy(
          params = fncParams,
          body = Some(newBody),
          returnType = retType)

      } else if (fixedPrecision) {
        assert(!ctx.option[Option[String]]("mixed-precision").isDefined,
          "Mixed-precision codegen is not supported for fixed-points.")

        val b = (uniformPrecisions(fnc.id): @unchecked) match { case FixedPrecision(b) => b}
        val newBody = toFixedPointCode(_body, FixedPrecision(b),
          ctx.intermediateRanges(fnc.id), ctx.intermediateAbsErrors(fnc.id))
        val valDefType = b match {
           // cast precisions between 8, 16 and 32
          case x if x <= 8 => Int16Type
          case x if 8 < x && x <= 16 => Int32Type
          case x if 16 < x && x <= 32 => Int64Type
        }
        val newReturnType = fnc.returnType match {
          case TupleType(args) => TupleType(args.map(x => valDefType))
          case _ => valDefType
        }

        fnc.copy(params = fnc.params.map(vd => ValDef(vd.id.changeType(valDefType))),
          body = Some(newBody),
          returnType = newReturnType)

        // floats with DS
      } else if (ctx.hasFlag("ds") || ctx.hasFlag("ds-naive")) {
        val defaultPrec = uniformPrecisions.getOrElse(fnc.id, Float64)
        //val typeMap = ctx.specInputPrecisions(fnc.id).withDefaultValue(defaultPrec)
        val (vds, tpeSeq) = fnc.params.map(vd => {
          val (tpe, prec) = vd.getType match {
            case RealType => (FinitePrecisionType(defaultPrec), defaultPrec)
            case VectorType(_) => (VectorType(Seq(FinitePrecisionType(defaultPrec))), VectorFloat64) // todo parametrize to have other precisions
            case MatrixType(_) => (MatrixType(Seq(FinitePrecisionType(defaultPrec))), MatrixFloat64) // todo parametrize to have other precisions
          }
          (ValDef(vd.id.changeType(tpe)), vd.id -> prec)
        }).unzip
        val tpeMap = tpeSeq.toMap
        val tpeTACMap = allIDsOf(fnc.body.get).diff(tpeMap.keySet).map(id => id -> precisionWithArity(ValDef(id))._1)
        // // if we have floating-point code, we need to just change the types
        val newBody = assignFloatType(_body, tpeMap ++ tpeTACMap, defaultPrec)._1
        val newReturnType = fnc.returnType match {
          case TupleType(args) => TupleType(args.map(x => FinitePrecisionType(defaultPrec)))
          case VectorType(args) => VectorType(Seq(FinitePrecisionType(defaultPrec))) // todo upd to have mixed precision
          case MatrixType(args) => MatrixType(Seq(FinitePrecisionType(defaultPrec))) // todo upd to have mixed precision
          case _ => FinitePrecisionType(defaultPrec)
        }

        fnc.copy(
          returnType = newReturnType,
          params = vds,
          body = Some(newBody)
        )

      // floats
      } else {
        val defaultPrec = uniformPrecisions.getOrElse(fnc.id, Float64)
        val typeMap = ctx.specInputPrecisions(fnc.id).withDefaultValue(defaultPrec)
        // // if we have floating-point code, we need to just change the types
        val newBody = assignFloatType(_body, typeMap, defaultPrec)._1
        val newReturnType = fnc.returnType match {
          case TupleType(args) => TupleType(args.map(x => FinitePrecisionType(defaultPrec)))
          case _ => FinitePrecisionType(defaultPrec)
        }

        fnc.copy(
          returnType = newReturnType,
          params = fnc.params.map(vd => ValDef(vd.id.changeType(FinitePrecisionType(typeMap(vd.id))))),
          body = Some(newBody)
        )

      }
    }

    val newProgram = Program(prg.id, newDefs)

    val lang = if ((mixedTuning && fixedPrecision) || ctx.hasFlag("apfixed")) "apfixed"
      else ctx.option[String]("lang")


    // write the program to file
    val filename = lang match {
      case "apfixed" => System.getProperty("user.dir")+"/output/" + newProgram.id + ".cpp"
      case _ =>
      System.getProperty("user.dir")+"/output/" + newProgram.id + CodePrinter.suffix(lang)
    }
    ctx.codegenOutput.append(newProgram.id)
    val fstream = new FileWriter(filename)
    val out = new BufferedWriter(fstream)
    CodePrinter(newProgram, ctx, lang, out)

    // for metalibm generated code, we need to inline the generated functions
    if (ctx.hasFlag("metalibm")) {
      out.append("\n// ----- metalibm generated code -----\n")
      for (file <- ctx.metalibmGeneratedFiles) {
        val lines = scala.io.Source.fromFile(file).mkString.replace("void", "static inline void")
        out.append(lines + "\n")
      }

      // delete intermediate files
      val metalibmFolder = new java.io.File(opt.MetalibmPhase.metalibmPath)
      if (metalibmFolder.isDirectory) { // always a dir
        metalibmFolder.listFiles.filter(f =>
          f.getName.contains("_id_sollya_git_devel_") || f.getName.contains("__gappa.gappa") ||
          f.getName.contains("problemdefForDaisy_")
        ).foreach(_.delete())
      }
    }
    out.close()
    (ctx, newProgram)
  }

  private def reTuple(e: Expr, resIds: Seq[Identifier]): Expr = e match {
    // found the first inserted result ID
    case Let(id, _, _) if (resIds.contains(id)) =>
      // replace with just the tuple
      Tuple(reconstructTuple(e))

    case Let(id, value, body) => Let(id, value, reTuple(body, resIds))

    case _ => e

  }

  private def reconstructTuple(e: Expr): Seq[Expr] = e match {
    // last one, i.e. base case
    case Let(_, Variable(id), Variable(_)) =>
      Seq(Variable(id))

    case Let(_, Variable(id), body) =>
      reconstructTuple(body) :+ Variable(id)
  }

  private def precisionWithArity(vd: ValDef, defaultPrec: Precision = Float64): (Precision, TypeTree) = vd.getType match {
    case RealType => (defaultPrec, FinitePrecisionType(defaultPrec))
    case t@FinitePrecisionType(p) => (p, t)
    case t@Int32Type => (Int32, t)
    case VectorType(_) => (VectorFloat64, VectorType(Seq(FinitePrecisionType(defaultPrec)))) // todo parametrize to have other precisions
    case MatrixType(_) => (MatrixFloat64, MatrixType(Seq(FinitePrecisionType(defaultPrec)))) // todo parametrize to have other precisions
  }

  private def typeFromPrecision(pre: Precision, defaultPrec: Precision = Float64): TypeTree = pre match {
    case Int32 => Int32Type
    case VectorFloat64 => VectorType(Seq(FinitePrecisionType(Float64))) // todo parametrize to have other precisions
    case MatrixFloat64 => MatrixType(Seq(FinitePrecisionType(Float64))) // todo parametrize to have other precisions
    case x@FloatPrecision(_) => FinitePrecisionType(x)
    case x@FixedPrecision(_) => FinitePrecisionType(x)
    case x => throw DaisyFatalError(Some(s"Unknown precision $x"))
  }

  private def assignFloatType(e: Expr, tpeMap: Map[Identifier, Precision],
    defaultPrecision: Precision): (Expr, Precision) = e match {

    // these are needed to keep DS literals DS (not replace them with Variable)
    case VectorLiteral(id) =>
      val (_, tpe) = precisionWithArity(ValDef(id))
      (VectorLiteral(id.changeType(tpe)), tpeMap(id))
    case MatrixLiteral(id) =>
      val (_, tpe) = precisionWithArity(ValDef(id))
      (MatrixLiteral(id.changeType(tpe)), tpeMap(id))
    case Variable(id) =>
      if (id.getType == RealType)
        (Variable(id.changeType(FinitePrecisionType(tpeMap(id)))), tpeMap(id))
      else {
        val (prec, _) = precisionWithArity(ValDef(id), defaultPrecision)
        (Variable(id), prec)
      }

    case x @ RealLiteral(r) => if(x.stringValue == null){
      (FinitePrecisionLiteral(r, defaultPrecision, r.toString()), defaultPrecision)
    } else {
      (FinitePrecisionLiteral(r, defaultPrecision,x.stringValue), defaultPrecision)
    }
    case x @ FinitePrecisionLiteral(_, prec, _) => (x, prec)
    case x @ Int32Literal(_) => (x, Int32)
    case x@Cast(n, Int32Type) =>
      val (tmpN, _) = assignFloatType(n, tpeMap, defaultPrecision)
      (Cast(tmpN, Int32Type), Int32)
    // todo handle all cases where one of the arguments is Int (not a literal, but expression?)

    case VectorElement(v, index) =>
      val (tmpV, tmpPre) = assignFloatType(v, tpeMap, defaultPrecision)
      val pre = tmpPre match {
        case VectorFloat64 => defaultPrecision
        case _ => defaultPrecision // todo other options? when parametrized, will be able to get the argument
      }
      val (tmpIndex, _) = assignFloatType(index, tpeMap, defaultPrecision)
      (VectorElement(tmpV, tmpIndex), pre)

    case MatrixElement(m, irow, icol) =>
      val (tmpM, tmpPre) = assignFloatType(m, tpeMap, defaultPrecision)
      val (tmpIRow, _) = assignFloatType(irow, tpeMap, defaultPrecision)
      val (tmpICol, _) = assignFloatType(icol, tpeMap, defaultPrecision)
      val pre = tmpPre match {
        case MatrixFloat64 => defaultPrecision
        case _ => defaultPrecision // todo other options? when parametrized, will be able to get the argument
      }
      (MatrixElement(tmpM, tmpIRow, tmpICol), pre)

    case x@RowOfMatrix(m, row) =>
      // todo actually use tmpPre instead of VectorFloat64
      val (tmpM, tmpPre) = assignFloatType(m, tpeMap, defaultPrecision)
      val (tmpIRow, _) = assignFloatType(row, tpeMap, defaultPrecision)
      (RowOfMatrix(tmpM, tmpIRow), VectorFloat64)

    case x@SubVector(v, from, to) =>
      // todo actually use tmpPre instead of VectorFloat64
      val (tmpV, tmpPre) = assignFloatType(v, tpeMap, defaultPrecision)
      val (ffrom, _) = assignFloatType(from, tpeMap, defaultPrecision)
      val (fto, _) = assignFloatType(to, tpeMap, defaultPrecision)
      (SubVector(tmpV, ffrom, fto), VectorFloat64)

    case PadVector(v, padSize) =>
      // todo actually use tmpPre instead of VectorFloat64
      val (tmpV, tmpPre) = assignFloatType(v, tpeMap, defaultPrecision)
      val (fpadSz, _) = assignFloatType(padSize, tpeMap, defaultPrecision)
      (PadVector(tmpV, fpadSz), VectorFloat64)

    case PadMatrix(m, padRows, padCols) =>
      // todo actually use tmpPre instead of MatrixFloat64
      val (tmpM, tmpPre) = assignFloatType(m, tpeMap, defaultPrecision)
      val (fpadRows, _) = assignFloatType(padRows, tpeMap, defaultPrecision)
      val (fpadCols, _) = assignFloatType(padCols, tpeMap, defaultPrecision)
      (PadMatrix(tmpM, fpadRows, fpadCols), MatrixFloat64)

      // the commented out cases are covered by the DSOperations
    //case x@ZipVectors(lhs, rhs) =>
    //  // todo actually use tmpPre instead of MatrixFloat64
    //  val (fLhs, tmpPre) = assignFloatType(lhs, tpeMap, defaultPrecision)
    //  val (fRhs, _) = assignFloatType(rhs, tpeMap, defaultPrecision)
    //  (ZipVectors(fLhs, fRhs), MatrixFloat64)
    //
    //case x@FlipUpsideDown(m) =>
    //  // todo actually use tmpPre instead of MatrixFloat64
    //  val (tmpM, tmpPre) = assignFloatType(m, tpeMap, defaultPrecision)
    //  (FlipUpsideDown(tmpM), MatrixFloat64)
    //
    //case x@FlipLeftToRight(m) =>
    //  // todo actually use tmpPre instead of MatrixFloat64
    //  val (tmpM, tmpPre) = assignFloatType(m, tpeMap, defaultPrecision)
    //  (FlipLeftToRight(tmpM), MatrixFloat64)

    //case x@AppendElement(ds, el) => ??? // if isVector(v) =>
    //case x@PrependElement(v, el) => ???
    //case x@Concat(lhs, rhs) => ???
    case x@VectorFromList(list, sz) =>
      val (es, ps) = list.unzip(assignFloatType(_, tpeMap, defaultPrecision))
      //val prec = getUpperBound(ps: _*) // todo use as parameter for VectorFloat
      (VectorFromList(es, sz), VectorFloat64)

    case x@MatrixFromLists(listOflists, numRows, numCols) =>
      // todo for now ignore precision of the individual elements
      val es = listOflists.map(ls => ls.unzip(assignFloatType(_, tpeMap, defaultPrecision))._1)
      //val prec = getUpperBound(ps: _*) // todo use as parameter for VectorFloat
      (MatrixFromLists(es, numRows, numCols), MatrixFloat64)

    //case x@EveryNthVector(v, Int32Literal(n), Int32Literal(from)) => ???
    //case x@EveryNthMatrix(m, Int32Literal(n), Int32Literal(from)) => ???
    //case x@MaxOf(v) => ???
    //case x@MinOf(v) => ???
    case SizeLength(ds) =>
      val (fds, _) = assignFloatType(ds, tpeMap, defaultPrecision)
      (SizeLength(fds), Int32)

    case SizeNumRows(ds) =>
      val (fds, _) = assignFloatType(ds, tpeMap, defaultPrecision)
      (SizeNumRows(fds), Int32)
    case x@SizeNumCols(ds) =>
      val (fds, _) = assignFloatType(ds, tpeMap, defaultPrecision)
      (SizeNumRows(fds), Int32)

    case FunctionInvocation(fdId, params, args, returnType) =>
      val (_, tpe) = precisionWithArity(ValDef(fdId), defaultPrecision)
      val fp_fdId = fdId.changeType(tpe)
      val (fparams, tpeParamMap) = params.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fargs, tpeArgMap) = args.map(arg => {assignFloatType(arg, tpeMap, defaultPrecision)
      }).unzip
      val (newType, newPrec) = returnType match {
        case RealType => (FinitePrecisionType(defaultPrecision), defaultPrecision)
        case t@FinitePrecisionType(p) => (t, p)
        case VectorType(_) => (VectorType(Seq(FinitePrecisionType(VectorFloat64))), defaultPrecision)
        case MatrixType(_) => (MatrixType(Seq(FinitePrecisionType(MatrixFloat64))), defaultPrecision)
      }
      (FunctionInvocation(fp_fdId, fparams, fargs, newType), newPrec)


    case x@FilterIter(ds, ld@Lambda(args, body)) =>
      val (fds, pre) = assignFloatType(ds, tpeMap, defaultPrecision)
      val (fargs, tpeArgMap) = args.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fbody, _) = assignFloatType(body, tpeMap ++ tpeArgMap, defaultPrecision)
      (FilterIter(fds, Lambda(fargs, fbody)), pre)

    case x@MapIter(ds, Lambda(args, body)) =>
      val (fds, pre) = assignFloatType(ds, tpeMap, defaultPrecision)
      val (fargs, tpeArgMap) = args.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fbody, _) = assignFloatType(body, tpeMap ++ tpeArgMap, defaultPrecision)
      (MapIter(fds, Lambda(fargs, fbody)), pre)

    case Sum(v, init) if isVector(v) =>
      // transform sum to simple fold
      val acc = ValDef(FreshIdentifier("_tmp_sum_acc", init.getType))
      val x = ValDef(FreshIdentifier("_tmp_sum_x", init.getType))
      val args = Seq(acc, x)
      val body = Plus(Variable(acc.id), Variable(x.id))
      assignFloatType(FoldIter(v, init, Lambda(args, body)), tpeMap, defaultPrecision)

    case Sum(m, init) if isMatrix(m) =>
      // transform sum to simple fold
      val acc = ValDef(FreshIdentifier("_tmp_sum_acc", init.getType))
      val x = ValDef(FreshIdentifier("_tmp_sum_x", init.getType))
      val args = Seq(acc, x)
      val body = Plus(Variable(acc.id), Variable(x.id))
      assignFloatType(FoldElemsIter(m, init, Lambda(args, body)), tpeMap, defaultPrecision)

    case x@FoldIter(ds, init, Lambda(args, body)) =>
      val (fds, pre) = assignFloatType(ds, tpeMap, defaultPrecision)
      val (finit, init_pre) = assignFloatType(init, tpeMap, defaultPrecision)
      val (fargs, tpeArgMap) = args.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fbody, _) = assignFloatType(body, tpeMap ++ tpeArgMap, defaultPrecision)
      (FoldIter(fds, finit, Lambda(fargs, fbody)), init_pre)

    case x@FoldElemsIter(ds, init, Lambda(args, body)) =>
      val (fds, _) = assignFloatType(ds, tpeMap, defaultPrecision)
      val (finit, init_pre) = assignFloatType(init, tpeMap, defaultPrecision)
      val (fargs, tpeArgMap) = args.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fbody, _) = assignFloatType(body, tpeMap ++ tpeArgMap, defaultPrecision)
      (FoldElemsIter(fds, finit, Lambda(fargs, fbody)), init_pre)

    case x@SlideReduceIter(ds, size, step, Lambda(args, body)) =>
      val (fds, _) = assignFloatType(ds, tpeMap, defaultPrecision)
      val (fsz, _) = assignFloatType(size, tpeMap, Int32)
      val (fstep, _) = assignFloatType(step, tpeMap, Int32)
      val (fargs, tpeArgMap) = args.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fbody, bodypre) = assignFloatType(body, tpeMap ++ tpeArgMap, defaultPrecision)
      // final precision should be a vector/matrix of the fbody return type
      val (pre, _) = fds.getType match {
        case VectorType(_) => (VectorFloat64, VectorType(Seq(FinitePrecisionType(bodypre)))) // todo parametrize to have other precisions
        case MatrixType(_) => (MatrixFloat64, MatrixType(Seq(FinitePrecisionType(bodypre)))) // todo parametrize to have other precisions
      }
      (SlideReduceIter(fds, fsz, fstep, Lambda(fargs, fbody)), pre)

    case x@EnumRowsAndMap(ds, Lambda(args, body)) =>
      val (fds, _) = assignFloatType(ds, tpeMap, defaultPrecision)
      val (fargs, tpeArgMap) = args.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fbody, pre) = assignFloatType(body, tpeMap ++ tpeArgMap, defaultPrecision)
      (EnumRowsAndMap(fds, Lambda(fargs, fbody)), pre)

    case x@EnumSlideFlatMap(ds, size, Lambda(args, body)) =>
      val (fds, _) = assignFloatType(ds, tpeMap, defaultPrecision)
      val (fsz, _) = assignFloatType(size, tpeMap, Int32)
      val (fargs, tpeArgMap) = args.map(vd => {
        val (prec_arg, tpe) = precisionWithArity(vd)
        (ValDef(vd.id.changeType(tpe)), vd.id -> prec_arg)
      }).unzip
      val (fbody, pre) = assignFloatType(body, tpeMap ++ tpeArgMap, defaultPrecision)
      (EnumSlideFlatMap(fds, fsz, Lambda(fargs, fbody)), pre)

    //case x@CrossProduct(lhs, rhs) => ??? // need variations of it?
    //case x@Sum(ds, init) => ???

    case DSOperations(es_old, recons) =>
      val (es, ps) = es_old.unzip(assignFloatType(_, tpeMap, defaultPrecision))

      val prec = getUpperBound(ps: _*)
      (recons(es), prec)

    case ArithOperator(es_old, recons) =>
      val (es, ps) = es_old.unzip(assignFloatType(_, tpeMap, defaultPrecision))

      val prec = getUpperBound(ps: _*)
      (recons(es), prec)

    case Let(id, value, body) =>
      val (eValue, valuePrec) = assignFloatType(value, tpeMap, defaultPrecision)
      val (eBody, bodyPrec) = assignFloatType(body, tpeMap, defaultPrecision)

      if (id.getType == RealType && value.getType == Int32Type) {
        // when assigning an int to a variable, e.g. n = vector.size
        (Let(id.changeType(Int32Type), eValue, eBody), bodyPrec)
      } else {
        val idPrec = tpeMap(id)

        val tp = typeFromPrecision(idPrec)
        if (idPrec >= valuePrec) {
          (Let(id.changeType(tp), eValue, eBody), bodyPrec)
        } else {
          val newValue = Cast(eValue, tp)
          (Let(id.changeType(tp), newValue, eBody), bodyPrec)
        }
      }

    case IfExpr(cond, thenn, elze) =>
      val (eCond, _) = assignFloatType(cond, tpeMap, defaultPrecision)
      val (eThen, thenPrec) = assignFloatType(thenn, tpeMap, defaultPrecision)
      val (eElse, elsePrec) = assignFloatType(elze, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(thenPrec, elsePrec): _*)

      (IfExpr(eCond, eThen, eElse), prec)

    case Tuple(args) =>
      (Tuple(args.map(assignFloatType(_, tpeMap, defaultPrecision)).unzip._1),
        defaultPrecision)
        // tuples can only occur at the very end, hence the returned precision
        // doesn't matter

    case GreaterThan(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (GreaterThan(eLeft, eRight), prec)

    case GreaterEquals(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (GreaterEquals(eLeft, eRight), prec)

    case LessThan(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (LessThan(eLeft, eRight), prec)

    case LessEquals(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (LessEquals(eLeft, eRight), prec)

    case Equals(l, r) =>
      val (eLeft, leftPrec) = assignFloatType(l, tpeMap, defaultPrecision)
      val (eRight, rightPrec) = assignFloatType(r, tpeMap, defaultPrecision)

      val prec = getUpperBound(Seq(leftPrec, rightPrec): _*)
      (Equals(eLeft, eRight), prec)
  }

  /*
   * Expects code to be already in SSA form.
   * @param format the (uniform) fixed-point precision to use
   */
  def toFixedPointCode(expr: Expr, format: FixedPrecision, intermRanges: Map[(Expr, PathCond), Interval],
    intermAbsErrors: Map[(Expr, PathCond), Rational]): Expr = {
    val newType = format match {
      case FixedPrecision(x) if x <= 8 => Int16Type
      case FixedPrecision(x) if 8 < x && x <= 16 => Int32Type
      case FixedPrecision(x) if 16 < x && x <= 32 => Int64Type
      //case FixedPrecision(x) if x > 32 => Long // todo ???
    }

    @inline
    def getFractionalBits(e: Expr, path: PathCond): Int = {
      // the overall interval is the real-valued range +/- absolute errors
      val interval = intermRanges(e, path) +/- intermAbsErrors(e, path)
      format.fractionalBits(interval)
    }


    def _toFPCode(e: Expr, path: PathCond): Expr = (e: @unchecked) match {
      case x @ Variable(id) => Variable(id.changeType(newType))

      case RealLiteral(r) => // TODO: translate constant
        val f = format.fractionalBits(r)
        format match {
          case FixedPrecision(x) if x <= 8 => Int16Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
          case FixedPrecision(x) if 8 < x && x <= 16 => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
          case FixedPrecision(x) if 16 < x && x <= 32 => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
        }

      case UMinus(t) => UMinus(_toFPCode(t, path))

      case Sqrt(t) =>
        throw new Exception("Sqrt is not supported for fixed-points!")

      case x @ Plus(lhs, rhs) =>
        val fLhs = getFractionalBits(lhs, path)
        val fRhs = getFractionalBits(rhs, path)

        // determine how much to shift left or right
        val fAligned = math.max(fLhs, fRhs)
        val newLhs =
          if (fLhs < fAligned) {
            LeftShift(_toFPCode(lhs, path), (fAligned - fLhs))
          } else {
            _toFPCode(lhs, path)
          }
        val newRhs =
          if (fRhs < fAligned) {
            LeftShift(_toFPCode(rhs, path), (fAligned - fRhs))
          } else {
            _toFPCode(rhs, path)
          }

        // fractional bits result
        val fRes = getFractionalBits(x, path)
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
        val fLhs = getFractionalBits(lhs, path)
        val fRhs = getFractionalBits(rhs, path)

        // determine how much to shift left or right
        val fAligned = math.max(fLhs, fRhs)
        val newLhs =
          if (fLhs < fAligned) {
            LeftShift(_toFPCode(lhs, path), (fAligned - fLhs))
          } else {
            _toFPCode(lhs, path)
          }
        val newRhs =
          if (fRhs < fAligned) {
            LeftShift(_toFPCode(rhs, path), (fAligned - fRhs))
          } else {
            _toFPCode(rhs, path)
          }

        // fractional bits result
        val fRes = getFractionalBits(x, path)
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

        val mult = Times(_toFPCode(lhs, path), _toFPCode(rhs, path))
        val fMult = getFractionalBits(lhs, path) + getFractionalBits(rhs, path)

        // fractional bits result
        val fRes = getFractionalBits(x, path)
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
        val fLhs = getFractionalBits(lhs, path)
        val fRhs = getFractionalBits(rhs, path)

        val fRes = getFractionalBits(x, path)
        val shift = fRes + fRhs - fLhs
        Division(LeftShift(_toFPCode(lhs, path), shift), _toFPCode(rhs, path))

      case Let(id, value, body) =>
        Let(id.changeType(newType), _toFPCode(value, path), _toFPCode(body, path))

      case x @ IfExpr(cond, thenn, elze) =>
        IfExpr(_toFPCode(cond, path), _toFPCode(thenn, path :+ cond),
          _toFPCode(elze, path :+ lang.TreeOps.negate(cond)))

      case Tuple(args) => Tuple(args.map(_toFPCode(_, path)))

      case GreaterThan(l, r) => GreaterThan(_toFPCode(l, path), _toFPCode(r, path))
      case GreaterEquals(l, r) => GreaterEquals(_toFPCode(l, path), _toFPCode(r, path))
      case LessThan(l, r) => LessThan(_toFPCode(l, path), _toFPCode(r, path))
      case LessEquals(l, r) => LessEquals(_toFPCode(l, path), _toFPCode(r, path))

      case ApproxPoly(original, arg, approxFncId, errBudget) => ApproxPoly(original, arg, approxFncId.changeType(newType), errBudget)
    }

    _toFPCode(expr, emptyPath)
  }

  /*
   * Generates code for the Vivado HLS hardware synthesis tool with the ap_fixed data type.
   * Expects code to be already in SSA form.
   */
  def toAPFixedCode(expr: Expr, totalBits: Int, intermRanges: Map[(Expr, PathCond), Interval],
    intermAbsErrors: Map[(Expr, PathCond), Rational]): Expr = {

    @inline
    def getIntegerBits(e: Expr, path: PathCond): Int = {
      // the overall interval is the real-valued range +/- absolute errors

      var actualRange = intermRanges.getOrElse((e, path), e match
      {
        case FinitePrecisionLiteral(r, _, stringVal) => intermRanges(RealLiteral(r, stringVal), path)
        case x => throw new Exception(s"No range computed for $x")
      })
      val absError = intermAbsErrors.getOrElse((e, path), e match
        {
          case FinitePrecisionLiteral(r, _, stringVal) => intermAbsErrors(RealLiteral(r, stringVal), path)
          case x => throw new Exception(s"No absolute roundoff error computed for $x")
        })
      actualRange = actualRange +/- absError
      FixedPrecision.integerBitsNeeded(Interval.maxAbs(actualRange))
    }

    def _toFPCode(e: Expr, path: PathCond): Expr = (e: @unchecked) match {
      case x @ Variable(id) => x

      case x @ RealLiteral(r) => x  // constants are handled automatically?

      case x @ FinitePrecisionLiteral(r,prec,s)  => x

      case x @ Cast(ex,typ) =>
        val bits = typ match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        ex match {
          case Variable(_) => Cast(ex, APFixedType(bits,getIntegerBits(x, path)))
          case UMinus(Variable(_)) => Cast(ex, APFixedType(bits,getIntegerBits(x, path)))
          case _ => Cast(_toFPCode(ex, path), APFixedType(bits,getIntegerBits(ex, path)))
        }

      case x @ ArithOperator(Seq(t: Expr), recons) =>
        recons(Seq(_toFPCode(t, path)))

      case x @ ArithOperator(Seq(lhs: Expr, rhs: Expr), recons) =>
        val rhsFP = _toFPCode(rhs, path)
        val lhsFP = _toFPCode(lhs, path)
        recons(Seq(lhsFP, rhsFP))

      case Let(id, value, body) =>
        val bits = id.getType match {
            case FinitePrecisionType(FixedPrecision(a)) => a
            case _ => totalBits
        }
        val idType = APFixedType(bits, getIntegerBits(value, path))
        Let(id.changeType(idType), _toFPCode(value, path),
          _toFPCode(body, path))

      case x @ IfExpr(cond, thenn, elze) =>
        IfExpr(_toFPCode(cond, path), _toFPCode(thenn, path :+ cond),
               _toFPCode(elze, path :+ lang.TreeOps.negate(cond)))

      case x @ GreaterThan(l, r) => GreaterThan(_toFPCode(l, path), _toFPCode(r, path))
      case x @ GreaterEquals(l, r) => GreaterEquals(_toFPCode(l, path), _toFPCode(r, path))
      case x @ LessThan(l, r) => LessThan(_toFPCode(l, path), _toFPCode(r, path))
      case x @ LessEquals(l, r) => LessEquals(_toFPCode(l, path), _toFPCode(r, path))

      case x @ ApproxPoly(original, arg, approxFncId, errBudget) =>
        val funType = APFixedType(totalBits, getIntegerBits(x, path))
        ApproxPoly(original, arg, approxFncId.changeType(funType), errBudget)

    }
    _toFPCode(expr, emptyPath)
  }

  /*
   * Expects code to be already in SSA form.
   * Expects the types to be attached to the tree and expects the program
   * to be in SSA form.
   * TODO: check that this is sound, I think the range should be finite-precision
   * and not real-valued as it is most likely here...
   */
  def toMixedFixedPointCode(expr: Expr, path: PathCond, rangeMap: Map[(Expr, PathCond), Interval]): Expr = (expr: @unchecked) match {

    case x @ Variable(id) => id.getType match {
      case FinitePrecisionType(FixedPrecision(x)) if 8 < x && x <= 16 => Variable(id.changeType(Int32Type))
      case FinitePrecisionType(FixedPrecision(x)) if 16 < x && x <= 32 => Variable(id.changeType(Int64Type))
    }

    case FinitePrecisionLiteral(r, prec @ FixedPrecision(_), strVal) =>
      val f = prec.fractionalBits(r)
      prec match {
        case FixedPrecision(x) if 8 < x && x <= 16 => Int32Literal((r * Rational.fromDouble(math.pow(2, f))).roundToInt)
        case FixedPrecision(x) if 16 < x && x <= 32 => Int64Literal((r * Rational.fromDouble(math.pow(2, f))).roundToLong)
      }

    // TODO: we may shift too much, i.e. (x >> 32) << 3, which can be optmixed/
    // cleaned up after the fact.
    // necessarily a down cast from 32 bit
    case Cast(e, FinitePrecisionType(FixedPrecision(16))) =>
      Cast(RightShift(toMixedFixedPointCode(e, path, rangeMap), 16), Int32Type)

    // necessarily an up cast from 16 bit, and not redundant (if all went well in mixed-opt)
    case Cast(e, FinitePrecisionType(FixedPrecision(32))) =>
      Cast(LeftShift(toMixedFixedPointCode(e, path, rangeMap), 16), Int64Type)

    case UMinus(t) => UMinus(toMixedFixedPointCode(t, path, rangeMap))

    case Sqrt(t) =>
      throw new Exception("Sqrt is not supported for fixed-points!")


    case x @ Plus(lhs, rhs) =>
      // TODO: is there some better way?!
      val lhsPrec = lhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      val rhsPrec = rhs.getType.asInstanceOf[FinitePrecisionType].prec.asInstanceOf[FixedPrecision]
      // fractional bits from lhs
      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs, path))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs, path))

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toMixedFixedPointCode(lhs, path, rangeMap), (fAligned - fLhs))
        else toMixedFixedPointCode(lhs, path, rangeMap)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toMixedFixedPointCode(rhs, path, rangeMap), (fAligned - fRhs))
        else toMixedFixedPointCode(rhs, path, rangeMap)

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
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
      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs, path))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs, path))

      // determine how much to shift left or right
      val fAligned = math.max(fLhs, fRhs)
      val newLhs =
        if (fLhs < fAligned) LeftShift(toMixedFixedPointCode(lhs, path, rangeMap), (fAligned - fLhs))
        else toMixedFixedPointCode(lhs, path, rangeMap)
      val newRhs =
        if (fRhs < fAligned) LeftShift(toMixedFixedPointCode(rhs, path, rangeMap), (fAligned - fRhs))
        else toMixedFixedPointCode(rhs, path, rangeMap)

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
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

      val mult = Times(toMixedFixedPointCode(lhs, path, rangeMap), toMixedFixedPointCode(rhs, path, rangeMap))
      val fMult = lhsPrec.fractionalBits(rangeMap(lhs, path)) +
        rhsPrec.fractionalBits(rangeMap(rhs, path))

      // fractional bits result
      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
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

      val fLhs = lhsPrec.fractionalBits(rangeMap(lhs, path))
      val fRhs = rhsPrec.fractionalBits(rangeMap(rhs, path))

      val fRes = getUpperBound(lhsPrec, rhsPrec).asInstanceOf[FixedPrecision].fractionalBits(rangeMap(x, path))
      val shift = fRes + fRhs - fLhs
      Division(LeftShift(toMixedFixedPointCode(lhs, path, rangeMap), shift), toMixedFixedPointCode(rhs, path, rangeMap))

    case Let(id, value, body) =>
      val newId = id.changeType(id.getType match {
        case FinitePrecisionType(FixedPrecision(x)) if 8 < x && x <= 16 => Int32Type
        case FinitePrecisionType(FixedPrecision(x)) if 16 < x && x <= 32 => Int64Type
      })
      Let(newId, toMixedFixedPointCode(value, path, rangeMap), toMixedFixedPointCode(body, path, rangeMap))

    case x @ ApproxPoly(original, arg, approxFncId, errBudget) =>
      val newType = approxFncId.getType match {
        case FinitePrecisionType(FixedPrecision(x)) if 8 < x && x <= 16 => Int32Type
        case FinitePrecisionType(FixedPrecision(x)) if 16 < x && x <= 32 => Int64Type
      }
      ApproxPoly(original, arg, approxFncId.changeType(newType), errBudget)
  }

}
