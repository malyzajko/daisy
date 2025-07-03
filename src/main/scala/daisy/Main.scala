// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import java.io.File
import daisy.tools.FinitePrecision._
import lang.Trees.Program

object Main {

  // TODO: these should be split into different types such that help
  // is actually readable
  val globalOptions: Set[CmdLineOption[Any]] = Set(
    FlagOption(
      "help",
      "Show this message"),
    FlagOption(
      "silent",
      "Don't print anything, except for results."),
    MultiChoiceOption(
      "debug",
      DebugSections.all.map(s => s.name -> s).toMap,
      "For which sections to print debug info"),
    FlagOption(
      "dynamic",
      "Run dynamic analysis"),
    FlagOption(
      "codegen",
      "Generate code (as opposed to just doing analysis)"),
    FlagOption(
      "three-address",
      "Transform code to three-address code prior to analysis."),
    FlagOption(
      "rewrite",
      "Rewrite expression to improve accuracy"),
    MultiStringOption(
      "functions",
      List("f1", "f2"),
      "Which functions to consider"),
    FlagOption(
      "print-tough-smt-calls",
      "If enabled, will print those SMT queries to file which take longer"),
    StringChoiceOption(
      "solver",
      Set("dReal", "z3"),
      "z3",
      "smt solver to use for smt range analysis"),
    ChoiceOption(
      "analysis",
      Map("dataflow" -> analysis.DataflowPhase, "opt" -> analysis.FPTaylorPhase,
          "relative" -> analysis.RelativeErrorPhase),
      "dataflow",
      "Which analysis method to use"),
    FlagOption(
      "subdiv",
      "Apply subdivision to absolute error computation."
    ),
    ChoiceOption(
      "precision",
      Map("Float16" -> Float16, "Float32" -> Float32, "Float64" -> Float64,
        "Quad" -> DoubleDouble, "QuadDouble" -> QuadDouble) ++
        (1 to 64).map(x => ("Fixed" + x -> FixedPrecision(x))),
      "Float64",
      "(Default, uniform) precision to use"),
    StringChoiceOption(
      "rangeMethod",
      Set("affine", "interval", "smt", "intervalMPFR", "affineMPFR"),
      "interval",
      "Method for range analysis"),
    FlagOption(
      "noRoundoff",
      "Do not track roundoff errors"),
    FlagOption(
      "noInitialErrors",
      "Do not track initial errors specified by user"),
    FlagOption(
      "pow-roll",
      "Roll products, e.g. x*x*x -> pow(x, 3)"
    ),
    FlagOption(
      "pow-unroll",
      "Unroll products, e.g. pow(x, 3) => x*x*x"
    ),
    StringOption(
      "mixed-precision",
      """File with type assignment for all variables.
        The format is the following:
        function_name = {
          variable_name_1: prec_1
          variable_name_2: prec_2
          ... }
        function_name_2 = {
          variable_name_i: prec_i }

        The file can also only give a partial precision map."""),
    FlagOption(
      "denormals",
      "Include parameter for denormals in the FP abstraction (for optimization-based approach only)."),


    FlagOption("mixed-cost-eval", "Mixed-precision cost function evaluation experiment"),
    FlagOption("mixed-exp-gen", "Mixed-precision experiment generation"),
    FlagOption("mixed-tuning", "Perform mixed-precision tuning"),
    FlagOption(
      "approx",
      "Replaces expensive transcendental function calls with its approximations"
    ),
    StringOption(
      "spec",
      "Specification file with intervals for input variables and target error."),
    StringChoiceOption(
      "cost",
      Set("area", "ml", "combined"),
      "area",
      "Cost function for mixed-tuning and approximation phases."),

    FlagOption("metalibm", "approximate an elementary function from Metalibm"),
    FlagOption("benchmarking", "generates the benchmark file"),
    FlagOption("modularRoundOffEval", "modular roundoff error evaluation"),
    FlagOption("FPTaylor", "evaluates the Taylor abstract using FPTaylor approach"),
    FlagOption("FPTaylorOriginal", "evaluates the Taylor abstract using FPTaylor approach"),
    FlagOption("inlineTranslate", "inlines functions and generates code in specified format by the 'lang' flag"),
    FlagOption("print-ast", "prints the AST of a parsed program"),
    FlagOption("unroll", "unrolls all loops over DS [WARN] only used with --print-ast at the moment"),
    FlagOption("ds", "applies abstraction to data structures and computes ranges, errors"),
    FlagOption("ds-naive", "naive analysis of programs with data structures (ranges, errors)"),

    // Omelette settings
    NumOption(
      "omelettePrecision",
      128,
      "number of bits used for interval evaluation; higher is slower but more accurate"
    ),
    NumOption(
      "omeletteIterLimit",
      30,
      "maximum number of iterations to run the simplification for; higher is slower but more accurate"
    ),
    StringChoiceOption(
      "omeletteCostFn", 
      Set("astSize", "width", "magnitude", "widthFirst", "magnitudeFirst"), 
      "widthFirst", 
      "cost function used when extracting an expression", 
    ) 
    
  )

  lazy val allPhases: Set[DaisyPhase] = Set(
    analysis.SpecsProcessingPhase,
    transform.CompilerOptimizationPhase,
    analysis.AbsErrorPhase,
    analysis.RangePhase,
    analysis.DataflowPhase,
    analysis.DSAbstractionPhase,
    analysis.DSNaivePhase,
    analysis.RelativeErrorPhase,
    analysis.TaylorErrorPhase,
    analysis.DataflowSubdivisionPhase,
    analysis.TaylorAbstractPhase,
    analysis.FPTaylorPhase,
    analysis.FPTaylorPhaseOriginal,
    analysis.ModularRoundOffErrorEvalPhase,
    analysis.FunctionCallPhase,
    backend.CodeGenerationPhase,
    transform.TACTransformerPhase,
    transform.PowTransformerPhase,
    analysis.DynamicPhase,
    opt.RewritingOptimizationPhase,
    transform.ConstantTransformerPhase,
    opt.MixedPrecisionOptimizationPhase,
    experiment.MixedPrecisionExperimentGenerationPhase,
    experiment.CostFunctionEvaluationExperiment,
    backend.InfoPhase,
    frontend.ExtractionPhase,
    frontend.CExtractionPhase,
    opt.ApproxPhase,
    opt.MetalibmPhase,
    //transform.ReassignElemFuncPhase,
    experiment.BenchmarkingPhase,
    transform.DecompositionPhase,
    transform.UnrollPhase
  )

  // all available options from all phases
  private val allOptions: Set[CmdLineOption[Any]] =
    globalOptions ++ allPhases.flatMap(_.definedOptions)

  var ctx: Context = null

  def interfaceMain(args: Array[String]): Option[Context] = {
    processOptions(args.toList) match {
      case Some(new_ctx) =>
        ctx = new_ctx
        ctx.timers.total.start()
        val pipeline = computePipeline(ctx)
        ctx.reporter.info("\n************ Starting Daisy ************")
        try { // for debugging it's better to have these off.
          pipeline.run(ctx, Program(null, Nil))
        } catch {
          //case tools.DivisionByZeroException(msg) =>
          //  ctx.reporter.warning(msg)
          //case tools.DenormalRangeException(msg) =>
          //  ctx.reporter.warning(msg)
          //case tools.OverflowException(msg) =>
          //  ctx.reporter.warning(msg)
          //case e: java.lang.UnsatisfiedLinkError =>
          //  ctx.reporter.warning("A library could not be loaded: " + e)
          //case tools.NegativeSqrtException(msg) =>
          //  ctx.reporter.warning(msg)
          //case tools.ArcOutOfBoundsException(msg) =>
          //  ctx.reporter.warning(msg)
          // case e: DaisyFatalError =>
          //   ctx.reporter.info(f"Something really bad happened. Cannot continue.")
          case e: Exception =>
            val msg = f"${e.getClass.getSimpleName}: ${e.getMessage}"
            ctx.reporter.info(f"Something really bad happened. Cannot continue: $msg")
            if ((ctx.options.contains("ds") || ctx.options.contains("ds-naive")) && ctx.options.contains("results-csv")) {
              val (_,prg) = frontend.ExtractionPhase.runPhase(ctx, ctx.originalProgram)
              backend.InfoPhase.runPhase(ctx.copy(errMsg = Some(msg)), prg)
            }
          case t : Throwable =>
            val msg = f"${t.getClass.getSimpleName}: ${t.getMessage}"
            ctx.reporter.info(f"Something really bad happened. Cannot continue: $msg")
            if ((ctx.options.contains("ds") || ctx.options.contains("ds-naive")) && ctx.options.contains("results-csv")) {
              val (_,prg) = frontend.ExtractionPhase.runPhase(ctx, ctx.originalProgram)
              backend.InfoPhase.runPhase(ctx.copy(errMsg = Some(msg)), prg)
            }
        }
        ctx.timers.get("total").stop()
        ctx.reporter.info("time: \n" + ctx.timers.toString)
        Option(ctx)
      case None =>
        None
    }
  }

  def main(args: Array[String]): Unit = {
    interfaceMain(args)
    return
  }

  private def computePipeline(ctx: Context): Pipeline[Program, Program] = {

    var pipeline: Pipeline[Program, Program] =
      if (ctx.lang == ProgramLanguage.ScalaProgram) frontend.ExtractionPhase else frontend.CExtractionPhase

    if (ctx.hasFlag("ds-pre-c") || ctx.hasFlag("ds-pre-scala")) {
      pipeline >>= analysis.SpecsProcessingPhase
      pipeline >>= backend.InfoPhase
      return pipeline
    }
    if (ctx.hasFlag("print-ast")) {
      if (ctx.hasFlag("unroll")) {
        pipeline >>= analysis.SpecsProcessingPhase
        pipeline >>= transform.UnrollPhase
        //pipeline >>= analysis.DataflowPhase
        //pipeline >>= backend.CodeGenerationPhase
      }
      pipeline >>= backend.InfoPhase
      return pipeline
    }

    pipeline >>= analysis.SpecsProcessingPhase
    if (ctx.hasFlag("unroll")) {
      pipeline >>= transform.UnrollPhase
      if (ctx.hasFlag("codegen")) {
        pipeline >>= backend.CodeGenerationPhase
        return pipeline
      }
    }

    if (ctx.hasFlag("rewrite")) {
      pipeline >>= opt.RewritingOptimizationPhase
    } else if (ctx.option[List[Any]]("comp-opts").nonEmpty) {
      pipeline >>= transform.CompilerOptimizationPhase
    }

    if ((ctx.hasFlag("pow-roll") || ctx.hasFlag("pow-unroll")) && !ctx.fixedPoint) {
      pipeline >>= transform.PowTransformerPhase
    }

    if (ctx.hasFlag("dynamic")) {
      pipeline >>= analysis.DynamicPhase
      pipeline >>= backend.InfoPhase

    } else if (ctx.hasFlag("mixed-cost-eval")) {
      pipeline >>= transform.TACTransformerPhase >>
        transform.ConstantTransformerPhase >>
        analysis.RangePhase >>
        experiment.CostFunctionEvaluationExperiment

    } else if (ctx.hasFlag("mixed-exp-gen")) {
      pipeline >>= experiment.MixedPrecisionExperimentGenerationPhase

    } else if (ctx.hasFlag("approx")) {
      pipeline >>= transform.TACTransformerPhase >>
        transform.ConstantTransformerPhase

      if (ctx.hasFlag("mixed-tuning")) {
        pipeline >>= analysis.RangePhase >>
          opt.MixedPrecisionOptimizationPhase
      } else
        pipeline >>= analysis.DataflowPhase

      pipeline >>= opt.ApproxPhase >>
        analysis.AbsErrorPhase >>
        backend.InfoPhase >>
        backend.CodeGenerationPhase

    } else if (ctx.hasFlag("metalibm") && ctx.hasFlag("mixed-tuning")){
      // for now will only consider depth = 0 and equal error distribution
      pipeline >>= transform.TACTransformerPhase >>
        transform.ConstantTransformerPhase >>
        analysis.DataflowPhase >>
        opt.MixedPrecisionOptimizationPhase >>
        analysis.AbsErrorPhase >>
        opt.MetalibmPhase >>
        analysis.DataflowPhase >>     // TODO: AbsErrorPhase is enough?
        backend.InfoPhase >>
        backend.CodeGenerationPhase

    } else if (ctx.hasFlag("mixed-tuning")) {

      val rangePhase = if (ctx.hasFlag("subdiv")) {
        analysis.DataflowSubdivisionPhase
      } else {
        analysis.DataflowPhase
      }

      pipeline >>= transform.TACTransformerPhase >>
        transform.ConstantTransformerPhase >>
        rangePhase >>
        opt.MixedPrecisionOptimizationPhase

      ctx.option[Precision]("precision") match {
        case FixedPrecision(_) =>
          pipeline >>=
            analysis.AbsErrorPhase >> // needed to get intermediate ranges for fixed-points
            backend.InfoPhase >>
            backend.CodeGenerationPhase

        case _ =>
          pipeline >>=
            backend.InfoPhase >>
            backend.CodeGenerationPhase
      }

    } else if (ctx.hasFlag("metalibm")){
      pipeline >>= transform.DecompositionPhase >>
        analysis.DataflowPhase >>
        opt.MetalibmPhase >>
        analysis.DataflowPhase >>  // TODO: AbsErrorPhase is enough?
        backend.InfoPhase >>
        backend.CodeGenerationPhase

    } else if (ctx.hasFlag("FPTaylorOriginal")) {
       pipeline  >>= analysis.FunctionCallPhase
       //pipeline >>= analysis.TaylorAbstractPhase
       pipeline >>= analysis.FPTaylorPhaseOriginal
       pipeline >>= backend.InfoPhase

    } else if (ctx.hasFlag("FPTaylor")) {
       pipeline  >>= analysis.FunctionCallPhase
       //pipeline >>= analysis.TaylorAbstractPhase
       pipeline >>= analysis.FPTaylorPhase
       pipeline >>= backend.InfoPhase

    } else if (ctx.hasFlag("modularRoundOffEval")) {
//       pipeline  >>= analysis.FunctionCallPhase
       pipeline >>= analysis.TaylorAbstractPhase
       pipeline >>= analysis.ModularRoundOffErrorEvalPhase
       pipeline >>= backend.InfoPhase

    } else if (ctx.hasFlag("inlineTranslate")) {
      pipeline  >>= analysis.FunctionCallPhase
      pipeline >>= backend.CodeGenerationPhase
    } else if (ctx.hasFlag("ds") && !ctx.hasFlag("unroll")) {
      pipeline >>= analysis.DSAbstractionPhase
      pipeline >>= backend.InfoPhase
      //pipeline >>= transform.TACTransformerPhase
      //pipeline >>= backend.CodeGenerationPhase
      if (ctx.hasFlag("codegen")) {
        pipeline >>= backend.CodeGenerationPhase
      }
    } else if (ctx.hasFlag("ds-naive")) {
      pipeline >>= analysis.DSNaivePhase
      pipeline >>= backend.InfoPhase
      if (ctx.hasFlag("codegen")) {
        pipeline >>= backend.CodeGenerationPhase
      }
    } else {
      // Standard static analyses
      if (ctx.fixedPoint && ctx.hasFlag("apfixed")) {
        pipeline >>= transform.ConstantTransformerPhase
      }

      if (ctx.hasFlag("three-address") || (ctx.fixedPoint && ctx.hasFlag("codegen"))) {
        pipeline >>= transform.TACTransformerPhase
      }

      // must come just before the (standard) analysis so that it inlines
      // functions with transformations applied
      pipeline >>= analysis.FunctionCallPhase

      // TODO: this is very ugly
      if (ctx.hasFlag("subdiv") && ctx.option[DaisyPhase]("analysis") == analysis.DataflowPhase) {
        pipeline >>= analysis.DataflowSubdivisionPhase
      } else {
        pipeline >>= ctx.option[DaisyPhase]("analysis")
      }

      pipeline >>= backend.InfoPhase

      if (ctx.hasFlag("codegen")) {
        pipeline >>= backend.CodeGenerationPhase
      }
    }

    if (ctx.hasFlag("benchmarking")) {
      pipeline >>= experiment.BenchmarkingPhase
    }

    pipeline
  }

  private def showHelp(reporter: Reporter): Unit = {
    reporter.info("usage: [--help] [--debug=<N>] [..] <files>")
    reporter.info("")
    for (opt <- Main.globalOptions.toSeq.sortBy(_.name)) {
      reporter.info(opt.helpLine)
    }
    reporter.info("")
    reporter.info("Additional options, by component:")

    for (c <- Main.allPhases.toSeq.sortBy(_.name) if c.definedOptions.nonEmpty) {
      reporter.info("")
      reporter.info(s"${c.name} Phase")
      for (opt <- c.definedOptions.toSeq.sortBy(_.name)) {
        reporter.info(opt.helpLine)
      }
    }
    None
  }


  def processOptions(args: List[String]): Option[Context] = {
    val initReporter = new DefaultReporter(Set(), false)

    if (args.isEmpty || args.contains("--help")) {
      showHelp(initReporter)
      return None
    }

    val argsMap: Map[String, String] =
      args.filter(_.startsWith("--")).map(_.drop(2).split("=", 2).toList match {
        case List(name, value) => name -> value
        case List(name) => name -> "yes"
      }).toMap


    argsMap.keySet.diff(allOptions.map(_.name)).foreach {
      name => initReporter.warning(s"Unknown option: $name")
    }

    // go through all options and check if they are defined, else use default
    val opts: Map[String, Any] = allOptions.map({
      case FlagOption(name, _) => name -> argsMap.get(name).isDefined

      case StringOption(name, _) => name -> argsMap.get(name)

      case MultiStringOption(name, _, _) =>
        name -> argsMap.get(name).map(_.stripPrefix("[").stripPrefix("]").split(":").toList).getOrElse(Nil)

      case NumOption(name, default, _) => argsMap.get(name) match {
        case None => name -> default
        case Some(s) => try {
          name -> s.toLong
        } catch {
          case e: NumberFormatException =>
            initReporter.warning(s"Can't parse argument for option $name, using default")
            name -> default
        }
      }

      case ChoiceOption(name, choices, default, _) => argsMap.get(name) match {
        case Some(s) if choices.keySet.contains(s) => name -> choices(s)
        case Some(s) =>
          initReporter.warning(s"Unknown choice value for $name: $s. Options: " +
            s"${choices.keySet.toSeq.sorted.mkString(", ")}. Using default $default")
          name -> choices(default)
        case None => name -> choices(default)
      }

      case MultiChoiceOption(name, choices, _) => argsMap.get(name) match {
        case Some("all") | Some("[all]") =>
          name -> choices.values.toList
        case Some(ss) => name -> ss.stripPrefix("[").stripSuffix("]").split(":").toList.filter {
          case "all" =>
            initReporter.warning(s"'all' in list for $name, ignoring"); false
          case s if !choices.keySet.contains(s) =>
            initReporter.warning(s"Unknown choice value for $name: $s. Options: ${choices.keySet.toSeq.sorted.mkString(", ")}"); false
          case _ => true
        }.map(choices(_))
        case None => name -> Nil
      }
    }).toMap

    def inputInfo: (String, ProgramLanguage.Value) = args.filterNot(_.startsWith("-")) match {
      case Seq() => initReporter.fatalError("No input file")
      case Seq(f) if new File(f).exists && f.endsWith(".c") => (f, ProgramLanguage.CProgram)
      case Seq(f) if new File(f).exists => (f, ProgramLanguage.ScalaProgram)
      case Seq(f) => initReporter.fatalError(s"File $f does not exist")
      case fs => initReporter.fatalError("More than one input file: " + fs.mkString(", "))
    }

    val (inputFile, programLanguage) = inputInfo
    Option(Context(
      initReport = initReporter.report,
      file = inputFile,
      lang = programLanguage,
      options = opts
    ))
  }

  object ProgramLanguage extends Enumeration {
    val CProgram, ScalaProgram = Value
  }

}
