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
      Map("dataflow" -> analysis.DataflowPhase, "opt" -> analysis.TaylorErrorPhase,
          "relative" -> analysis.RelativeErrorPhase),
      "dataflow",
      "Which analysis method to use"),
    FlagOption(
      "subdiv",
      "Apply subdivision to absolute error computation."
    ),
    ChoiceOption(
      "precision",
      Map("Float16" -> Float16, "Float32" -> Float32, "Float64" -> Float64, "Quad" -> DoubleDouble,
            "QuadDouble" -> QuadDouble,
          "Fixed8" -> FixedPrecision(8), "Fixed16" -> FixedPrecision(16), "Fixed32" -> FixedPrecision(32)),
      "Float64",
      "(Default, uniform) precision to use"),
    StringChoiceOption(
      "rangeMethod",
      Set("affine", "interval", "smt"),
      "interval",
      "Method for range analysis"),
    FlagOption(
      "noRoundoff",
      "Do not track roundoff errors"),
    FlagOption(
      "noInitialErrors",
      "Do not track initial errors specified by user"),
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

    FlagOption("rewrite-fitness-eval", "Generate expressions and analyze errors for various fitness functions"),
    FlagOption("rewrite-stability-experiment", "Run rewriting stability experiment."),
    FlagOption("mixed-cost-eval", "Mixed-precision cost function evaluation experiment"),
    FlagOption("mixed-exp-gen", "Mixed-precision experiment generation"),

    FlagOption("mixed-tuning", "Perform mixed-precision tuning")
  )

  lazy val allPhases: Set[DaisyPhase] = Set(
    analysis.SpecsProcessingPhase,
    analysis.AbsErrorPhase,
    analysis.RangePhase,
    analysis.DataflowPhase,
    analysis.RelativeErrorPhase,
    analysis.TaylorErrorPhase,
    analysis.DataflowSubdivisionPhase,
    backend.CodeGenerationPhase,
    transform.TACTransformerPhase,
    analysis.DynamicPhase,
    opt.RewritingOptimizationPhase,
    transform.ConstantTransformerPhase,
    opt.MixedPrecisionOptimizationPhase,
    experiment.RewritingFitnessEvaluation,
    experiment.MixedPrecisionExperimentGenerationPhase,
    experiment.CostFunctionEvaluationExperiment,
    backend.InfoPhase,
    frontend.ExtractionPhase
  )

  // all available options from all phases
  private val allOptions: Set[CmdLineOption[Any]] =
    globalOptions ++ allPhases.flatMap(_.definedOptions)

  var ctx: Context = null

  def interfaceMain(args: Array[String]): Option[Context] = {
    // TODO: only needs to be run once at compile time, maybe make this into a test
    verifyCmdLineOptions()
    processOptions(args.toList) match {
      case Some(new_ctx) =>
        ctx = new_ctx
        ctx.timers.total.start
        val pipeline = computePipeline(ctx)
        ctx.reporter.info("\n************ Starting Daisy ************")
        try { // for debugging it's better to have these off.
          pipeline.run(ctx, Program(null, Nil))
        } catch {
          case tools.DivisionByZeroException(msg) =>
            ctx.reporter.warning(msg)
          case tools.DenormalRangeException(msg) =>
            ctx.reporter.warning(msg)
          case tools.OverflowException(msg) =>
            ctx.reporter.warning(msg)
          case e: java.lang.UnsatisfiedLinkError =>
            ctx.reporter.warning("A library could not be loaded: " + e)
          case tools.NegativeSqrtException(msg) =>
            ctx.reporter.warning(msg)
          case e: DaisyFatalError =>
            ctx.reporter.info("Something really bad happened. Cannot continue.")
          case _ : Throwable =>
            ctx.reporter.info("Something really bad happened. Cannot continue.")

        }
        ctx.timers.get("total").stop
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

    var pipeline: Pipeline[Program, Program] = frontend.ExtractionPhase

    pipeline >>= analysis.SpecsProcessingPhase

    if (ctx.hasFlag("rewrite")) {
      pipeline >>= opt.RewritingOptimizationPhase
    }

    if (ctx.hasFlag("dynamic")) {
      pipeline >>= analysis.DynamicPhase
      pipeline >>= backend.InfoPhase

    } else if (ctx.hasFlag("rewrite-fitness-eval")) {
      pipeline >>= experiment.RewritingFitnessEvaluation

    // } else if (ctx.hasFlag("rewrite-stability-experiment")) {
    //   pipeline >>= experiment.RewritingStabilityExperiment

    } else if (ctx.hasFlag("mixed-cost-eval")) {
      pipeline >>= transform.TACTransformerPhase >>
        transform.ConstantTransformerPhase >>
        analysis.RangePhase >>
        experiment.CostFunctionEvaluationExperiment

    } else if (ctx.hasFlag("mixed-exp-gen")) {
      pipeline >>= experiment.MixedPrecisionExperimentGenerationPhase

    } else if (ctx.hasFlag("mixed-tuning")) {
      pipeline >>= transform.TACTransformerPhase >>
        transform.ConstantTransformerPhase >>
        analysis.RangePhase >>
        opt.MixedPrecisionOptimizationPhase >>
        backend.InfoPhase >>
        backend.CodeGenerationPhase

    } else {
      // Standard static analyses
      if (ctx.fixedPoint && ctx.hasFlag("apfixed")) {
        pipeline >>= transform.ConstantTransformerPhase
      }

      if (ctx.hasFlag("three-address") || (ctx.fixedPoint && ctx.hasFlag("codegen"))) {
        pipeline >>= transform.TACTransformerPhase
      }

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
      for(opt <- c.definedOptions.toSeq.sortBy(_.name)) {
        reporter.info(opt.helpLine)
      }
    }
    None
  }

  private def verifyCmdLineOptions(): Unit = {
    val allOpts =
      Main.globalOptions.toList.map((_, "global")) ++
      Main.allPhases.flatMap(c => c.definedOptions.toList.map((_, c.name)))
    allOpts.groupBy(_._1.name).collect{
      case (name, opts) if opts.size > 1 =>
        Console.err.println(s"Duplicate command line option '$name' in " +
         opts.map("'"+_._2+"'").mkString(", "))
    }
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

    val inputFile: String = args.filterNot(_.startsWith("-")) match {
      case Seq() => initReporter.fatalError("No input file")
      case Seq(f) if new File(f).exists => f
      case Seq(f) => initReporter.fatalError(s"File $f does not exist")
      case fs => initReporter.fatalError("More than one input file: " + fs.mkString(", "))
    }

    Option(Context(
      initReport = initReporter.report,
      file = inputFile,
      options = opts
    ))
  }
}
