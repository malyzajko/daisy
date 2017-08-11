// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.collection.immutable.Seq
import lang.Trees.Program

object Main {

  val optionFunctions = ListOptionDef("functions",  "Which functions to consider (currently only for error analysis).",
    List("f1", "f2"))

  val optionPrintToughSMTCalls = FlagOptionDef("print-tough-smt-calls",
    "If enabled, will print those SMT queries to file which take longer.")

  val optionSolver = ChoiceOptionDef("solver", "smt solver to use for smt range analysis",
    Set("dReal", "z3"), "z3")

  val optionAnalysis = ChoiceOptionDef("analysis",  "Which analysis method to use.",
    Set("dataflow", "opt", "dataflowSubdiv", "relative"), "dataflow")

  val optionPrecision = ChoiceOptionDef("precision", "(Default, uniform) precision to use",
    Set("Float32", "Float64", "DoubleDouble", "QuadDouble", "Fixed16", "Fixed32"), "Float64")

  val optionRangeMethod = ChoiceOptionDef("rangeMethod", "Method for range analysis.",
    Set("affine", "interval", "smt"), "interval")

  val optionNoRoundoff = FlagOptionDef("noRoundoff", "Do not track roundoff errors.")

  val optionNoInitialErrors = FlagOptionDef("noInitialErrors", "Do not track initial errors specified by user.")

  val optionMixedPrecFile = ParamOptionDef("mixed-precision", """File with type assignment for all variables.
    The format is the following:
    function_name = {
      variable_name_1: prec_1
      variable_name_2: prec_2
      ... }
    function_name_2 = {
      variable_name_i: prec_i }

    The precision is Float, Double or DoubleDouble.
    The file can give only a partial precision map.""", "")

  val optionDenormals = FlagOptionDef("denormals", "Include parameter for denormals in the FP abstraction")

  val globalOptions: Set[CmdLineOptionDef[Any]] = Set(
    FlagOptionDef("help",             "Show this message."),
    ListOptionDef("debug",            "For which sections to print debug info.",
      List("analysis","solver")),
    FlagOptionDef("dynamic",          "Run dynamic analysis."),
    FlagOptionDef("codegen",    "Generate code (as opposed to just doing analysis)."),
    FlagOptionDef("rewrite",      "Rewrite expression to improve accuracy ."),
    optionAnalysis,
    optionFunctions,
    optionPrintToughSMTCalls,
    optionSolver,
    optionMixedPrecFile,
    optionPrecision,
    optionNoRoundoff,
    optionNoInitialErrors,
    optionRangeMethod,
    optionDenormals
  )

  /*
    For now these are phases, but it should be anything that
    needs command-line options.
   */
  lazy val allComponents: Set[DaisyPhase] = Set(
    analysis.SpecsProcessingPhase,
    analysis.AbsErrorPhase,
    analysis.RangePhase,
    analysis.RangeErrorPhase,
    analysis.RelativeErrorPhase,
    analysis.TaylorErrorPhase,
    analysis.DataflowSubdivisionPhase,
    backend.CodeGenerationPhase,
    transform.SSATransformerPhase,
    analysis.DynamicPhase,
    opt.RewritingOptimizationPhase,
    backend.InfoPhase)

  def main(args: Array[String]): Unit = {

    val ctx = processOptions(args.toList)

    if (ctx.files.length > 1) {
      ctx.reporter.fatalError("More than one input file.")
    } else {
      val timerTotal = ctx.timers.total.start

      // this is the old frontend going through Leon
      // val inputPrg = frontend.ScalaExtraction.run(ctx)
      // new frontend going directly through Scala compiler
      val inputPrg = frontend.ExtractionPhase(ctx)

      val pipeline = computePipeline(ctx)

      try { // for debugging it's better to have these off.
        ctx.reporter.info("\n************ Starting Daisy ************")
        pipeline.run(ctx, inputPrg)
      } catch {
        case tools.DenormalRangeException(msg) =>
          ctx.reporter.warning(msg)
        case tools.OverflowException(msg) =>
          ctx.reporter.warning(msg)
        case e: DaisyFatalError => ctx.reporter.info("Something really bad happened. Cannot continue.")
      }

      timerTotal.stop
      ctx.reporter.info("time: \n" + ctx.timers.toString)
    }
  }

  private def processOptions(args: List[String]): Context = {

    val initReporter = new DefaultReporter(Set())

    // all available options from all phases
    val allOptions: Map[String, CmdLineOptionDef[Any]] =
      (globalOptions ++ allComponents.flatMap(_.definedOptions)).map(o => o.name -> o).toMap

    // check for duplicated command-line options
    val allOptionNames = globalOptions.map(_.name).toSeq ++
      allComponents.toSeq.flatMap(_.definedOptions).map(_.name)
    val diff = allOptionNames.diff(allOptions.keys.toSeq)
    if (!diff.isEmpty) {
      initReporter.warning("Duplicated command-line options: " + diff.sorted)
    }

    val options = args.filter(_.startsWith("--"))
    val inputFiles = args.filterNot(_.startsWith("-"))
    if (inputFiles.length == 0) {
      showHelp(initReporter)
    }

    var validOptions: List[CmdLineOption[Any]] = List()
    var debugSections = Set[DebugSection]()
    var fixedPoints: Boolean = false

    for (opt <- options) {
      opt.drop(2).split("=", 2).toList match {
        case List(name, value) =>
          allOptions.get(name) match {
            case Some(ints: ParamOptionDef) =>
              validOptions +:= ParamOption(name, value)

            case Some(lists: ListOptionDef) =>
              validOptions +:= ListOption(name, value.split(":").toList)

            case Some(ChoiceOptionDef(n, _, choices, _)) =>
              if (choices.contains(value)) {
                validOptions +:= ChoiceOption(name, value)
              } else {
                initReporter.warning(s"Unknown choice value for $name: $value")
              }

            case _ =>
              initReporter.warning(s"Unknown option: $name, $value")
          }

        case List(name) =>
          allOptions.get(name) match {
            case Some(flg: FlagOptionDef) =>
              validOptions +:= FlagOption(name)
            case _ =>
              initReporter.warning(s"Unknown option: $name")
          }
      }
    }

    // Process options we understand:
    for(opt <- validOptions) opt match {
      case ListOption("debug", sections) =>
        sections.foreach {{
          case "all" => debugSections = DebugSections.all
          case x =>
            DebugSections.all.find(_.name == x) match {
              case Some(rs) =>
                debugSections += rs
              case None =>
                initReporter.error("Section " + x + " not found, available: " +
                  DebugSections.all.map(_.name).mkString(", "))
            }
        }}

      case FlagOption("help") =>
        showHelp(initReporter)

      case ChoiceOption("precision", "Fixed16") | ChoiceOption("precision", "Fixed32") =>
        fixedPoints = true

      case _ =>
    }

    Context(
      reporter = new DefaultReporter(debugSections),
      files = inputFiles,
      options = validOptions,
      fixedPoint = fixedPoints
    )
  }

  private def showHelp(reporter: Reporter): Unit = {
    reporter.info("usage: [--help] [--debug=<N>] [..] <files>")
    reporter.info("")
    for (opt <- globalOptions.toSeq.sortBy(_.name)) {
      reporter.info(opt.helpLine)
    }
    reporter.info("")
    reporter.info("Additional options, by component:")

    for (c <- allComponents.toSeq.sortBy(_.name) if !c.definedOptions.isEmpty) {
      reporter.info("")
      reporter.info(s"${c.name}")
      for(opt <- c.definedOptions.toSeq.sortBy(_.name)) {
        reporter.info(opt.helpLine)
      }
    }
    sys.exit(0)
  }


  private def computePipeline(ctx: Context): Pipeline[Program, Program] = {
    val ssaNeeded = ctx.fixedPoint && ctx.hasFlag("codegen")

    val beginning = if (ctx.hasFlag("rewrite")) {
      analysis.SpecsProcessingPhase andThen opt.RewritingOptimizationPhase
    } else {
      analysis.SpecsProcessingPhase
    }

    if (ctx.hasFlag("dynamic")) {

      beginning andThen analysis.DynamicPhase

    } else {

      val analysisPipeline = ((ssaNeeded, ctx.findOption(optionAnalysis)): @unchecked) match {
        case (true, Some("dataflow")) =>
          transform.SSATransformerPhase andThen analysis.RangeErrorPhase

        case (false, Some("dataflow")) =>
          analysis.RangeErrorPhase

        case (_, Some("opt")) =>
          analysis.TaylorErrorPhase

        case (_, Some("dataflowSubdiv")) =>
          analysis.DataflowSubdivisionPhase

        case (_, Some("relative")) =>
          analysis.RelativeErrorPhase

        case (true, None) =>
          ctx.reporter.info("No analysis method specified, choosing default.")
          transform.SSATransformerPhase andThen analysis.RangeErrorPhase

        case (false, None) =>
          ctx.reporter.info("No analysis method specified, choosing default.")
          analysis.RangeErrorPhase
      }

      if (ctx.hasFlag("codegen")) {
        beginning andThen analysisPipeline andThen backend.InfoPhase andThen
        backend.CodeGenerationPhase
      } else {
        beginning andThen analysisPipeline andThen backend.InfoPhase
      }

    }

  }
}
