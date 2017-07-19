
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy

import scala.collection.immutable.Seq
import lang.Trees.Program

object Main {

  val optionFunctions = ListOptionDef("functions",  "Which functions to consider (currently only for error analysis).",
      List("f1", "f2"))

  val optionPrintToughSMTCalls = FlagOptionDef("print-tough-smt-calls",
    "If enabled, will print those SMT queries to file which take longer.")

  val optionSolver = ChoiceOptionDef("solver", "smt solver to use for smt range analysis",
    Set("dreal", "dReal", "z3"), "z3")

  val globalOptions: Set[CmdLineOptionDef[Any]] = Set(
    FlagOptionDef("help",       "Show this message."),
    FlagOptionDef("dynamic",    "Run dynamic analysis."),
    FlagOptionDef("relative", "Run experimental relative phase"),
    FlagOptionDef("taylor", "Run experimental taylor simplification phase"),
    FlagOptionDef("relabs", "Run experimental relative through absolute error phase"),
    //ParamOptionDef("timeout",   "Timeout in ms.", "1000"),
    ListOptionDef("debug",      "For which sections to print debug info.",
      List("analysis","solver")),
    FlagOptionDef("codegen",    "Generate code (as opposed to just doing analysis)."),
    optionFunctions,
    optionPrintToughSMTCalls,
    optionSolver
  )

  /*
    For now these are phases, but it should be anything that
    needs command-line options.
   */
  lazy val allComponents : Set[DaisyPhase] = Set(
    analysis.SpecsProcessingPhase,
    analysis.RangeErrorPhase,
    analysis.RelativeErrorPhase,
    analysis.TaylorErrorPhase,
    analysis.RelThroughAbsPhase,
    backend.CodeGenerationPhase,
    transform.SSATransformerPhase,
    analysis.DynamicPhase,
    InfoPhase)

  def main(args: Array[String]) {

    val ctx = processOptions(args.toList)

    if (ctx.files.length > 1) {
      ctx.reporter.fatalError("More than one input file.")
    } else {
      val timerTotal = ctx.timers.total.start

      // this is the old frontend going through Leon
      //val inputPrg = frontend.ScalaExtraction.run(ctx)
      // new frontend going directly through Scala compiler
      val inputPrg = frontend.ExtractionPhase(ctx)

      val pipeline = computePipeline(ctx)

      try { // for debugging it's better to have these off.
        pipeline.run(ctx, inputPrg)
      } catch {
        case e: DaisyFatalError => ctx. reporter.info("Something really bad happened. Cannot continue.")
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

    val options = args.filter(_.startsWith("--"))
    val inputFiles = args.filterNot(_.startsWith("-"))
    if (inputFiles.length == 0) {
      showHelp(initReporter)
    }

    var validOptions: List[CmdLineOption[Any]] = List()
    var debugSections = Set[DebugSection]()

    for (opt <- options) {
      opt.drop(2).split("=", 2).toList match {
        case List(name, value) =>
          allOptions.get(name) match {
            case Some(ints: ParamOptionDef) => //if (value.forall(_.isDigit))
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
                initReporter.error("Section "+ x +" not found, available: "+DebugSections.all.map(_.name).mkString(", "))
            }
        }}

      case FlagOption("help") =>
        showHelp(initReporter)
      case _ =>
    }

    Context(
      reporter = new DefaultReporter(debugSections),
      files = inputFiles,
      options = validOptions
    )
  }

  private def showHelp(reporter: Reporter) {
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
    val fixedPointArith = ctx.findOption(analysis.RangeErrorPhase.optionPrecision) match {
      case Some("Fixed16") => true
      case Some("Fixed32") => true
      case _ => false
    }

    // this is not ideal, using 'magic' strings
    if (ctx.hasFlag("dynamic")) {
      analysis.SpecsProcessingPhase andThen
      analysis.DynamicPhase
    } else if (ctx.hasFlag("codegen") && fixedPointArith) {
      analysis.SpecsProcessingPhase andThen
      transform.SSATransformerPhase andThen
      analysis.RangeErrorPhase andThen
      InfoPhase andThen
      backend.CodeGenerationPhase
    } else if (ctx.hasFlag("codegen")) {
      analysis.SpecsProcessingPhase andThen
      analysis.RangeErrorPhase andThen
      InfoPhase andThen
      backend.CodeGenerationPhase
    } else if (ctx.hasFlag("relative")) {
      analysis.SpecsProcessingPhase andThen
      analysis.RelativeErrorPhase
    } else if (ctx.hasFlag("taylor")) {
      analysis.SpecsProcessingPhase andThen
      analysis.TaylorErrorPhase andThen
        InfoPhase
    } else if (ctx.hasFlag("relabs")) {
      analysis.SpecsProcessingPhase andThen
        analysis.RelThroughAbsPhase
    } else {
      analysis.SpecsProcessingPhase andThen
      analysis.RangeErrorPhase andThen
      InfoPhase
    }

  }
}
