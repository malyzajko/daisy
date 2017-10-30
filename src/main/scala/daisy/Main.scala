// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

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
    MultiChoiceOptionDef(
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
                                    // TODO @robert is this still true?
      "Which functions to consider (currently only for error analysis)"),
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
      "Include parameter for denormals in the FP abstraction (for optimization-based approach only).")
  )

  /*
    For now these are phases, but it should be anything that
    needs command-line options.
   */
  lazy val allComponents: Set[Component] = Set(
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
    backend.InfoPhase,
    frontend.ExtractionPhase)

  var cfg: Config = _

  def main(args: Array[String]): Unit = {

    cfg = Config(args: _*)

    val pipeline = computePipeline(cfg)

    cfg.reporter.info("\n************ Starting Daisy ************")

    try { // for debugging it's better to have these off.
      pipeline.run(Context(), Program(null, Nil))
    } catch {
      case tools.DivisionByZeroException(msg) =>
        cfg.reporter.warning(msg)
      case tools.DenormalRangeException(msg) =>
        cfg.reporter.warning(msg)
      case tools.OverflowException(msg) =>
        cfg.reporter.warning(msg)
      case e: java.lang.UnsatisfiedLinkError =>
        cfg.reporter.warning("A library could not be loaded: " + e)
      //case e: DaisyFatalError => cfg.reporter.info("Something really bad happened. Cannot continue.")
    }

    cfg.timers.get("total").stop
    cfg.reporter.info("time: \n" + cfg.timers.toString)
  }

  private def computePipeline(cfg: Config): Pipeline[Program, Program] = {

    var pipeline: Pipeline[Program, Program] = frontend.ExtractionPhase(cfg)

    pipeline >>= analysis.SpecsProcessingPhase(cfg)

    if (cfg.hasFlag("rewrite")) {
      pipeline >>= opt.RewritingOptimizationPhase(cfg)
    }

    if (cfg.hasFlag("dynamic")) {
      pipeline >>= analysis.DynamicPhase(cfg)
    } else {
      if (cfg.hasFlag("three-address") || (cfg.fixedPoint && cfg.hasFlag("codegen"))) {
        pipeline >>= transform.TACTransformerPhase(cfg)
      }

      // TODO: this is very ugly
      if (cfg.hasFlag("subdiv") && cfg.option[PhaseComponent]("analysis") == analysis.DataflowPhase) {
        pipeline >>= analysis.DataflowSubdivisionPhase(cfg)
      } else {
        pipeline >>= cfg.option[PhaseComponent]("analysis").apply(cfg)
      }

      pipeline >>= backend.InfoPhase(cfg)

      if (cfg.hasFlag("codegen")) {
        pipeline >>= backend.CodeGenerationPhase(cfg)
      }
    }
    pipeline
  }
}
