package regression


import org.scalatest.FunSuite

import daisy._

/**
  Regression test for the basic absolute error and range computations.
*/
class MixedTuningRegressionTest extends FunSuite {

  val fileName = "src/test/resources/MixedTuningRegressionFunctions.scala"

  val _ctx: Context = Main.processOptions(List(fileName, "--silent")).get
  Main.ctx = _ctx
  val (ctx, prg) = frontend.ExtractionPhase.run(_ctx, null)

  val pipeline = analysis.SpecsProcessingPhase >>
      transform.TACTransformerPhase >>
      transform.ConstantTransformerPhase >>
      analysis.RangePhase >>
      opt.MixedPrecisionOptimizationPhase

  def run_tests(options: String, results: List[(String, (String, String))]): Unit = {
    if (results.isEmpty) {
      ignore(s"$options"){}
    }
    for ((fnc,(error, range)) <- results) {
      test(s"$options: $fnc") {

        val ctx2 = Main.processOptions(options.split(' ').toList ++ List(fileName, "--silent",
          "--functions="+fnc)).get

        val (res, _) = pipeline.run(ctx2, prg)
        assert(error === res.resultAbsoluteErrors.values.head.toString)
      }
    }
  }


  run_tests("--rangeMethod=interval", List(
    ("doppler_float64_05", ("1.604514563551882e-13", "")),
    ("sine_float64_05", ("3.3660100422339055e-16", "")),
    ("sqroot_float64_05", ("1.6342482922482307e-13", "")),
    ("bspline0_float64_05", ("9.251858538542973e-17", "")),
    ("bspline1_float64_05", ("3.1456319031046104e-16", "")),
    ("bspline2_float64_05", ("2.590520390792032e-16", "")),
    ("rigidBody1_float64_05", ("1.2256862191861728e-13", "")),
    ("rigidBody2_float64_05", ("1.468603016974157e-11", "")),
    ("turbine1_float64_05", ("3.449915557837011e-14", "")),
    ("turbine2_float64_05", ("5.6781315624501624e-14", "")),
    ("turbine3_float64_05", ("2.3527931649024305e-14", "")),
    ("kepler0_float64_05", ("4.204636638860393e-14", "")),
    ("kepler1_float64_05", ("2.2222650386538592e-13", "")),
    ("kepler2_float64_05", ("9.331273531643093e-13", "")),
    ("himmilbeau_float64_05", ("5.080380560684716e-13", ""))
  ))

  run_tests("--rangeMethod=interval", List(
    ("doppler_float32_05", ("8.388367288853761e-05", "")),
    ("sine_float32_05", ("1.807112941741575e-07", "")),
    ("sqroot_float32_05", ("8.77380432557919e-05", "")),
    ("bspline0_float32_05", ("2.980232427044857e-08", "")),
    ("bspline1_float32_05", ("1.688798359304282e-07", "")),
    ("bspline2_float32_05", ("1.3907751265455448e-07", "")),
    ("rigidBody1_float32_05", ("6.58035291678516e-05", "")),
    ("rigidBody2_float32_05", ("0.00788450276878195", "")),
    ("turbine1_float32_05", ("1.8521599395563295e-05", "")),
    ("turbine2_float32_05", ("3.0484246406228637e-05", "")),
    ("turbine3_float32_05", ("1.2631466566316841e-05", "")),
    ("kepler0_float32_05", ("2.2573471700129134e-05", "")),
    ("kepler1_float32_05", ("0.00011930695707116155", "")),
    ("kepler2_float32_05", ("0.0005009689839862508", "")),
    ("himmilbeau_float32_05", ("2.3252511027749283e-12", ""))
  ))

}