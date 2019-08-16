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
      opt.MixedPrecisionOptimizationPhase >>
      analysis.AbsErrorPhase

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
    ("doppler_float64_05", ("2.2355864078143494e-13", "")),
    ("sine_float64_05", ("1.2718624100140849e-33", "")),
    ("sqroot_float64_05", ("1.667554982986985e-13", "")),
    ("bspline0_float64_05", ("9.251858538542972e-17", "")),
    ("bspline1_float64_05", ("3.3306690738754696e-16", "")),
    ("bspline2_float64_05", ("2.220446049250313e-16", "")),
    ("rigidBody1_float64_05", ("6.838973831690964e-14", "")),
    ("rigidBody2_float64_05", ("1.9532819806045154e-11", "")),
    ("turbine1_float64_05", ("4.0091690651094673e-14", "")),
    ("turbine2_float64_05", ("6.983917717644337e-14", "")),
    ("turbine3_float64_05", ("2.8185317327929985e-14", "")),
    ("kepler0_float64_05", ("4.3050008002865075e-14", "")),
    ("kepler1_float64_05", ("2.313882419002766e-13", "")),
    ("kepler2_float64_05", ("7.725020623183809e-13", "")),
    ("himmilbeau_float64_05", ("5.080380560684716e-13", ""))
  ))

  run_tests("--rangeMethod=interval", List(
    ("doppler_float32_05", ("2.389686436884487e-05", "")),
    ("sine_float32_05", ("1.1296729607621835e-15", "")),
    ("sqroot_float32_05", ("7.963181114800672e-05", "")),
    ("bspline0_float32_05", ("1.6190752442450204e-16", "")),
    ("bspline1_float32_05", ("7.956598343146956e-16", "")),
    ("bspline2_float32_05", ("7.401486830834377e-16", "")),
    ("rigidBody1_float32_05", ("3.67164616354998e-05", "")),
    ("rigidBody2_float32_05", ("0.0019097328524768646", "")),
    ("turbine1_float32_05", ("3.656772343479721e-06", "")),
    ("turbine2_float32_05", ("5.214214525618507e-06", "")),
    ("turbine3_float32_05", ("1.7050579010251316e-05", "")),
    ("kepler0_float32_05", ("2.311229783000357e-05", "")),
    ("kepler1_float32_05", ("8.647480357513048e-05", "")),
    ("kepler2_float32_05", ("0.0005296690367263712", "")),
    ("himmilbeau_float32_05", ("2.3252511027749283e-12", ""))
  ))

}