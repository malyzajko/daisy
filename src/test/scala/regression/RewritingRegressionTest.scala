package regression

import daisy._
import org.scalatest.FunSuite


/**
  Regression test for the basic absolute error and range computations.
*/
class RewritingRegressionTest extends FunSuite {
  val fileName = "src/test/resources/RegressionFunctions.scala"

  val _ctx : Context = Main.processOptions(List(fileName, "--silent")).get
  Main.ctx = _ctx
  val (ctx, prg) = frontend.ExtractionPhase.run(_ctx, null)


  def run_tests(errorMethod: String, precision: String, results: List[(String, String)]): Unit = {
    if (results.isEmpty) {
      ignore(s"$errorMethod $precision"){}
    }
    for ((fnc,error) <- results) {
      test(s"$errorMethod $precision: $fnc") {
        val ctx2 = Main.processOptions(errorMethod.split(' ').toList ++ List(fileName, "--silent",
          "--precision="+precision, "--functions="+fnc)).get
        val (res, _) = (analysis.SpecsProcessingPhase >> opt.RewritingOptimizationPhase >> analysis.DataflowPhase).run(ctx2, prg)
        assert(res.resultAbsoluteErrors.values.head.toString === error)

      }
    }
  }


  run_tests("--rewrite --rewrite-fitness-fnc=interval-affine --rewrite-seed=4781 " +
    "--analysis=dataflow --rangeMethod=interval", "Float64", List(
    ("doppler", ("1.980501095711251e-13")),
    ("sine", ("7.435281363324554e-16")),
    ("sineOrder3", ("1.4439568010937595e-15")),
    ("sqroot", ("2.8799185258776566e-13")),
    ("bspline0", ("1.5265566588595905e-16")),
    ("bspline1", ("6.106226635438361e-16")),
    ("bspline2", ("5.921189464667502e-16")),
    ("bspline3", ("9.714451465470121e-17")),
    ("rigidBody1", ("1.971756091734278e-13")),
    ("rigidBody2", ("2.9067415141526002e-11")),
    ("turbine1", ("8.021557111621937e-14")),
    ("turbine2", ("1.1901079531341762e-13")),
    ("turbine3", ("5.740117117434138e-14")),
    ("kepler0", ("6.190603585309873e-14")),
    ("kepler1", ("3.181060748147502e-13")),
    ("kepler2", ("1.6850943751478554e-12")),
    ("himmilbeau", ("2.246203223421617e-12"))
  ))


  run_tests("--rewrite --rewrite-fitness-fnc=affine-affine --rewrite-seed=4781 " +
    "--analysis=dataflow --rangeMethod=affine", "Float64", List(
    ("doppler", ("1.775206439078186e-13")),
    ("sine", ("5.640619536410695e-16")),
    ("sineOrder3", ("1.107331728625595e-15")),
    ("sqroot", ("2.85049761572509e-13")),
    ("bspline0", ("1.5265566588595905e-16")),
    ("bspline1", ("4.94974431812049e-16")),
    ("bspline2", ("3.839521293495333e-16")),
    ("bspline3", ("9.714451465470121e-17")),
    ("rigidBody1", ("1.971756091734278e-13")),
    ("rigidBody2", ("2.9067415141526002e-11")),
    ("turbine1", ("7.599566968715011e-14")),
    ("turbine2", ("1.1397471619453025e-13")),
    ("turbine3", ("5.371826198956567e-14")),
    ("kepler0", ("5.1372239795455246e-14")),
    ("kepler1", ("3.2136782124325694e-13")),
    ("kepler2", ("1.4017276228628363e-12")),
    ("himmilbeau", ("8.961720254774266e-13"))
  ))


}
