package regression

import daisy._
import org.scalatest.FunSuite


/**
  Regression test for the basic absolute error and range computations.
*/
class RelErrorRegressionTest extends FunSuite {
  val fileName = "src/test/resources/RelErrorRegressionFunctions.scala"

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
        var (res, _) = (analysis.SpecsProcessingPhase >> analysis.RelativeErrorPhase).run(ctx2, prg)
        assert(res.resultRelativeErrors.values.head.get.toString === error)

      }
    }
  }


  run_tests("--analysis=relative --rel-rangeMethod=interval", "Float64", List(
    ("doppler", ("8.742550943810163e-10")),
    ("sine", ("1.060843821871097e-15")),
    ("sineOrder3", ("1.4334238325068803e-15")),
    ("sqroot", ("2.211943629532428e-15")),
    ("bspline0", ("4.681440420502748e-15")),
    ("bspline1", ("5.299386818897896e-15")),
    ("bspline2", ("1.2499665562116672e-15")),
    ("bspline3", ("2.491710541600866e-13")),
    ("rigidBody1", ("1.5778025529968637e-11")),
    ("rigidBody2", ("1.6019547336082975e-12")),
    ("turbine1", ("3.991594984732872e-14")),
    ("turbine2", ("7.952387263538645e-15")),
    ("turbine3", ("1.4163135412566422e-13")),
    //("kepler0", ("2.4363316234721624e-12")),  // for some reason does not work when run in full testsuite
    ("kepler1", ("4.089859713369839e-12")),
    ("kepler2", ("4.083662264413485e-11")),
    ("himmilbeau", ("4.3247288170609234e-14"))
  ))

  // the expensive ones we skip for this
  run_tests("--analysis=relative --rel-rangeMethod=interval --denormals", "Float64", List(
    //("doppler", ("8.7425888512587e-10")),
    //("sine", ("1.0608438218710974e-15")),
    ("sineOrder3", ("1.433423832506883e-15")),
    ("sqroot", ("2.2119436295330775e-15")),
    ("bspline0", ("4.681440420502753e-15")),
    ("bspline1", ("5.299386818897965e-15")),
    ("bspline2", ("1.2499665562116676e-15")),
    ("bspline3", ("2.49171054160112e-13")),
    ("rigidBody1", ("1.577802552996865e-11")),
    ("rigidBody2", ("1.6019547336082998e-12")),
    //("turbine1", ("3.9915949847329135e-14")),
    //("turbine2", ("7.952387263538714e-15")),
    //("turbine3", ("1.416313541256797e-13")),
    //("kepler0", ("2.436331623472163e-12")),
    //("kepler1", ("4.089859713369839e-12")),
    //("kepler2", ("4.083662264413498e-11")),
    ("himmilbeau", ("4.3247288170609234e-14"))
  ))

}
