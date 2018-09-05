package regression

import daisy._
import org.scalatest.FunSuite

class PowTransformerRegressionTest extends FunSuite {
  val fileName = "src/test/resources/RegressionFunctions.scala"

  val _ctx: Context = Main.processOptions(List(fileName, "--silent")).get
  Main.ctx = _ctx
  val (ctx, prg) = frontend.ExtractionPhase.run(_ctx, null)

  def run_tests(errorMethod: String, precision: String, results: List[(String, (String, String))]): Unit = {
    if (results.isEmpty) {
      ignore(s"$errorMethod $precision") {}
    }
    for ((fnc, (error, range)) <- results) {
      test(s"$errorMethod $precision: $fnc") {
        val ctx2 = Main.processOptions(errorMethod.split(' ').toList ++ List(fileName, "--silent",
          "--precision=" + precision, "--functions=" + fnc)).get
        var (res, _) = (analysis.SpecsProcessingPhase >>
          transform.PowTransformerPhase >> analysis.DataflowPhase).run(ctx2, prg)
        assert(res.resultAbsoluteErrors.values.head.toString === error)
        assert(res.resultRealRanges.values.head.toString === range)

      }
    }
  }

  run_tests("--analysis=dataflow --rangeMethod=interval --errorMethod=affine --pow-roll", "Float64", List(
    ("doppler", ("2.724751141821674e-10", "[-158.7191444098274, -0.02944244059231351]")),
    ("sine", ("1.3483922323051159e-15", "[-2.3011348046703466, 2.3011348046703466]")),
    ("sineOrder3", ("2.253136404056927e-15", "[-2.9419084189651277, 2.9419084189651277]")),
    ("bspline0", ("1.9891495857867394e-16", "[0.0, 0.16666666666666666]")),
    ("predatorPrey", ("2.067625063567953e-16", "[0.03727705922396188, 0.35710168263424846]")),
    ("turbine1", ("9.405907357327134e-14", "[-58.32912689020381, -1.5505285721480735]")),
    ("turbine2", ("1.4021733867237813e-13", "[-29.43698909090909, 80.993]")),
    ("turbine3", ("6.776520905168677e-14", "[0.4660958448753463, 40.375126890203816]")),
    ("himmilbeau", ("1.4032863759894132e-10", "[0.0, 890.0]")),
    ("evenPowers", ("3.244460832606249e-06", "[0.0, 1.8422830894185123E8]")),
    ("inverseSquare", ("6462.356889741219", "[1.0E-10, 1.0E10]")),
    ("euclideanDistance", ("4.959167599283754e-13", "[9.963, 27.88814736739607]"))
  ))

  run_tests("--analysis=dataflow --rangeMethod=interval --errorMethod=affine --pow-unroll", "Float64", List(
    ("evenPowers", ("1.5563309611474134e-07", "[-1.1271230032394606E8, 1.8422830894185123E8]")),
    ("inverseSquare", ("6462.356889741219", "[1.0E-10, 1.0E10]"))
  ))
}
