package regression

import daisy._
import daisy.tools.FinitePrecision._
import org.scalatest.funsuite.AnyFunSuite

/**
  Regression test for the fixed-point arithmetic code generation.
*/
class FixedpointCodegenRegressionTest extends AnyFunSuite {

  val inputFile: String = "src/test/resources/RegressionFunctions.scala"
  val context : Context = Main.processOptions(List(inputFile)).get
  val (_, inputPrg) = frontend.ExtractionPhase.run(context, null)

  def pipeline = analysis.SpecsProcessingPhase >> transform.TACTransformerPhase >>
      analysis.DataflowPhase

  def generateCode(bits: Int, function: String): String = {
    val _ctx : Context = Main.processOptions(List(inputFile, "--silent", "--rangeMethod=interval", "--errorMethod=affine",
      "--precision=Fixed" + bits, "--functions=" + function)).get
    val (ctx, program) = pipeline.run(_ctx, inputPrg)

    val fnc = program.defs.find(_.id.toString == function).get

    val newBody = backend.CodeGenerationPhase.toFixedPointCode(fnc.body.get, FixedPrecision(32),
      ctx.intermediateRanges(fnc.id), ctx.intermediateAbsErrors(fnc.id))

    val printer = new daisy.utils.ScalaPrinter(new StringBuffer, ctx)
    printer.pp(newBody, None)(0)
    printer.toString
  }


  val tmpVarRegex = "_tmp[0-9]*".r

  test("codegen-doppler-fixed32") {
    val fixedCode = generateCode(32, "doppler")
    val reference =
      "val _tmp: Long = ((1288490189 * T) >> 30)\n" +
      "val t1: Long = (((1389992346 << 4) + _tmp) >> 4)\n" +
      "val _tmp1: Long = -(t1)\n" +
      "val _tmp4: Long = ((_tmp1 * v) >> 30)\n" +
      "val _tmp2: Long = (((t1 << 2) + u) >> 2)\n" +
      "val _tmp3: Long = (((t1 << 2) + u) >> 2)\n" +
      "val _tmp5: Long = ((_tmp2 * _tmp3) >> 31)\n" +
      "((_tmp4 << 28) / _tmp5)"

    assert(tmpVarRegex.replaceAllIn(fixedCode, "") ===
      tmpVarRegex.replaceAllIn(reference, ""))
  }

  test("codegen-sine-fixed32") {
    val fixedCode = generateCode(32, "sine")

    val reference =
      "val _tmp229: Long = ((x * x) >> 31)\n" +
      "val _tmp230: Long = ((_tmp229 * x) >> 30)\n" +
      "val _tmp231: Long = ((_tmp230 << 30) / 1610612736)\n" +
      "val _tmp236: Long = (((x << 1) - _tmp231) >> 2)\n" +
      "val _tmp232: Long = ((x * x) >> 31)\n" +
      "val _tmp233: Long = ((_tmp232 * x) >> 30)\n" +
      "val _tmp234: Long = ((_tmp233 * x) >> 31)\n" +
      "val _tmp235: Long = ((_tmp234 * x) >> 31)\n" +
      "val _tmp237: Long = ((_tmp235 << 28) / 2013265920)\n" +
      "val _tmp244: Long = (((_tmp236 << 2) + _tmp237) >> 2)\n" +
      "val _tmp238: Long = ((x * x) >> 31)\n" +
      "val _tmp239: Long = ((_tmp238 * x) >> 30)\n" +
      "val _tmp240: Long = ((_tmp239 * x) >> 31)\n" +
      "val _tmp241: Long = ((_tmp240 * x) >> 31)\n" +
      "val _tmp242: Long = ((_tmp241 * x) >> 30)\n" +
      "val _tmp243: Long = ((_tmp242 * x) >> 31)\n" +
      "val _tmp245: Long = ((_tmp243 << 23) / 1321205760)\n" +
      "(((_tmp244 << 2) - _tmp245) >> 2)"
    assert(tmpVarRegex.replaceAllIn(fixedCode, "") ===
      tmpVarRegex.replaceAllIn(reference, ""))
  }

  test("codegen-rigidBody2-fixed32") {
    val fixedCode = generateCode(32, "rigidBody2")
    val reference =
      "val _tmp516: Long = ((x1 * x2) >> 31)\n" +
     "val _tmp517: Long = ((_tmp516 * x3) >> 31)\n" +
     "val _tmp519: Long = ((1073741824 * _tmp517) >> 30)\n" +
     "val _tmp518: Long = ((1610612736 * x3) >> 31)\n" +
     "val _tmp520: Long = ((_tmp518 * x3) >> 31)\n" +
     "val _tmp523: Long = (((_tmp519 << 3) + _tmp520) >> 3)\n" +
     "val _tmp521: Long = ((x1 * x2) >> 31)\n" +
     "val _tmp522: Long = ((_tmp521 * x3) >> 31)\n" +
     "val _tmp524: Long = ((x2 * _tmp522) >> 31)\n" +
     "val _tmp526: Long = ((_tmp523 - (_tmp524 << 3)) >> 3)\n" +
     "val _tmp525: Long = ((1610612736 * x3) >> 31)\n" +
     "val _tmp527: Long = ((_tmp525 * x3) >> 31)\n" +
     "val _tmp528: Long = (((_tmp526 << 6) + _tmp527) >> 6)\n" +
     "(((_tmp528 << 12) - x2) >> 12)"
     assert(tmpVarRegex.replaceAllIn(fixedCode, "") ===
      tmpVarRegex.replaceAllIn(reference, ""))
  }

  test("codegen-turbine1-fixed8") {
    val fixedCode = generateCode(8, "turbine1")
    val reference =
      "val _tmp771: Long = ((r * r) >> 31)\n" +
     "val _tmp772: Long = ((1073741824 << 27) / _tmp771)\n" +
     "val _tmp781: Long = (((1610612736 << 2) + _tmp772) >> 2)\n" +
     "val _tmp773: Long = ((1073741824 * v) >> 30)\n" +
     "val _tmp774: Long = ((1610612736 - (_tmp773 << 2)) >> 2)\n" +
     "val _tmp777: Long = ((268435456 * _tmp774) >> 28)\n" +
     "val _tmp775: Long = ((w * w) >> 31)\n" +
     "val _tmp776: Long = ((_tmp775 * r) >> 31)\n" +
     "val _tmp778: Long = ((_tmp776 * r) >> 31)\n" +
     "val _tmp779: Long = ((_tmp777 * _tmp778) >> 31)\n" +
     "val _tmp780: Long = ((1073741824 - (v << 2)) >> 2)\n" +
     "val _tmp782: Long = ((_tmp779 << 28) / _tmp780)\n" +
     "val _tmp783: Long = ((_tmp781 - (_tmp782 << 5)) >> 5)\n" +
     "(((_tmp783 << 4) - 1207959552) >> 4)"
     assert(tmpVarRegex.replaceAllIn(fixedCode, "") ===
      tmpVarRegex.replaceAllIn(reference, ""))
  }

  test("codegen-kepler2-fixed32") {
    val fixedCode = generateCode(16, "kepler2")
    val reference =
      "val _tmp1071: Long = ((x1 * x4) >> 31)\n" +
     "val _tmp1066: Long = -(x1)\n" +
     "val _tmp1067: Long = ((_tmp1066 + x2) << 1)\n" +
     "val _tmp1068: Long = ((_tmp1067 + (x3 << 1)) >> 2)\n" +
     "val _tmp1069: Long = ((_tmp1068 << 1) - x4)\n" +
     "val _tmp1070: Long = ((_tmp1069 + x5) >> 1)\n" +
     "val _tmp1072: Long = (((_tmp1070 << 1) + x6) >> 2)\n" +
     "val _tmp1079: Long = ((_tmp1071 * _tmp1072) >> 30)\n" +
     "val _tmp1077: Long = ((x2 * x5) >> 31)\n" +
     "val _tmp1073: Long = ((x1 - x2) << 1)\n" +
     "val _tmp1074: Long = ((_tmp1073 + (x3 << 1)) >> 2)\n" +
     "val _tmp1075: Long = (((_tmp1074 << 1) + x4) >> 1)\n" +
     "val _tmp1076: Long = (((_tmp1075 << 1) - x5) >> 1)\n" +
     "val _tmp1078: Long = (((_tmp1076 << 1) + x6) >> 2)\n" +
     "val _tmp1080: Long = ((_tmp1077 * _tmp1078) >> 30)\n" +
     "val _tmp1087: Long = ((_tmp1079 + _tmp1080) >> 1)\n" +
     "val _tmp1085: Long = ((x3 * x6) >> 31)\n" +
     "val _tmp1081: Long = ((x1 + x2) >> 1)\n" +
     "val _tmp1082: Long = (((_tmp1081 << 1) - x3) >> 1)\n" +
     "val _tmp1083: Long = (((_tmp1082 << 1) + x4) >> 1)\n" +
     "val _tmp1084: Long = (((_tmp1083 << 1) + x5) >> 2)\n" +
     "val _tmp1086: Long = (((_tmp1084 << 2) - x6) >> 2)\n" +
     "val _tmp1088: Long = ((_tmp1085 * _tmp1086) >> 30)\n" +
     "val _tmp1090: Long = (((_tmp1087 << 1) + _tmp1088) >> 2)\n" +
     "val _tmp1089: Long = ((x2 * x3) >> 31)\n" +
     "val _tmp1091: Long = ((_tmp1089 * x4) >> 31)\n" +
     "val _tmp1093: Long = (((_tmp1090 << 3) - _tmp1091) >> 3)\n" +
     "val _tmp1092: Long = ((x1 * x3) >> 31)\n" +
     "val _tmp1094: Long = ((_tmp1092 * x5) >> 31)\n" +
     "val _tmp1096: Long = (((_tmp1093 << 3) - _tmp1094) >> 2)\n" +
     "val _tmp1095: Long = ((x1 * x2) >> 31)\n" +
     "val _tmp1097: Long = ((_tmp1095 * x6) >> 31)\n" +
     "val _tmp1099: Long = (((_tmp1096 << 2) - _tmp1097) >> 2)\n" +
     "val _tmp1098: Long = ((x4 * x5) >> 31)\n" +
     "val _tmp1100: Long = ((_tmp1098 * x6) >> 31)\n" +
     "(((_tmp1099 << 2) - _tmp1100) >> 2)"
     assert(tmpVarRegex.replaceAllIn(fixedCode, "") ===
      tmpVarRegex.replaceAllIn(reference, ""))
  }

  test("codegen-himmilbeau-fixed32") {
    val fixedCode = generateCode(32, "himmilbeau")
    val reference =
      "val _tmp1324: Long = ((x1 * x1) >> 30)\n" +
   "val _tmp1325: Long = (((_tmp1324 << 2) + x2) >> 2)\n" +
   "val _tmp1328: Long = (((_tmp1325 << 1) - 1476395008) >> 2)\n" +
   "val _tmp1326: Long = ((x1 * x1) >> 30)\n" +
   "val _tmp1327: Long = (((_tmp1326 << 2) + x2) >> 2)\n" +
   "val _tmp1329: Long = (((_tmp1327 << 1) - 1476395008) >> 2)\n" +
   "val _tmp1336: Long = ((_tmp1328 * _tmp1329) >> 30)\n" +
   "val _tmp1330: Long = ((x2 * x2) >> 30)\n" +
   "val _tmp1331: Long = ((x1 + (_tmp1330 << 2)) >> 2)\n" +
   "val _tmp1334: Long = (((_tmp1331 << 2) - 1879048192) >> 3)\n" +
   "val _tmp1332: Long = ((x2 * x2) >> 30)\n" +
   "val _tmp1333: Long = ((x1 + (_tmp1332 << 2)) >> 2)\n" +
   "val _tmp1335: Long = (((_tmp1333 << 2) - 1879048192) >> 3)\n" +
   "val _tmp1337: Long = ((_tmp1334 * _tmp1335) >> 30)\n" +
   "((_tmp1336 + _tmp1337) >> 1)"
    assert(tmpVarRegex.replaceAllIn(fixedCode, "") ===
      tmpVarRegex.replaceAllIn(reference, ""))
  }

}
