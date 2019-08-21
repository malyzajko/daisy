// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools

import scala.util.parsing.combinator.RegexParsers

/**
 * Utils for working with Metalibm - creating input problemDef file and parsing output
 */
trait MetalibmUtils {

  class MetalibmOutputParser extends RegexParsers {
    def fileName: Parser[String] = """[\s\S]*The implementation can be found in file: """.r ~> """.+(\.c-)?[a-z|A-Z|/|-|_|0-9]+\.c""".r //^^ {case _ ~ x => x}  [a-z|A-Z|/|-|_|0-9]+(\.c-)?[a-z|A-Z|/|-|_|0-9]+\.c
    def reportedErr: Parser[Rational] = """[\s\S]* implErr = """.r ~ """-?\d\.\d+[eE][\+|-]\d+""".r ^^ {case _ ~ x => Rational.fromString(x)} // -?\d\.\d+[eE][\+|-]\d+""".r
    // add here parsers for whatever information has to be extracted from metalibm output
  }

  object Extractor extends MetalibmOutputParser {

    def getApproxFilename(output: String): String = {
      parse(fileName, output) match {
        case Success(result, _) => result
        case m: NoSuccess => throw new Exception(s"Failed to parse metalibm output: ${m.msg}")
      }
    }

    def getReportedError(output: String): Rational = {
      parse(reportedErr, output) match {
        case Success(result, _) => result
        case m: NoSuccess => throw new Exception(s"Failed to parse metalibm output: ${m.msg}")
      }
    }
  }

  class MetalibmSettings(val functionName: String,
                         val domain: Interval,
                         val approxError: Rational,
                         var implementationName: String,
                         val rndError: Rational,
                         val maxPolyDegree: Int = 7,
                         val minSubdomainWidth: Rational = Rational(1, 4096), // might want to adjust this parameter
                         val tableIndexWidth: Int = 0,
                         val minimalReductionRatio: Int = 1000,
                         val metaSplitMinWidth: Rational = Rational(1, 128),
                         val performExprDecomposition: Int = 0,
                         val adaptWorkingPrecision: Boolean = false,
                         val maxDegreeReconstruction: Int = 7,
                         val metalibmPath: Option[String] = scala.util.Properties.envOrNone("METALIBM_PATH"), // "/Users/anastasiia/Documents/metalibm/metalibm-lutetia",
                         val problemdefFile: String = scala.util.Properties.userDir + "/output/problemdef.sollya"
                        ) {

    def toProblemDef: String = {
      // for the f(x) in the future change (x) to meaningful argument
      val execute = metalibmPath match {
        case None => throw new DaisyFatalError(Some("Cannot find Metalibm. Check that it is installed and the path is in METALIBM_PATH"))
        case Some(x) => x
      }
      s"""execute("$execute/inverse.sollya");
        |
        |f = $functionName(x);
        |dom = ${domain.toString};
        |target = ${approxError.toString};
        |maxDegree = $maxPolyDegree;
        |minWidth = (sup(dom) - inf(dom)) * $minSubdomainWidth;
        |performExpressionDecomposition = $performExprDecomposition;
        |metaSplitMinWidth = (sup(dom) - inf(dom)) * $metaSplitMinWidth;
        |tableIndexWidth = $tableIndexWidth;
        |minimalReductionRatio = $minimalReductionRatio;
        |adaptWorkingPrecision = $adaptWorkingPrecision;
        |maxDegreeReconstruction = $maxDegreeReconstruction;
      """.stripMargin
    }
  }
}


