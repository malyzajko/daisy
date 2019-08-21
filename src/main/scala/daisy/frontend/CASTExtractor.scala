// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package frontend

import daisy.lang.Identifiers.FreshIdentifier
import daisy.lang.Trees._
import daisy.lang.Types.{RealType, TypeTree}
import daisy.tools.{Interval, Rational, MetalibmUtils}

import scala.collection.immutable.Seq
import scala.io.Source

trait CASTExtractor extends CCodeExtraction with CReqExtraction with MetalibmUtils {

  object SpecParser extends ReqParser {
    /**
      * Parse specification from the problemdef.sollya file
      * @param specFile name of the file with specification (e.g. testcases/approx/example_spec.txt)
      * @return
      */
    def parse(specFile: String): Option[Map[String, (Interval, Rational)]] = {
      val fileContents = Source.fromFile(specFile).getLines.mkString
      parse(spec, fileContents) match {
        case Success(result, _) => Some(result)
        case _ => None
      }
    }

    /**
      * Constructs pre- and postcondition
      * parsed from problemdef.sollya file
      * @param specFile problem definition *.sollya file name
      * @param inputVar variable to put precondition on
      * @return tuple (precondition, postcondtition)
      */
    def getSpec(specFile: String, inputVar: Variable, fncName: String): (Expr, Expr) = {
      parse(specFile) match {
        case Some(mmap) if mmap.isEmpty => throw new Exception(s"Specification file is empty $specFile")
        case Some(mmap) =>
          val (i, err) = mmap.getOrElse(fncName, throw new Exception(s"Specification for the function $fncName is not in the file $specFile"))
          getPrePostConditions(i, err, inputVar)

        case None => throw new DaisyFatalError(Some(s"Specification file $specFile could not be parsed"))
      }
    }

    def getPrePostConditions(i: Interval, err: Rational, inputVar: Variable): (Expr, Expr) = {
      val assertion = And(LessEquals(RealLiteral(i.xlo), inputVar), LessEquals(inputVar, RealLiteral(i.xhi)))
      val res = FreshIdentifier("res")
      val post = Lambda(Seq(ValDef(res)), AbsError(Variable(res), RealLiteral(err)))
      (assertion, post)
    }

    def existsForFnc(fnc: String, specFile: String): Boolean = {
      parse(specFile) match {
        case Some(mmap) => mmap.contains(fnc)
        case _ => false
      }
    }

    def getAllSpecValues(specFile: String): Map[String, (Interval, Rational)] = {
      parse(specFile) match {
        case Some(mmap) => mmap
        case _ => throw new DaisyFatalError(Some(s"Specification file $specFile could not be parsed"))
      }
    }
  }

  object CodeParser extends ExpressionParser {
    def parse(progFile: String): Option[Seq[Tree]] = {
      val fileContents = Source.fromFile(progFile).getLines.mkString 
      parse(prg, fileContents) match {
        case Success(result, _) => Some(result)
        case m: NoSuccess => throw new DaisyFatalError(Some(s"Failed to extract Daisy program. ${m.msg}"))
      }
    }

    /**
      * Extracts function definition from the C file. Use for C files created outside of Daisy
      * @param progFile name of file that contains C program
      * @param specFile name of file that contains a specifiction (pre- and post-conditions)
      * @param funName name of parsed function
      * @return optional function definition of parsed function
      */
    def getFunctionDef(progFile: String, specFile: String, funName: String): Option[FunDef] = {
      parse(progFile) match {
        case Some(stmts) =>
          val extraction = new Extraction(stmts)
          // 1. Get input variable id
          val input = extraction.extractInputVar
          // 2. Get pre- and postconditions
          val (pre, post) = SpecParser.getSpec(specFile, input, funName)

          // 3. Extract function body
          val body = extraction.extractTree
          val params = extraction.extractFncVarArgs

          val funId = extraction.extractMainFncId

          val mainFnc = FunDef(
            funId,
            RealType, // TODO: if used outside of ApproxPhase, need other types
            params,
            Some(pre),
            Some(body),
            Some(post)
          )

          Some(mainFnc)

        case None => throw new DaisyFatalError(Some("Failed to extract Daisy program"))
      }
    }

    /**
      * Extracts function definition from the C file. Use for C files generated during [[daisy.opt.ApproxPhase]]
      * @param progFile name of file that contains C program
      * @param settings [[MetalibmSettings]] created during ApproxPhase
      * @param expectedReturnType expected type of return value - depends on precision assigned to the node that replaces by the call of this funciton
      * @return optional function definition of parsed function
      */
    def getFunctionDef(progFile: String, settings: MetalibmSettings, expectedReturnType: TypeTree): Option[FunDef] = {
      parse(progFile) match {
        case Some(stmts) =>
          val extraction = new Extraction(stmts)
          // 1. Get input variable id
          val input = extraction.extractInputVar
          // 2. Get pre- and postconditions
          val (pre, post) = SpecParser.getPrePostConditions(settings.domain, settings.rndError, input)

          // 3. Extract function body
          val body = extraction.extractTree
          val params = extraction.extractFncVarArgs

          val funId = FreshIdentifier(settings.implementationName, expectedReturnType)

          val mainFnc = FunDef(
            funId,
            RealType, // TODO: if used outside of ApproxPhase, need other types
            params,
            Some(pre),
            Some(body),
            Some(post)
          )

          Some(mainFnc)

        case None => throw new DaisyFatalError(Some("Failed to extract Daisy program"))
      }
    }

    def getProgram(progFile: String, specFile: String): Option[Program] = {

      val fNameIndex = progFile.lastIndexOf('/')
      val progName = progFile.substring(fNameIndex + 1, progFile.length-2)

      // TODO: for using outside of ApproxPhase - to enable parsing multiple functions change "default" name to a list of names
      val fncDefs = getFunctionDef(progFile, specFile, "default").getOrElse(throw new DaisyFatalError(Some("Failed to extract Daisy program")))

      val prg = Program(FreshIdentifier(progName), Seq(fncDefs))
      Some(prg)
    }
  }
}
