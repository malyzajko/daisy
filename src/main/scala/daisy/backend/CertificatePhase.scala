package daisy
package backend

import scala.collection.immutable.Seq

import lang.Trees._
import lang.Types._
import lang.Identifiers._
import tools.{Interval, Rational}
import tools.FinitePrecision._
import lang.Extractors.ArithOperator
import lang.TreeOps.allVariablesOf

object CertificatePhase extends DaisyPhase {

  override val name = "Certificate Creation"
  override val description = "Generates certificate for chosen theorem prover to check"

  //Map tracking which number has been assigned to which identifier
  var identifierNums :Map [Identifier, Int] = Map[Identifier, Int] ()
  //Map tracking which theorem prover name is associated with a subexpression
  var expressionNames :Map [Expr, String] = Map[Expr, String] ()
  var reporter:Reporter = null

  var fixedPointGeneration = false

  var constPrecision:Precision = null
  val shortName:String = "Certificate generation"

  def runPhase(ctx: Context, prg: Program): (Context, Program) = {

    reporter = ctx.reporter
    //must be set when entering this phase --> should not crash
    val prover = ctx.option[String]("certificate")

    constPrecision = ctx.option[Precision]("precision")
    val mixedPrecision: Boolean = ctx.option[Option[String]]("mixed-precision") != None
    fixedPointGeneration = constPrecision match {
      case x @ FixedPrecision(b) => true
      case _ => false
    }

    //The full certificate as a string
    val fullCertificate = new StringBuilder();

    reporter.info(s"\nStarting $name")

    def writeToFile (fileContent: String, prover: String): String = {
      import java.io.FileWriter
      import java.io.BufferedWriter

      val fileLocation =
        if (prover == "coq")
          s"./output/certificate_${prg.id}.v"
        else
          if (prover == "hol4")
          s"./output/certificate_${prg.id}Script.sml"
        else
          s"./output/certificate_${prg.id}.txt"

      val fstream = new FileWriter(fileLocation)
      val out = new BufferedWriter(fstream)
      out.write(fileContent)
      out.close
      fileLocation
    }

    //First write the imports
    fullCertificate ++= getPrelude(prover, "certificate_" + prg.id)

    //Add some spacing for readability
   fullCertificate ++= "\n"
    var num_of_functions = 0

    for (fnc <- prg.defs) if (!fnc.precondition.isEmpty && !fnc.body.isEmpty){
      reporter.info(s"Generating certificate for ${fnc.id}")

      val precisionMap: Map[Identifier, Precision] =
        if (mixedPrecision) {
          ctx.specInputPrecisions.get(fnc.id).get
        } else {
          allVariablesOf(fnc.body.get).map(id => (id -> constPrecision)).toMap
        }

      //first retrieve the values
      //these should be defined, otherwise the generation will fail
      val thePrecondition = fnc.precondition.get
      val theBody = (changeType (fnc.body.get,constPrecision, precisionMap))._1
      val errorMap = ctx.intermediateAbsErrors.get(fnc.id).get
      val rangeMap = ctx.intermediateRanges.get(fnc.id).get
      val queryMap = ctx.intermediateQueries.getOrElse(fnc.id, Map())

      val subdivIntvs = ctx.subdivIntervals.getOrElse(fnc.id, Seq())
      val subdivResults = ctx.subdivResults.getOrElse(fnc. id, Seq())

      // the definitions for the whole expression
      val (theDefinitions, lastGenName) =
        getValues(theBody, precisionMap,
          constPrecision,
          rangeMap,
          errorMap,
          prover)

      val (defVars, defVarsName) = getDefVars(fnc, precisionMap, rangeMap, errorMap, prover)

      // generate results for subdivisions
      val (subdivText, subdivName) = getSubdivs(fnc, theBody, thePrecondition, subdivIntvs, subdivResults,
        precisionMap, prover)

      // generate the precondition
      val (thePreconditionFunction, functionName) = getPrecondFunction(thePrecondition, fnc.id.toString, prover)
      // the analysis result function
      val (analysisResultText, analysisResultName) =
        getAbsEnvDef(theBody, errorMap, rangeMap, precisionMap, fnc.id.toString, prover)
      // generate queries
      val adjustedQueryMap = queryMap.mapValues({ case (loQ, hiQ) => (adjustQuery(loQ), adjustQuery(hiQ)) })
      val (queriesText, queriesName) = getQueriesDef(theBody, adjustedQueryMap, fnc.id.toString, prover)
      // generate the final evaluation statement
      val functionCall = getComputeExpr(lastGenName, analysisResultName, functionName, queriesName, subdivName,
        defVarsName, fnc.id.toString, prover)

      // compose the strings and append to certificate
      fullCertificate ++= theDefinitions + "\n\n" +
        defVars + "\n\n" +
        subdivText + "\n\n" +
        thePreconditionFunction + "\n\n" +
        analysisResultText + "\n\n" +
        queriesText + "\n\n"

      fullCertificate ++= functionCall

      num_of_functions += 1
    }
    if (prover == "hol4")
      fullCertificate ++= "\n\n val _ = export_theory();"

    if (prover == "binary")
      fullCertificate.insert(0,s"100 $num_of_functions ")

    //end iteration, write certificate
    writeToFile(fullCertificate.mkString,prover)

    (ctx, prg)
  }

  private val nextConstantId = { var i = 0; () => { i += 1; i} }

  private def getPrelude(prover: String, fname: String): String=
    if (prover == "coq")
      "Require Import Flover.CertificateChecker."
    else if (prover == "hol4")
      "open preamble FloverTactics AbbrevsTheory MachineTypeTheory CertificateCheckerTheory FloverMapTheory;\nopen simpLib realTheory realLib RealArith;\n\n" +
      "val _ = new_theory \"" + fname +"\";\n\n"
    else
      ""

  /*
   * As fixed-point precisions need to know the range of values to get the current
   * exponent, the interval is an additional parameter for the function
   */
  private def precisionToString(p: Precision, i:Interval): String =
  {
    p match {
      case Float32 => "M32"
      case Float64 => "M64"
      // Fixed-Points are parametric in the bit-length
      case x @ FixedPrecision(b) => s"(F $b ${x.fractionalBits(i)})"
      // case DoubleDouble => "M128"
      // case QuadDouble => "M256"fractional
      case _ => reporter.fatalError ("In precisionToString, unknown precision.")
    }
  }

  private def precisionToBinaryString(p: Precision, i:Interval): String =
  {
    p match {
      case Float32 => "32"
      case Float64 => "64"
      // case DoubleDouble => "M128"
      // case QuadDouble => "M256"
      case x @ FixedPrecision(b) => s"F $b ${x.fractionalBits(i)}"
      case _ => reporter.fatalError ("In precisionToString, unknown precision.")
    }
  }

  private def coqVariable(vname: Identifier): (String, String) =
  {
    val name = s"$vname${vname.globalId}"
    val theExpr = s"Definition $name :expr Q := Var Q ${vname.globalId}.\n"
    (theExpr, name)
  }

  private def hol4Variable(vname: Identifier): (String, String) =
  {
    val name = s"$vname${vname.globalId}"
    val theExpr = s"val ${name}_def = Define `$name:(real expr) = Var ${vname.globalId}`;\n"
    (theExpr, name)
  }

  private def coqConstant(r: RealLiteral, id: Int, precision: Precision, i:Interval): (String, String) =
    r match {
      case RealLiteral(v) =>
        //FIXME: Ugly Hack to get disjoint names for multiple function encodings with same variable names:
        val freshId = nextConstantId()
        val prec = precisionToString(precision,i)
        val rationalStr = v.toFractionString
        val coqRational = rationalStr.replace('/', '#')
        val name = s"C$id$freshId"
        val theExpr = s"Definition $name :expr Q := Const $prec ($coqRational).\n"
        (theExpr, name)
    }

  private def hol4Constant(r: RealLiteral, id: Int, precision: Precision, i:Interval): (String, String) =
    r match {
      case RealLiteral(v) =>
        //FIXME: Ugly Hack to get disjoint names for multiple function encodings with same variable names:
        val freshId = nextConstantId()
        val prec = precisionToString(precision, i)
        val rationalStr = v.toFractionString
        val name = s"C$id$freshId"
        val theExpr = s"val ${name}_def = Define `$name:(real expr) = Const $prec ($rationalStr)`;\n"
        (theExpr, name)
    }

  private def coqBinOp (e: Expr, nameLHS: String, nameRHS: String): (String, String) =
    {
      val name = s"e${nextConstantId()}"
      e match {
        case x @ Plus(lhs, rhs) =>
          (s"Definition $name :expr Q := Binop Plus $nameLHS $nameRHS.\n",
            name)
        case x @ Minus(lhs, rhs) =>
          (s"Definition $name :expr Q := Binop Sub $nameLHS $nameRHS.\n",
            name)
        case x @ Times(lhs, rhs) =>
          (s"Definition $name :expr Q := Binop Mult $nameLHS $nameRHS.\n",
            name)
        case x @ Division(lhs, rhs) =>
          (s"Definition $name :expr Q := Binop Div $nameLHS $nameRHS.\n",
            name)
        case x @ _ =>
          reporter.fatalError("Unsupported value")
      }
    }

    private def hol4BinOp (e: Expr, nameLHS: String, nameRHS: String): (String, String) =
    e match {
      case x @ Plus(lhs, rhs) =>
        ("val Plus"+ nameLHS + nameRHS + " = Define `Plus"+ nameLHS + nameRHS + s":(real expr) = Binop Plus $nameLHS $nameRHS`;;\n",
          "Plus"+ nameLHS + nameRHS)
      case x @ Minus(lhs, rhs) =>
        ("val Sub"+ nameLHS + nameRHS + " = Define `Sub"+ nameLHS + nameRHS + s":(real expr) = Binop Sub $nameLHS $nameRHS`;;\n",
          "Sub"+ nameLHS + nameRHS)
      case x @ Times(lhs, rhs) =>
        ("val Mult"+ nameLHS + nameRHS + " = Define `Mult"+ nameLHS + nameRHS + s":(real expr) = Binop Mult $nameLHS $nameRHS`;;\n",
          "Mult"+ nameLHS + nameRHS)
      case x @ Division(lhs, rhs) =>
        ("val Div"+ nameLHS + nameRHS + " = Define `Div"+ nameLHS + nameRHS + s":(real expr) = Binop Div $nameLHS $nameRHS`;;\n",
          "Div"+ nameLHS + nameRHS)
       case x @ _ =>
       reporter.fatalError("Unsupported value")
    }

  private def coqCast(nameOp: String, prec: Precision, i:Interval): (String, String) = {
    val str = precisionToString(prec, i)
    val name = s"Cast${str}$nameOp"
    (s"Definition $name :expr Q := Downcast $str $nameOp.\n", name)
  }

  private def hol4Cast(nameOp: String, prec: Precision, i:Interval): (String, String) = {
    val str = precisionToString(prec, i)
    val name = s"Cast${str}$nameOp"
    (s"val $name = Define `$name : real expr = Downcast $str $nameOp`;;\n", name)
  }

  private def coqUMin (e: Expr, nameOp: String): (String, String) =
    (s"Definition UMin${nameOp} :expr Q := Unop Neg $nameOp.\n", s"UMin${nameOp}")

  private def hol4UMin (e: Expr, nameOp: String): (String, String) =
    (s"val UMin${nameOp} = Define `UMin${nameOp}:(real expr) = Unop Neg ${nameOp}`;\n",
      s"UMin${nameOp}")

  private def coqFma (e1Name:String, e2Name:String, e3Name:String) :(String, String) = {
    val theName = s"FMA${nextConstantId()}"
    (s"Definition $theName :expr Q := Fma $e1Name $e2Name $e3Name.\n",theName)
  }

  private def hol4Fma (e1Name:String, e2Name:String, e3Name:String) :(String, String) = {
    val theName = s"FMA${nextConstantId()}"
    (s"val ${theName}_def = Define `\n $theName = Fma $e1Name $e2Name $e3Name`;\n",theName)
  }

  private def getOrRec (e:Expr, rangeMap: Map[Expr, Interval]) :Interval=
    e match {
      case x @ Cast (e, tp) => getOrRec (e, rangeMap)
      case x @ _ => rangeMap (e) }

  private def getValues(e: Expr, precisions: Map[Identifier, Precision],
    constPrecision: Precision,
    rangeMap: Map[Expr, Interval],
    errorMap: Map[Expr, Rational],
    prv: String): (String, String) = {
    //if the expression has already been defined before
    if (expressionNames.contains(e)){
      ("", expressionNames(e))
    } else {
      val realRange = getOrRec (e, rangeMap)
      val fprange = realRange+/-(errorMap(e))
      e match {
        case x @ Variable(id) =>
          if (identifierNums.contains(id)){
            ("", expressionNames(e))
          }else{
            identifierNums += (id -> id.globalId)
            val (definition, name) =
              if (prv == "coq"){
                coqVariable (id)
              }else if (prv == "hol4"){
                hol4Variable (id)
              }else {
                (s"Var ${id.globalId}", s"Var ${id.globalId}")
              }
            expressionNames += (e -> name)
            (definition, name)
          }

        case x @ RealLiteral(r) =>
          val (definition, name) =
            if (prv == "coq")
              coqConstant (x,nextConstantId(), constPrecision, fprange)
            else if (prv == "hol4")
              hol4Constant (x, nextConstantId(), constPrecision, fprange)
            else {
              val fracText = r.toFractionString.replace("/","#").replace("(","").replace(")","").replace("-","~")
              val text = s"$fracText MTYPE ${precisionToBinaryString(constPrecision, fprange)} "
              (text, text)
            }
          expressionNames += (e -> name)
          (definition, name)

        case x @ Plus(lhs, rhs) =>
          val (lhsText, lhsName) = getValues(lhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (rhsText, rhsName) = getValues(rhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (definition, name) =
            if (prv == "coq"){
              val (binOpDef, binOpName) = coqBinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else if (prv == "hol4") {
              val (binOpDef, binOpName) = hol4BinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else {
              val text = s"+ $lhsName $rhsName"
              (text, text)
            }
          expressionNames += (e -> name)
          (definition, name)

        case x @ Minus(lhs, rhs) =>
          val (lhsText, lhsName) = getValues(lhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (rhsText, rhsName) = getValues(rhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (definition, name) =
            if (prv == "coq"){
              val (binOpDef, binOpName) = coqBinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else if (prv == "hol4") {
              val (binOpDef, binOpName) = hol4BinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else {
              val text = s"- $lhsName $rhsName"
              (text, text)
            }
          expressionNames += (e -> name)
          (definition, name)

        case x @ Times(lhs, rhs) =>
          val (lhsText, lhsName) = getValues(lhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (rhsText, rhsName) = getValues(rhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (definition, name) =
            if (prv == "coq"){
              val (binOpDef, binOpName) = coqBinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else if (prv == "hol4") {
              val (binOpDef, binOpName) = hol4BinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else {
              val text = s"* $lhsName $rhsName"
              (text, text)
            }
          expressionNames += (e -> name)
          (definition, name)

        case x @ Division(lhs, rhs) =>
          val (lhsText, lhsName) = getValues(lhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (rhsText, rhsName) = getValues(rhs, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (definition, name) =
            if (prv == "coq"){
              val (binOpDef, binOpName) = coqBinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else if (prv == "hol4") {
              val (binOpDef, binOpName) = hol4BinOp(e, lhsName, rhsName)
              (lhsText + rhsText + binOpDef, binOpName)
            } else {
              val text = s"/ $lhsName $rhsName"
              (text, text)
            }
          expressionNames += (e -> name)
          (definition, name)

        case x @ UMinus (exp) =>
          val (opDef, opName) = getValues(exp, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (defintion, name) =
            if (prv == "coq") {
              val (unopDef, unopName) = coqUMin(e, opName)
              (opDef + unopDef, unopName)
            } else if (prv == "hol4"){
              val (unopDef, unopName) = hol4UMin(e, opName)
              (opDef + unopDef, unopName)
            } else {
              val text = s"~ $opName"
              (text,text)
            }
          expressionNames += (e -> name)
          (defintion, name)

        case x @ Cast(exp, tpe) =>
          val (expDef, expName) = getValues(exp, precisions, constPrecision, rangeMap, errorMap, prv)
          val tpe_prec = tpe match { case FinitePrecisionType(t) => t }
          val (definition, name) =
            if (prv == "coq") {
              val (dDef, dName) = coqCast(expName, tpe_prec, fprange)
              (expDef + dDef, dName)
            } else if (prv == "hol4") {
              val (dDef, dName) = hol4Cast(expName, tpe_prec, fprange)
              (expDef + dDef, dName)
            } else {
              val text = s"Cast MTYPE ${precisionToBinaryString(tpe_prec, fprange)} $expName"
              (text,text)
            }
          expressionNames += (e -> name)
          (definition, name)

        case x @ FMA (e1,e2,e3) =>
          val (e1Text, e1Name) = getValues(e1, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (e2Text, e2Name) = getValues(e2, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (e3Text, e3Name) = getValues(e3, precisions, constPrecision,
            rangeMap, errorMap, prv)
          val (definition, name) =
            if (prv == "coq"){
              val (fmaDef, fmaName) = coqFma(e1Name, e2Name, e3Name)
              // TODO: see for [[daisy.tools.RangeEvaluators.evalRange]] (the case for FMA).
              //  The multiplaction might use some SMT queries.
              val multName = s"${fmaName}mult"
              val multDef = s"Definition $multName :expr Q := Binop Mult $e1Name $e2Name.\n"
              expressionNames += (Times(e1, e2) -> multName)
              (e1Text + e2Text + e3Text + multDef + fmaDef, fmaName)
            } else if (prv == "hol4") {
              val (fmaDef, fmaName) = hol4Fma(e1Name, e2Name, e3Name)
              (e1Text + e2Text + e3Text + fmaDef, fmaName)
            } else {
              val text = s"FMA $e1Name $e2Name $e3Name"
              (text, text)
            }
          expressionNames += (e -> name)
          (definition, name)

        case _ @ Let(x, e1, e2) =>
          val fprange = rangeMap(Variable(x))+/-(errorMap(Variable(x)))
          // first compute e1 AST
          val (e1Def, e1Name) = getValues(e1, precisions, constPrecision, rangeMap, errorMap, prv)
          // now allocate a new variable
          val varId = x.globalId
          identifierNums += (x -> varId)
          val (varDef, varName) =
            if (prv == "coq"){
              coqVariable (x)
            } else if (prv == "hol4"){
              hol4Variable (x)
            } else {
              val text = s"Var ${varId.toString}"
              val textWithType = s"$text MTYPE ${precisionToBinaryString(precisions(x),fprange)}"
              (textWithType, text)
            }
          expressionNames += (Variable(x) -> varName)
          // now recursively compute e2
          val (e2Def, e2Name) = getValues(e2, precisions, constPrecision, rangeMap, errorMap, prv)
          val prec_x = precisionToString(precisions(x), fprange)
          val (letDef, letName) =
            if (prv == "coq"){
              val letName = s"Let${nextConstantId()}"
              (s"Definition $letName := Let $prec_x $varId $e1Name $e2Name.\n", letName)
            } else if (prv == "hol4") {
              val letName = s"Let${nextConstantId()}"
              (s"val ${letName}_def = Define `$letName = Let $prec_x $varId $e1Name $e2Name`;\n", letName)
            } else {
              val text = s"Let $varDef $e1Name $e2Def"
              (text, text)
            }
          expressionNames += (e -> letName)
          if (prv == "binary")
            (letDef, letDef)
          else
            (e1Def + varDef + e2Def + letDef, letName)

        case x @ _ =>
          reporter.fatalError(s"Unsupported operation $e while generating expression")
      }
    }
  }

  // TODO: remove if really not needed
  private def getCmd(e: Expr, precisions: Map[Identifier, Precision],
    constPrecision: Precision,
    rangeMap: Map[Expr, Interval],
    errorMap: Map[Expr, Rational],
    prv: String): (String, String) = {
    e match {
      case e @ Let(x, exp, g) =>
        val fprange = rangeMap(Variable(x))+/-(errorMap(Variable(x)))
        //first compute expression AST
        val (expDef, expName) = getValues(exp, precisions, constPrecision, rangeMap, errorMap, prv)
        //now allocate a new variable
        val varId = x.globalId
        identifierNums += (x -> varId)
        val (varDef, varName) =
          if (prv == "coq"){
            coqVariable (x)
          }else if (prv == "hol4"){
            hol4Variable (x)
          } else {
            val text = s"Var ${varId.toString}"
            val textWithType = s"$text MTYPE ${precisionToBinaryString(precisions(x),fprange)}"
            (textWithType, text)
          }
        expressionNames += (Variable(x) -> varName)
        //now recursively compute the command
        val (cmdDef, cmdName) = getCmd(g, precisions, constPrecision, rangeMap, errorMap, prv)
        val prec_x = precisionToString(precisions(x), fprange)
        val letName = "Let"+varName+expName+cmdName
        val letDef =
          if (prv == "coq"){
            s"Definition $letName := Let $prec_x $varId $expName $cmdName.\n"
          } else if (prv == "hol4") {
            s"val ${letName}_def = Define `$letName = Let $prec_x $varId $expName $cmdName`;\n"
          } else {
            s"Let $varDef $expName $cmdDef"
          }
        if (prv == "binary")
          (letDef, letDef)
        else
          (expDef + varDef + cmdDef + letDef, letName)

      case e @ _ =>
        //return statement necessary
        val (expDef, expName) = getValues (e, precisions, constPrecision, rangeMap, errorMap, prv)
        val retName = s"Ret$expName"
        if (prv == "coq"){
          (expDef + s"Definition $retName := Ret $expName.\n", retName)
        } else if (prv == "hol4") {
          (expDef + s"val ${retName}_def = Define `$retName = Ret $expName`;\n", retName)
        } else
            (s"Ret $expName",s"Ret $expName")
    }
  }

  private def coqInterval(intv: (Rational, Rational)): String =
    intv match {
      case (lowerBound, upperBound) =>
        val lowerBoundCoq = lowerBound.toFractionString.replace('/', '#')
        val upperBoundCoq = upperBound.toFractionString.replace('/', '#')
        "(" + lowerBoundCoq + ", " + upperBoundCoq + ")"
    }

  private def hol4Interval(intv: (Rational, Rational)): String =
    intv match {
      case (lowerBound, upperBound) =>
        "( " + lowerBound.toFractionString + ", " + upperBound.toFractionString + ")"
    }

  private def binaryInterval(intv:(Rational,Rational)) :String =
    intv match {
      case (lowerBound, upperBound) =>
        val loTmp = lowerBound.toFractionString.replace("(","").replace(")","")
        val hiTmp = upperBound.toFractionString.replace("(","").replace(")","")
        val lo = loTmp.replace("/","#").replace("-","~")
        val hi = hiTmp.replace("/", "#").replace("-","~")
        s"$lo $hi"
    }

  private def coqPrecondition (ranges: Map[Identifier, (Rational, Rational)], addCond: Seq[Expr], fName: String):
  (String, String) = {
    var theIntervals = "FloverMap.empty intv"
    for ((id, intv) <- ranges) {
      val ivCoq = coqInterval(intv)
      //variable must already have a binder here!
      //TODO: assert it?
      val (_, varName) = coqVariable(id)
      theIntervals = s"FloverMap.add $varName $ivCoq ($theIntervals)"
    }
    val addCondStr = addCond.foldRight("TrueQ")((q, str) => s"(AndQ ${coqQuery(q)} $str)")
    val precondName = s"thePrecondition_$fName"
    val thePrecond = s"Definition $precondName :=\n($theIntervals,\n$addCondStr)."
    (thePrecond, precondName)
  }

  private def hol4Precondition (ranges: Map[Identifier, (Rational, Rational)], fName: String): (String, String) =
  {
    var theFunction = s"val thePrecondition_${fName}_def = Define ` \n thePrecondition${fName} (n:num):interval = \n"
    for ((id, intv) <- ranges) {
      val ivHolLight = hol4Interval(intv)
      theFunction += "if n = " + identifierNums(id) + " then " + ivHolLight + " else "
    }
    theFunction += "(0,1)`;"
    (theFunction, s"thePrecondition${fName}")
  }

  private def binaryPrecondition (ranges:Map [Identifier, (Rational, Rational)],fName:String) :(String, String) =
  {
    var theFunction = " PRE "
    for ((id,intv) <- ranges) {
      val ivBin = binaryInterval(intv)
      theFunction += s"? Var ${identifierNums(id)} $ivBin "
    }
    (theFunction, "")
  }

  private def getPrecondFunction(pre: Expr, rangeMap: Option[Map[Identifier, Interval]], fName: String, prv: String):
  (String, String) = {
    val (rangesPrecond, errors, addCond) = daisy.analysis.SpecsProcessingPhase.extractPreCondition(pre)
    if (! errors.isEmpty){
      reporter.fatalError("Errors on inputs are currently unsupported")
    }
    val ranges = if (rangeMap.isEmpty) rangesPrecond else rangeMap.get.mapValues({ intv => (intv.xlo, intv.xhi)})
    if (prv == "coq"){
      coqPrecondition(ranges, addCond, fName)
    }else if (prv == "hol4"){
      hol4Precondition(ranges,fName)
    } else {
      binaryPrecondition(ranges,fName)
    }
  }

  private def getPrecondFunction(pre: Expr, fName: String, prv: String): (String, String) =
    getPrecondFunction(pre, None, fName, prv)

  private def getDefVars(fnc: FunDef, precMap: Map[Identifier, Precision],
    rangeMap:Map[Expr, Interval],
    errorMap:Map[Expr, Rational],
    prv: String): (String, String) = {
    if (prv == "coq")
      coqDefVars(fnc, precMap, rangeMap, errorMap)
    else if(prv == "hol4")
      hol4DefVars(fnc, precMap, rangeMap, errorMap)
    else if (prv=="binary")
      binaryDefVars(fnc,precMap, rangeMap, errorMap)
    else
      reporter.fatalError("Unknown theorem prover in getDefVars call)")
  }

    private def coqDefVars (fnc: FunDef, precMap: Map[Identifier, Precision],
      rangeMap:Map[Expr, Interval],
    errorMap:Map[Expr, Rational]): (String, String) =
  {
    val fName = fnc.id.toString
    val theFunction = s"Definition defVars_$fName: FloverMap.t mType :=\n"
    if (! fixedPointGeneration) {
      var akk = "(FloverMap.empty mType)"
      for (variable <- allVariablesOf(fnc.body.get)) {
        val fpRange=rangeMap(Variable(variable))+/-(errorMap(Variable(variable)))
        akk = s"(FloverMap.add (Var Q ${identifierNums(variable)}) (${precisionToString(precMap(variable), fpRange)}) $akk)"
      }
      (theFunction + akk + ".\n", s"defVars_$fName")
    } else {
      val (resText, p) = getFBitMap(fnc.body.get, precMap, constPrecision, rangeMap, errorMap,
        getEmptyFixedEnv("coq"), "coq")
      (theFunction + resText + ".", s"defVars_$fName")
    }
  }

  private def hol4DefVars (fnc: FunDef, precMap: Map[Identifier, Precision],
      rangeMap:Map[Expr, Interval],
    errorMap:Map[Expr, Rational]): (String, String) =
  {
    val fName = fnc.id.toString
    val theFunction = s"val defVars_${fName}_def = Define `\n defVars_$fName : typeMap = \n"
    if (! fixedPointGeneration) {
      var akk = "(FloverMapTree_empty)"
      for (variable <- allVariablesOf(fnc.body.get)) {
        val fpRange=rangeMap(Variable(variable))+/-(errorMap(Variable(variable)))
        akk = s"(FloverMapTree_insert (Var ${identifierNums(variable)}) (${precisionToString(precMap(variable), fpRange)}) $akk)"
      }
      (theFunction + akk + "`;\n", s"defVars_$fName")
    } else {
      val (resText, p) = getFBitMap(fnc.body.get, precMap, constPrecision, rangeMap, errorMap,
        getEmptyFixedEnv("hol4"), "hol4")
      (theFunction + resText, s"defVars_$fName")
    }
  }

  private def binaryDefVars (fnc: FunDef, precMap: Map[Identifier, Precision],
      rangeMap:Map[Expr, Interval],
    errorMap:Map[Expr, Rational]): (String, String) =
  {
    var theFunction = "GAMMA "
    if (! fixedPointGeneration) {
      for (variable <- allVariablesOf(fnc.body.get)) {
        val fpRange=rangeMap(Variable(variable))+/-(errorMap(Variable(variable)))
        //DVAR :: DCONST n :: MTYPE m
        theFunction += s"Var ${identifierNums(variable)} MTYPE ${precisionToBinaryString(precMap(variable), fpRange)} "
      }
      theFunction += "\n"
      (theFunction, s"")
    } else {
      val (resText, p) = getFBitMap(fnc.body.get, precMap, constPrecision, rangeMap, errorMap,
        getEmptyFixedEnv("binary"), "binary")
      (theFunction + resText, s"")
    }
  }

  private def getSubdivs(fnc: FunDef, e: Expr, precond: Expr, subdivIntvs: scala.Seq[Map[Identifier, Interval]],
    subdivResults: scala.Seq[(Map[Expr, Rational], Map[Expr, Interval], Map[Expr, (Expr, Expr)])],
    precMap: Map[Identifier, Precision], prover: String): (String, String) = {
    var subdivPreconds = ""
    var subdivQueries = ""
    var subdivText = ""
    var subdivs: Seq[String] = Seq()
    var subdivCtr = 0
    (subdivIntvs, subdivResults).zipped.map({ case (subdivIntv, (subdivErrorMap, subdivRangeMap, subdivQueryMap)) =>
      val (thePreconditionFunction, thePrecondName) =
        getPrecondFunction(precond, Some(subdivIntv), s"${fnc.id}_sub$subdivCtr", prover)
      // the analysis result function
      val (analysisResultText, analysisResultName) = getAbsEnvDef(e, subdivErrorMap, subdivRangeMap,
        precMap, s"${fnc.id}_sub$subdivCtr", prover)
      // generate queries
      val adjustedQueryMap = subdivQueryMap.mapValues({ case (loQ, hiQ) => (adjustQuery(loQ), adjustQuery(hiQ)) })
      val (queriesText, queriesName) = getQueriesDef(e, adjustedQueryMap, s"${fnc.id}_sub$subdivCtr", prover)
      // TODO queries
      subdivPreconds += thePreconditionFunction + "\n\n"
      subdivQueries += queriesText + "\n\n"
      subdivText += analysisResultText + "\n\n"
      subdivs = subdivs :+ s"($thePrecondName, $analysisResultName, $queriesName)"
      subdivCtr += 1
    })
    val (subdivDef, subdivName) =
      if (prover == "coq") {
        val name = s"subdivs_${fnc.id}"
        (subdivs.mkString(s"Definition $name: list (precond * analysisResult * usedQueries) := [\n", ";", "]."), name)
      } else ("", "") // TODO: include other provers

    subdivText += subdivDef
    (subdivPreconds + subdivQueries + subdivText, subdivName)
  }

  private def coqAbsEnv (e: Expr, errorMap: Map[Expr, Rational], rangeMap: Map[Expr, Interval], precisions: Map[Identifier, Precision], cont: String): String =
  {
    val nameE = expressionNames(e)

    val intvE = e match{ case x @Cast (e,t) => coqInterval((rangeMap(e).xlo, rangeMap(e).xhi))
      case x @ _ => coqInterval ((rangeMap(x).xlo, rangeMap(x).xhi))}

    val errE = errorMap(e).toFractionString.replace("/", "#")

    val continuation =
      e match {

        case x @ Variable(id) => cont

        case x @ RealLiteral(r) => cont

        case x @ Plus(lhs, rhs) =>
          val lFun = coqAbsEnv (lhs, errorMap, rangeMap, precisions, cont)
          coqAbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

        case x @ Minus(lhs, rhs) =>
          val lFun = coqAbsEnv (lhs, errorMap, rangeMap, precisions, cont)
          coqAbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

        case x @ Times(lhs, rhs) =>
          val lFun = coqAbsEnv (lhs, errorMap, rangeMap, precisions, cont)
          coqAbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

        case x @ Division(lhs, rhs) =>
          val lFun = coqAbsEnv (lhs, errorMap, rangeMap, precisions, cont)
          coqAbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

        case x @ UMinus(e) =>
          coqAbsEnv (e, errorMap, rangeMap, precisions, cont)

        case x @ Cast(e, t) =>
          coqAbsEnv (e, errorMap, rangeMap, precisions, cont)

        case x @ FMA (e1,e2,e3) =>
          val e1Fun = coqAbsEnv (e1, errorMap, rangeMap, precisions, cont)
          val e2Fun = coqAbsEnv (e2, errorMap, rangeMap, precisions, e1Fun)
          coqAbsEnv (e3, errorMap, rangeMap, precisions, e2Fun)

        case x @ Let (y, e1, e2) =>
          val e1Fun = coqAbsEnv(e1, errorMap, rangeMap, precisions, cont)
          val yFun = coqAbsEnv(Variable(y), errorMap, rangeMap, precisions, e1Fun)
          coqAbsEnv(e2, errorMap, rangeMap, precisions, yFun)

        case x @ _ =>
          reporter.fatalError(s"Unsupported operation $e while generating absenv")
      }
    s"FloverMap.add $nameE ($intvE, $errE) ($continuation)"
  }

  private def hol4AbsEnv (e: Expr, errorMap: Map[Expr, Rational], rangeMap: Map[Expr, Interval], precisions: Map[Identifier, Precision], cont: String): String =
  {
    e match {

      case x @ Let (y, exp, g) =>
        val expFun = hol4AbsEnv (exp, errorMap, rangeMap, precisions, cont)
        val gFun = hol4AbsEnv (g, errorMap, rangeMap, precisions, expFun)
        val intvY = hol4Interval((rangeMap(Variable(y)).xlo, rangeMap(Variable(y)).xhi))
        val errY = errorMap(Variable(y)).toFractionString
        //val nameY = expressionNames(Variable(y))
        s"FloverMapTree_insert (Var ${identifierNums(y)}) ($intvY , $errY) ($gFun)"

      case x @ _ =>

        val nameE = expressionNames(e)

        val intvE = e match{ case x @Cast (e,t) => hol4Interval((rangeMap(e).xlo, rangeMap(e).xhi))
          case x @ _ => hol4Interval ((rangeMap(x).xlo, rangeMap(x).xhi))}

        val errE = errorMap(x).toFractionString

        val continuation =
          e match {
            case x @ Variable(id) => cont

            case x @ RealLiteral(r) => cont

            case x @ Plus(lhs, rhs) =>
              val lFun = hol4AbsEnv (lhs, errorMap, rangeMap, precisions, cont)
              hol4AbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

            case x @ Minus(lhs, rhs) =>
              val lFun = hol4AbsEnv (lhs, errorMap, rangeMap, precisions, cont)
              hol4AbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

            case x @ Times(lhs, rhs) =>
              val lFun = hol4AbsEnv (lhs, errorMap, rangeMap, precisions, cont)
              hol4AbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

            case x @ Division(lhs, rhs) =>
              val lFun = hol4AbsEnv (lhs, errorMap, rangeMap, precisions, cont)
              hol4AbsEnv (rhs, errorMap, rangeMap, precisions, lFun)

            case x @ UMinus(e) =>
              hol4AbsEnv (e, errorMap, rangeMap, precisions, cont)

            case x @ Cast(e, t) =>
              hol4AbsEnv (e, errorMap, rangeMap, precisions, cont)

            case x @ FMA (e1,e2,e3) =>
              val e1Fun = hol4AbsEnv (e1, errorMap, rangeMap, precisions, cont)
              val e2Fun = hol4AbsEnv (e2, errorMap, rangeMap, precisions, e1Fun)
              hol4AbsEnv (e3, errorMap, rangeMap, precisions, e2Fun)

            case x @ _ =>
              reporter.fatalError(s"Unsupported operation $e while generating absenv")
        }
        s"FloverMapTree_insert $nameE ($intvE, $errE) ($continuation)"
    }
  }

  private def binaryAbsEnv (e:Expr, errorMap:Map[Expr, Rational], rangeMap:Map[Expr, Interval], reporter:Reporter) :String =
  {
    e match {

      case x @ Let (y,exp, g) =>
        val expFun = binaryAbsEnv (exp, errorMap, rangeMap, reporter)
        val gFun = binaryAbsEnv (g, errorMap, rangeMap, reporter)
        val intvY = binaryInterval((rangeMap(Variable(y)).xlo, rangeMap(Variable(y)).xhi))
        // should not be necessary: .replace("-","~")
        val errY = errorMap(Variable (y)).toFractionString.replace("(","").replace(")","").replace("/","#")
        //val nameY = expressionNames(Variable(y))
        s"? Var ${identifierNums(y)} $intvY $errY $expFun $gFun"

      case x @ _ =>

        val nameE = expressionNames(e)

        val intvE = e match{ case x @Cast (e,t) => binaryInterval((rangeMap(e).xlo, rangeMap(e).xhi))
          case x @ _ => binaryInterval ((rangeMap(x).xlo, rangeMap(x).xhi))}

        val errE = errorMap(x).toFractionString.replace("(","").replace(")","").replace("/","#")

        val continuation =
          e match {
            case x @ Variable(id) => ""

            case x @ RealLiteral(r) => ""

            case x @ Plus(lhs, rhs) =>
              binaryAbsEnv (lhs, errorMap, rangeMap, reporter) +
              binaryAbsEnv (rhs, errorMap, rangeMap, reporter)

            case x @ Minus(lhs, rhs) =>
              binaryAbsEnv (lhs, errorMap, rangeMap, reporter) +
              binaryAbsEnv (rhs, errorMap, rangeMap, reporter)

            case x @ Times(lhs, rhs) =>
              binaryAbsEnv (lhs, errorMap, rangeMap, reporter) +
              binaryAbsEnv (rhs, errorMap, rangeMap, reporter)

            case x @ Division(lhs, rhs) =>
              binaryAbsEnv (lhs, errorMap, rangeMap, reporter) +
              binaryAbsEnv (rhs, errorMap, rangeMap, reporter)

            case x @ UMinus(e) =>
              binaryAbsEnv (e, errorMap, rangeMap, reporter)

            case x @ Cast(e, t) =>
              binaryAbsEnv (e, errorMap, rangeMap, reporter)

            case x @ FMA (e1,e2,e3) =>
              binaryAbsEnv (e1, errorMap, rangeMap, reporter) +
              binaryAbsEnv (e2, errorMap, rangeMap, reporter) +
              binaryAbsEnv (e3, errorMap, rangeMap, reporter)

          case x @ _ =>
            reporter.fatalError(s"Unsupported operation $e while generating absenv")
          }
        s"? $nameE $intvE $errE $continuation"
    }
  }

  def queryLt(l: Expr, r: Expr): Boolean = {
    (l, r) match {
      case (LessEquals(_, Variable(idl)), LessEquals(_, Variable(idr))) => idl.globalId < idr.globalId
      case (LessEquals(Variable(idl), _), LessEquals(Variable(idr), _)) => idl.globalId < idr.globalId
      case (LessEquals(Variable(idl), _), LessEquals(_, Variable(idr))) => idl.globalId < idr.globalId
      case (LessEquals(_, Variable(idl)), LessEquals(Variable(idr), _)) => idl.globalId <= idr.globalId
      case (LessEquals(_, Variable(idl)), _) => true
      case (LessEquals(Variable(idl), _), _) => true
      case (_, _) => false
    }
  }

  def adjustQuery(q: Expr): Expr = {
    q match {
      case And(seq) =>
        val preQuery = seq.head match {
          case And(pre) =>
            val canonicalPre = Seq.newBuilder[Expr]
            canonicalPre.sizeHint(pre)
            for (query <- pre) {
              query match {
                case GreaterEquals(lhs, rhs) => canonicalPre += LessEquals(rhs, lhs)
                case _ => canonicalPre += query
              }
            }
            And(canonicalPre.result.sortWith(queryLt))
          case pre => pre
        }
        And(preQuery +: seq.tail)
      case _ => q
    }
  }

  private def coqQuery(query: Expr): String = {
    query match {
      case x @ Variable(id) => "(VarQ " + id.globalId + ")"

      case x @ RealLiteral(r) => "(ConstQ (" + r.toFractionString.replace('/', '#') + "))"

      case x @ Plus(lhs, rhs) =>
        "(PlusQ " + coqQuery(lhs) + " " + coqQuery(rhs) + ")"

      case x @ Minus(lhs, rhs) =>
        "(MinusQ " + coqQuery(lhs) + " " + coqQuery(rhs) + ")"

      case x @ Times(lhs, rhs) =>
        "(TimesQ " + coqQuery(lhs) + " " + coqQuery(rhs) + ")"

      case x @ Division(lhs, rhs) =>
        "(DivQ " + coqQuery(lhs) + " " + coqQuery(rhs) + ")"

      case x @ UMinus(e) =>
        "(UMinusQ " + coqQuery(e) + ")"

      case x @ BooleanLiteral(true) =>
        "TrueQ"

      case x @ BooleanLiteral(false) =>
        "FalseQ"

      case x @ And(seq) =>
        seq.foldRight("TrueQ")((q, str) => s"(AndQ ${coqQuery(q)} $str)")

      case x @ Or(seq) =>
        seq.foldRight("FalseQ")((q, str) => s"(OrQ ${coqQuery(q)} $str)")

      case x @ LessThan(lhs, rhs) =>
        "(LessQ " + coqQuery(lhs) + " " + coqQuery(rhs) + ")"

      case x @ GreaterThan(lhs, rhs) =>
        "(LessQ " + coqQuery(rhs) + " " + coqQuery(lhs) + ")"

      case x @ LessEquals(lhs, rhs) =>
        "(LessEqQ " + coqQuery(lhs) + " " + coqQuery(rhs) + ")"

      case x @ GreaterEquals(lhs, rhs) =>
        "(LessEqQ " + coqQuery(rhs) + " " + coqQuery(lhs) + ")"

      case x @Equals(lhs, rhs) =>
        "(EqualsQ " + coqQuery(lhs) + " " + coqQuery(rhs) + ")"

      case x @ _ =>
        reporter.fatalError(s"Unsupported operation $query while generating coq query")
    }
  }

  private def coqQueryMap(e: Expr, queryMap: Map[Expr, (Expr, Expr)], cont: String): String = {
    val nameE = expressionNames(e)

    val continuation =
      e match {
        case x@Variable(id) => cont

        case x@RealLiteral(r) => cont

        case x@Plus(lhs, rhs) =>
          val lFun = coqQueryMap(lhs, queryMap, cont)
          coqQueryMap(rhs, queryMap, lFun)

        case x@Minus(lhs, rhs) =>
          val lFun = coqQueryMap(lhs, queryMap, cont)
          coqQueryMap(rhs, queryMap, lFun)

        case x@Times(lhs, rhs) =>
          val lFun = coqQueryMap(lhs, queryMap, cont)
          coqQueryMap(rhs, queryMap, lFun)

        case x@Division(lhs, rhs) =>
          val lFun = coqQueryMap(lhs, queryMap, cont)
          coqQueryMap(rhs, queryMap, lFun)

        case x@UMinus(e) =>
          coqQueryMap(e, queryMap, cont)

        case x@Cast(e, t) =>
          coqQueryMap(e, queryMap, cont)

        case x@FMA(e1, e2, e3) =>
//          val e1Fun = coqQueryMap(e1, queryMap, cont)
//          val e2Fun = coqQueryMap(e2, queryMap, e1Fun)
          val mult = coqQueryMap(Times(e1, e2), queryMap, cont)
          coqQueryMap(e3, queryMap, mult)

        case x@Let(y, e1, e2) =>
          val e1Fun = coqQueryMap(e1, queryMap, cont)
          val yFun = coqQueryMap(Variable(y), queryMap, e1Fun)
          coqQueryMap(e2, queryMap, yFun)

        case x@_ =>
          reporter.fatalError(s"Unsupported operation $e while generating querymap")
      }

    queryMap(e) match {
      case (BooleanLiteral(false), BooleanLiteral(false)) => continuation
      case (loQ, hiQ) =>
        s"FloverMap.add $nameE (${coqQuery(loQ)}, ${coqQuery(hiQ)}) ($continuation)"
    }
  }


  private def getQueriesDef(e: Expr, queryMap: Map[Expr, (Expr, Expr)], fName: String, prv: String):
  (String, String) = {
    if (prv == "coq") {
      val qmapName = s"querymap_$fName"
      val emptyCoqMap = "FloverMap.empty (SMTLogic * SMTLogic)"
      val qmapText =
        if (queryMap.isEmpty) emptyCoqMap else coqQueryMap(e, queryMap, emptyCoqMap)
      (s"Definition $qmapName :=\n$qmapText.", qmapName)
    } else {
      // TODO take other provers into account
      reporter.fatalError(s"Unsupported prover: $prv")
    }
  }

  private def getAbsEnvDef(e: Expr, errorMap: Map[Expr, Rational], rangeMap: Map[Expr, Interval], precision: Map[Identifier, Precision], fName: String, prv: String): (String, String)=
    if (prv == "coq")
      (s"Definition absenv_${fName}: analysisResult :=\n" +
        coqAbsEnv(e, errorMap, rangeMap, precision, "FloverMap.empty (intv * error)") + ".",
        s"absenv_${fName}")
    else if (prv == "hol4")
      (s"val absenv_${fName}_def = RIGHT_CONV_RULE EVAL (Define `\n  absenv_${fName}:analysisResult = \n" +
        hol4AbsEnv(e, errorMap, rangeMap, precision, "FloverMapTree_empty") + "`);",
        s"absenv_${fName}")
    else
      ("ABS " + binaryAbsEnv(e, errorMap, rangeMap, reporter), "")

  private def getComputeExpr(lastGenName: String, analysisResultName: String, precondName: String, queriesName: String,
    subdivName: String, defVarsName: String, fName: String, prover: String): String =
    if (prover == "coq") {
      s"Theorem ErrorBound_${fName}_sound :\n" +
        s"CertificateChecker $lastGenName $analysisResultName $precondName $queriesName $subdivName $defVarsName" +
        "= true.\nProof. vm_compute; auto. Qed.\n"
    } else if (prover == "hol4"){
      "val _ = store_thm (\""+s"ErrorBound_${fName}_Sound"+"\",\n" +
      s"``CertificateCheckerCmd $lastGenName $analysisResultName $precondName $defVarsName``,\n flover_eval_tac \\\\ fs[REAL_INV_1OVER]);\n"
    } else
      ""

  def changeType(e: Expr, constPrecision:Precision, tpeMap: Map[Identifier, Precision]): (Expr, Precision) = e match {

    case Variable(id) =>
      // (Variable(id.changeType(FinitePrecisionType(tpeMap(id)))), tpeMap(id))
      (e, tpeMap(id))

    case x @ RealLiteral(r) =>
      // val tmp = FinitePrecisionLiteral(r, defaultPrecision)
      // tmp.stringValue = x.stringValue
      (x, constPrecision)

    case ArithOperator(Seq(l, r), recons) =>
      val (eLeft, pLeft) = changeType(l, constPrecision, tpeMap)
      val (eRight, pRight) = changeType(r, constPrecision, tpeMap)

      val prec = getUpperBound(pLeft, pRight)
      (recons(Seq(eLeft, eRight)), prec)

    case ArithOperator(Seq(t), recons) =>
      val (e, p) = changeType(t, constPrecision, tpeMap)
      (recons(Seq(e)), p)

    case FMA (e1,e2,e3) =>
      val (e1F,p1) = changeType(e1, constPrecision, tpeMap)
      val (e2F,p2) = changeType(e2, constPrecision, tpeMap)
      val (e3F,p3) = changeType(e3, constPrecision, tpeMap)
      (FMA (e1F,e2F,e3F), getUpperBound (getUpperBound (p1,p2),p3))

    case Let(id, value, body) =>
      val (eValue, valuePrec) = changeType(value, constPrecision, tpeMap)
      val (eBody, bodyPrec) = changeType(body, constPrecision, tpeMap)

      val idPrec = tpeMap(id)

      if (idPrec >= valuePrec) {
        (Let(id.changeType(FinitePrecisionType(tpeMap(id))), eValue, eBody), bodyPrec)
      } else {
        val newValue = Cast(eValue, FinitePrecisionType(idPrec))
        (Let(id.changeType(FinitePrecisionType(tpeMap(id))), newValue, eBody), bodyPrec)
      }
  }

  private def getEmptyFixedEnv (prover:String) :String = {
    if (prover == "coq")
      "FloverMap.empty mType"
    else if (prover == "hol4")
      "FloverMapTree_empty"
    else
      "GAMMA"
  }

  private def extendFBitMap (name:String, mType:String, akk:String, prover:String) :String = {
    if (prover == "coq")
      s"FloverMap.add $name $mType ($akk)"
    else if (prover == "hol4")
      s"FloverMapTree_insert $name $mType ($akk)"
    else
      s"$akk $name $mType"
  }

  private def getFBitMap (e:Expr, precisionMap: Map[Identifier,Precision],
    constPrecision:Precision,
    rangeMap: Map[Expr, Interval],
    errorMap: Map[Expr, Rational],
    akk:String,
    prover:String) :(String, Precision) =
    e match {
      case x @ Let (y,exp, g) =>
        val (expRes, p) = getFBitMap (exp, precisionMap, constPrecision,
          rangeMap, errorMap, akk, prover)
        val (a,b) = getFBitMap (g, precisionMap, constPrecision, rangeMap, errorMap, expRes, prover)
        val intvE = rangeMap(e)

        val errE = errorMap(e)

        val errIv = intvE +/- errE
        val name = expressionNames(e)

        val theText = extendFBitMap (name, precisionToString(b, errIv), a, prover)

        (theText, b)

      case x @ Variable(id) =>
        val intvE = rangeMap(e)

        val errE = errorMap(e)

        val errIv = intvE +/- errE
        val name = expressionNames(e)

        val prec = precisionMap(id)

        val theText = extendFBitMap (name, precisionToString(prec, errIv), akk, prover)

        (theText, prec)

      case x @ RealLiteral(r) => (akk, constPrecision)

      case x @ ArithOperator(Seq(lhs, rhs), recons) =>

        val (lhsText, pLhs) = getFBitMap (lhs, precisionMap, constPrecision, rangeMap, errorMap, akk, prover)
        val (rhsText, pRhs) = getFBitMap (rhs, precisionMap, constPrecision, rangeMap, errorMap, lhsText, prover)

        val p_join:FixedPrecision = getUpperBound (pLhs, pRhs) match
          { case x @ FixedPrecision(w) => x
            case x @ _ => reporter.fatalError(s"Unsupported type $x while generating fraction bits map") }

        val intvE = rangeMap(e)

        val errE = errorMap(e)

        val errIv = intvE +/- errE
        val name = expressionNames(e)

        val theText = extendFBitMap (name, precisionToString (p_join, errIv), rhsText, prover)

        (theText, p_join)

      case x @ UMinus(e1) =>
        val (opText, prec) = getFBitMap (e1, precisionMap, constPrecision, rangeMap, errorMap, akk, prover)

        val intvE = rangeMap(e)

        val errE = errorMap(e)

        val errIv = intvE +/- errE
        val name = expressionNames(e)

        val theText = extendFBitMap (name, precisionToString (prec, errIv), opText, prover)

        (theText, prec)

      case x @ Cast(e, t) =>
        val (text, p) = getFBitMap (e, precisionMap, constPrecision, rangeMap, errorMap, akk, prover)
      t match {
        case x @ FinitePrecisionType (p) =>
        (text, p)
        case x @ _ => reporter.fatalError (s"Unsupported Cast type $x while generating fraction bits map") }

      case x @ FMA (e1,e2,e3) =>
        val (e1Text, pE1) = getFBitMap (e1, precisionMap, constPrecision, rangeMap, errorMap, akk, prover)
        val (e2Text, pE2) = getFBitMap (e2, precisionMap, constPrecision, rangeMap, errorMap, e1Text, prover)
        val (e3Text, pE3) = getFBitMap (e3, precisionMap, constPrecision, rangeMap, errorMap, e2Text, prover)

        val p_join:FixedPrecision = getUpperBound (getUpperBound (pE1, pE2), pE3) match
          { case x @ FixedPrecision(w) => x
            case x @ _ => reporter.fatalError(s"Unsupported type $x while generating fraction bits map") }

        val intvE = rangeMap (e)
        val errE = errorMap(e)

        val errIv = intvE +/- errE
        val name = expressionNames(e)

        val theText = extendFBitMap (name, precisionToString (p_join, errIv), e3Text, prover)

        (theText, p_join)

      case x @ _ =>
        reporter.fatalError(s"Unsupported operation $e while generating fraction bits map")
    }

  private def wrapFixedEnv (content:String, name:String, prover:String) = {
    if (prover == "coq")
      (s"Definition $name := $content.", name)
    else if (prover == "hol4")
      (s"val ${name}_def = Define `\n $name:bitMap = $content `;", name)
    else (content, name)
  }
}
