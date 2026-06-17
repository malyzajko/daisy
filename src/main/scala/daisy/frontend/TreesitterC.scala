package daisy
package frontend

import io.github.treesitter.jtreesitter.Node
import daisy.lang.Identifiers._
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, Program => DaisyProgram, ValDef => DaisyValDef, _}
import daisy.lang.Constructors._
import daisy.tools.Rational

trait TreesitterC extends TreesitterCommon {
  protected final def convertCNode(raw: Node): DaisyExpr =
    {
      val node = unwrapExpr(raw)
      node.getType match {
        case "binary_expression" | "logical_expression" =>
          val (leftNodeOpt, opTokenOpt, rightNodeOpt) = findOperatorTripletFromAllChildren(node)
          convertBinaryByParts(
            node = node,
            leftNodeOpt = leftNodeOpt,
            opTokenOpt = opTokenOpt,
            rightNodeOpt = rightNodeOpt,
            contextLabel = node.getType,
            allowRawArithmeticFallback = true
          )

        case "compound_statement" =>
          def isGuardedLetAsVar(e: DaisyExpr): Option[(Identifier, DaisyExpr)] = e match {
            case Let(id, value, Variable(v)) if id == v => Some((id, value))
            case _ => None
          }

          val stmts = allNamedChildren(node).map(convertNode).filter(_ != null).toList
          val initialAcc: DaisyExpr =
            stmts.lastOption.flatMap(isGuardedLetAsVar).map { case (id, _) => Variable(id) }
              .getOrElse(RealLiteral(Rational.zero))

          val result: DaisyExpr = stmts match {
            case decl :: (ie @ IfExpr(_, thenB, elseB)) :: tail =>
              isGuardedLetAsVar(decl) match {
                case Some((declId, _)) =>
                  (isGuardedLetAsVar(thenB), isGuardedLetAsVar(elseB)) match {
                    // We want to find if the collection of children from the node
                    // have the following structure: let x = if (cond) tVal else eVal
                    case (Some((tid, tVal)), Some((eid, eVal)))
                        if tid == declId && eid == declId =>

                      val combinedIf = IfExpr(ie.cond, tVal, eVal)
                      val tailExpr   = foldBlock(tail, Variable(declId), dropNonLet = false)
                      Let(declId, combinedIf, tailExpr)
                    case _ =>
                      foldBlock(stmts, initialAcc, dropNonLet = false)
                  }
                case None =>
                  foldBlock(stmts, initialAcc, dropNonLet = false)
              }
            case _ =>
              foldBlock(stmts, initialAcc, dropNonLet = false)
          }

          result

        // case "type_descriptor" | "abstract_pointer_declarator" | "abstract_function_declarator" =>
        //   val rawText = extractText(node, src)
        //   ctx.reporter.debug(s"[TS DEBUG] Reinterpreting fake pointer node '${node.getType}' as expression: '$rawText'")

        //   val children = (0 until node.getNamedChildCount)
        //     .flatMap(i => optToScala(node.getNamedChild(i)))
        //     .toList

        //   if (children.isEmpty) {
        //     RealLiteral(Rational.zero)
        //   } else {
        //     val exprChildren: List[DaisyExpr] = children.map(convertNode)
        //     val raw = extractText(node, src)
        //     val opCandidates = List("+", "-", "*", "/").filter(raw.contains)
        //     val op = opCandidates.headOption.getOrElse("*")

        //     ctx.reporter.debug(s"[TS DEBUG] Treating '${node.getType}' as ${exprChildren.size}-ary expr with op '$op'")

        //     exprChildren.reduceLeft { (acc: DaisyExpr, next: DaisyExpr) =>
        //       op match {
        //         case "+" => Plus(acc, next)
        //         case "-" => Minus(acc, next)
        //         case "*" => Times(acc, next)
        //         case "/" => Division(acc, next)
        //         case _   => Times(acc, next)
        //       }
        //     }
        //   }

        // case "function_definition" =>
        //   ctx.reporter.debug("[TS DEBUG] function_definition")

        //   val decl = node.getNamedChild(1).get() // function_declarator
        //   val body = node.getNamedChild(2).get() // compound_statement

        //   val declExpr = convertNode(decl)
        //   val bodyExpr = convertNode(body)

        //   Lambda(Seq.empty, bodyExpr) // or whatever Daisy expects


        // case "parameter_list" =>
        //   ctx.reporter.debug(s"[TS DEBUG] Unwrapping parameter_list: '${extractText(node, src)}'")

        //   val children = (0 until node.getNamedChildCount)
        //     .flatMap(i => optToScala(node.getNamedChild(i)))
        //     .toList

        //   if (children.isEmpty) {
        //     ctx.reporter.debug("[TS DEBUG] parameter_list empty; returning 0")
        //     RealLiteral(Rational.zero)
        //   } else if (children.size == 1) {
        //     ctx.reporter.debug("[TS DEBUG] parameter_list single child; recursing")
        //     convertNode(children.head)
        //   } else {
        //     ctx.reporter.debug("[TS DEBUG] parameter_list multiple children; treating as binary expression")
        //     val left = convertNode(children.head)
        //     val right = convertNode(children.last)

        //     val raw = extractText(node, src)
        //     val op =
        //       if (raw.contains("+")) "+"
        //       else if (raw.contains("-")) "-"
        //       else if (raw.contains("*")) "*"
        //       else if (raw.contains("/")) "/"
        //       else "*"

        //     ctx.reporter.debug(s"[TS DEBUG] parameter_list treated as op '$op'")
        //     op match {
        //       case "+" => Plus(left, right)
        //       case "-" => Minus(left, right)
        //       case "*" => Times(left, right)
        //       case "/" => Division(left, right)
        //       case _   => Times(left, right)
        //     }
        //   }
        
        // case "parameter_declaration" =>
        //   val idNodeOpt = findIdent(node)
        //   val idText = idNodeOpt.map(extractText(_, src)).getOrElse("unnamed")

        //   Variable(varId(idText))


        case _ =>
          super.convertNode(raw)
    }
  }

  protected final def runCTreesitter(ctx: Context, prg: DaisyProgram): (Context, DaisyProgram) = {
    runTreesitterPhase(ctx, "libtree-sitter-c.so", "tree_sitter_c")(
      findFunctions = root => allNamedChildren(root).filter(_.getType == "function_definition"),
      convertFunction = convertCFunction
    )
  }

  protected final def convertCFunction(ctx: Context, node: Node, src: String): DaisyFunDef = {
      convertFunctionCommon(ctx, node, src)(
      extractName = { n =>
        val decl = safeAnyChild(n, 1).getOrElse(n)
        textOr(src, safeAnyChild(decl, 0), "anon_fun")
      },
      extractParams = { n =>
        val decl = safeAnyChild(n, 1)
        extractParamsCommon(
          paramsNodeOpt = decl.flatMap(d => safeAnyChild(d, 1)),
          keepParamNodeIfNoIdentifier = false
        )
      }
      )
  }
}
