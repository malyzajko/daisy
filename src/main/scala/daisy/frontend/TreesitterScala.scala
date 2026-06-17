package daisy
package frontend

import io.github.treesitter.jtreesitter.Node
import daisy.lang.Identifiers._
import daisy.lang.Trees.{Expr => DaisyExpr, FunDef => DaisyFunDef, Program => DaisyProgram, ValDef => DaisyValDef, _}
import daisy.lang.Constructors._
import daisy.tools.Rational

trait TreesitterScala extends TreesitterCommon {
  protected final def convertScalaNode(raw: Node): DaisyExpr =
    {
      val node = unwrapExpr(raw)
      node.getType match {
        case "infix_expression" =>
          val leftNodeOpt  = safeNamedChild(node, 0)
          val opNodeOpt    = safeNamedChild(node, 1)
          val rightNodeOpt = safeNamedChild(node, 2)
          convertBinaryByParts(
            node = node,
            leftNodeOpt = leftNodeOpt,
            opTokenOpt = opNodeOpt.map(n => extractText(n, currentSrc)),
            rightNodeOpt = rightNodeOpt,
            contextLabel = "infix_expression",
            allowRawArithmeticFallback = false
          )

        case "block" =>
          val children = allNamedChildren(node).toList

          val (stmtNodes, lastExprNodeOpt) =
            if (children.nonEmpty)
              (children.init, Some(children.last))
            else
              (Nil, None)

          val stmts = stmtNodes.map(convertNode).filter(_ != null)

          val finalExpr =
            lastExprNodeOpt.map(convertNode).getOrElse(RealLiteral(Rational.zero))

          foldBlock(stmts, finalExpr, dropNonLet = true)

        case "val_definition" =>
          val idNode   = safeNamedChild(node, 0).orElse(findIdent(node))
          val initNode = safeNamedChild(node, 2).orElse(safeNamedChild(node, 1))

          idNode.map { idn =>
            val idName = extractText(idn, currentSrc)
            val id = varId(idName)

            val initExpr =
              initNode.map(convertNode).getOrElse(RealLiteral(Rational.zero))

            Let(id, initExpr, Variable(id))
          }.getOrElse {
            RealLiteral(Rational.zero)
          }



        case _ =>
          super.convertNode(raw)
        }
    }

  protected final def runScalaTreesitter(ctx: Context, prg: DaisyProgram): (Context, DaisyProgram) = {
    def collectScalaNodes(root: Node): Seq[Node] = {
      if (root == null) Seq.empty
      else {
        val here = if (root.getType == "function_definition") Seq(root) else Seq.empty
        here ++ allNamedChildren(root).flatMap(ch => collectScalaNodes(ch))
      }
    }
    runTreesitterPhase(ctx, "libtree-sitter-scala.so", "tree_sitter_scala")(
      findFunctions = root => collectScalaNodes(root),
      convertFunction = convertScalaFunction
    )
  }

  protected final def convertScalaFunction(ctx: Context, node: Node, src: String): DaisyFunDef ={
    convertFunctionCommon(ctx, node, src)(
      extractName = { n =>
        textOr(src, safeNamedChild(n, 0), throw new Exception("Cannot find function name"))
      },
      extractParams = { n =>
        extractParamsCommon(
          paramsNodeOpt = safeNamedChild(n, 1),
          keepParamNodeIfNoIdentifier = true
        )
      }
    )
  }
}
