package daisy.frontend

import daisy.DaisyFatalError
import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.Trees._
import daisy.tools.FinitePrecision.Float64
import daisy.tools.Rational
import daisy.lang.Types._

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator._

trait CCodeExtraction {
  case class ExConsts(id: Identifier, value: ExNumber) extends Tree {
    override def toString: String = "let " + id.toString + "=" + value.n.toLongString
    def getValue: Rational = value.n
  }
  case class ExType(p: TypeTree) extends Tree {}

  case class ExFncHead(fncName: Identifier, params: Seq[Tree]) extends Tree {
    override def toString: String = fncName + "(" + params.mkString(",") + ")"
  }
  case class ExFncDef(head: ExFncHead, body: Seq[Tree]) extends Tree {
    override def toString: String = head.toString
    def getFncArgs: Seq[Tree] = head.params
  }

  case class ExArithmeticOperator(op: String) extends Tree {}
  case class ExAssign(lhs:Tree, rhs: Tree) extends Tree {
    override def toString: String = lhs.toString + "=" + rhs.toString
  }
  case class ExBinaryOperation(lhs: Tree, op: ExArithmeticOperator, rhs: Tree) extends Tree {
    override def toString: String = lhs.toString + op.op + rhs.toString
  }
  case class ExFncCall(fnc: Identifier, args: Seq[Tree]) extends Tree {} // TODO: better type for arguments? need to take into account both identifiers and pointers

  case class CondOp(op: String) extends Tree
  case class ExCond(lhs: Tree, op: CondOp, rhs: Tree) extends Tree {}
  case class ExIfElse(cond: ExCond, thenn: Seq[Tree], elze: Seq[Tree]) extends Tree {}
  // address = true stands for &x, false stands for *x
  case class ExPointer(idName: Identifier, address: Boolean = true) extends Tree {}
  case class ExCast(t: ExType, expr: Tree) extends Tree {}
  case class ExVarDecl(t: ExType, id: Identifier) extends Tree {}
  case class ExRecord(id: Identifier, field: Identifier) extends Tree {}
  case class ExNumber(n: Rational) extends Tree {}
  val one = ExNumber(Rational.one)

  class ExpressionParser extends RegexParsers {

    var ids: ListBuffer[Identifier] = new ListBuffer[Identifier]()

    /**
      * Variable and function ID
      * @return
      */
    def id: Parser[Identifier] = """[a-z|A-Z]+[[a-z|A-Z]*|_|[0-9]*]*h?""".r ^^ {
      x => ids.find(x == _.name) match {
        case None => val newId = FreshIdentifier(x, RealType)
          ids += newId
          newId
        case Some(z) => z
      }
    } // maintain the list of already given IDs and reuse; TODO: what if fnc and var have the same name?

    /**
      * Pointer &x or *x
      * @return
      */
    def ptr: Parser[ExPointer] = """(&|\*)[a-z|A-Z]+[[a-z|A-Z]*|_|[0-9]*]*h?""".r ^^ { x =>
      val id = x.substring(1)
      ids.find(id == _.name) match {
        case None if x.startsWith("&") =>
          val newId = FreshIdentifier(id)
          ids += newId
          ExPointer(newId)
        case None =>
          val newId = FreshIdentifier(id)
          ids += newId
          ExPointer(newId, false)

        case Some(z) if x.startsWith("&") =>
          ExPointer(z)
        case Some(z) =>
          ExPointer(z, false)
      }
    }

    // extend set of available types here
    def exType: Parser[ExType] = """double|int|void|db_number""".r ^^ {
      case "double" => ExType(FinitePrecisionType(Float64))
      case "int" => ExType(IntegerType)
      case "void" => ExType(UnitType)
      case "db_number" => ExType(IntegerType)
    }
    /**
      * Number in either scientific or decimal notation
      * @return
      */
    def number: Parser[ExNumber] = ("""-?\d\.\d+[eE][\+|-]\d+""".r ||| """-?\d+(\.\d+)?""".r) ^^ {x => ExNumber(Rational.fromString(x))}

    /**
      * Record access
      * @return
      */
    def record: Parser[ExRecord] = id ~ "." ~ id ^^ {case rec ~ _ ~ fld => ExRecord(rec, fld)}

    def atom: Parser[Tree] = record ||| id | ptr

    /**
      * Global constants, defined using #define directive
      * @return
      */
    def letStmt: Parser[ExConsts] =
      "#define " ~ id ~  number ^^ {case _ ~ varName ~ value => ExConsts(varName, value)}

    /**
      * Multiline comment
      * @return
      */
    // TODO: handle when comment contains #
    def comment: Parser[Unit] = """/\*[^#]*\*/""".r ^^ {_ => ;}

    // TODO: put actual types
    /**
      * Function declaration
      * @return
      */
    def fncHead: Parser[ExFncHead] = "void" ~ id ~ "(double" ~ (id | ptr) ~ ", double" ~ (id | ptr) ~ ")" ^^ {
      case _ ~ fncName ~ _ ~ param1 ~ _ ~ param2 ~ _ => ExFncHead(fncName, Seq(param1, param2))
    }

    /**
      * Function body. Contains (optionally) local variable declarations,
      * at least one expression, (optionally) function call.
      * @return
      */
    def fncBody: Parser[Seq[Tree]] = rep1(rep(varDecl) ~ rep1(stmt)) ^^ {
      x => x.flatMap(el => el match { case vars ~ exprs => Seq(vars, exprs).flatten })
    }

    /**
      * Local variable declaration
      * @return
      */
    def varDecl: Parser[Tree] = exType ~ id ~ ";" ^^ {case ttype ~ varName ~ _ => ExVarDecl(ttype, varName) }

    /**
      * Binary arithmetic operator
      * @return
      */
    def op: Parser[ExArithmeticOperator] = """\+|\*|-|&|<<|>>""".r ^^ ExArithmeticOperator // TODO: fail on bit shift

    /**
      * Binary arithmetic OPERATION
      * @return
      */
    def binaryOp: Parser[Tree] = (atom | number) ~ op ~ (atom | number) ^^ { // TODO: perform operations with pointers too? (or remove them from rhs)
      case xid ~ binOp ~ yid => ExBinaryOperation(xid, binOp, yid)
    }

    def shortBinOp: Parser[Tree] = atom ~ op ~ "=" ~ (number | atom) ~ ";" ^^ {case lhs ~ binOp ~ _ ~ rhs ~ _ => ExAssign(lhs, ExBinaryOperation(lhs, binOp, rhs))}
     /**
      * Assignment statement - assign to a variable or pointer
      * @return
      */
    def assign: Parser[Tree] =  atom ~ "=" ~ (binaryOp ||| fncCall ||| atom | number | cast) ~ ";".? ^^ { case lhs ~ _ ~ rhs ~ _ => ExAssign(lhs, rhs) }

    /**
      * Comparison operator
      * @return
      */
    def condOp: Parser[CondOp] = """<|>|<=|>=|==""".r ^^ CondOp

    /**
      * If-Then-Else statement
      * @return
      */
    def ifElseStmt: Parser[ExIfElse] = "if (" ~ (id | number) ~ (condOp | op) ~ (id | number) ~ ") {" ~ rep1(stmt) ~ "} else {" ~ rep1(stmt) ~ "}" ^^ {
      case _ ~ lhs ~ oper ~ rhs ~ _ ~ thenn ~ _ ~ elze ~ _ =>
        oper match {
          case cOp: CondOp => ExIfElse(ExCond(lhs, cOp, rhs), thenn, elze)
          case op: ExArithmeticOperator => ExIfElse(ExCond(ExBinaryOperation(lhs, op, rhs), CondOp("=="), one), thenn, elze)
        }
    }

    def cast: Parser[Tree] = "(" ~ exType ~ ")" ~ (atom | "(" ~ binaryOp ~ ")") ^^ {
      case _ ~ ttype ~ _ ~ toCast => toCast match {
        case x: Identifier => ExCast(ttype, x)
        case x: ExPointer => ExCast(ttype, x)
        case x: ExRecord => ExCast(ttype, x)
        case _ ~ x ~ _ => ExCast(ttype, x.asInstanceOf[ExBinaryOperation]) // TODO: better fix?
      }
    }

    /**
      * Program statement. Can be one of these:
      * - if-then-else statement
      * - assignment to a variable or a pointer
      * - binary arithmetic operation
      * - function call
      * @return
      */
    def stmt: Parser[Tree] = ifElseStmt | (assign ||| shortBinOp) | fncCall

    /**
      * Function call
      * @return
      */
    def fncCall: Parser[Tree] = id ~ "(" ~ rep((id | ptr) ~ ",") ~ id ~ ");" ^^ {
      case fncName ~ _ ~ arg1 ~ arg2 ~ _ => {
        val args = arg1.map({case idV ~ _ => idV}) :+ arg2
        val tmp = args.filter(x => {
          x match {
            case ptr: ExPointer => ptr.address
            case _ => false
          }})
        if (tmp.isEmpty)
          ExFncCall(fncName, args)
        else {
          val toAssign = tmp.head.asInstanceOf[ExPointer].idName // TODO: handle multiple assignments too?
          ExAssign(toAssign, ExFncCall(fncName, args))
        }
      }
    }

    /**
      * Function including its declaration and body
      * @return
      */
    def fnc: Parser[ExFncDef] = fncHead ~ "{" ~ fncBody ~ "}" ^^ { case head ~ _ ~ body ~ _ => ExFncDef(head, body) }

    /**
      * Program
      * @return
      */
    def prg: Parser[Seq[Tree]] = phrase(rep1(rep(comment) ~> rep(letStmt) ~ fnc)) ^^ {
      x => x.flatMap( el => el match { case lets ~ fnc => lets :+ fnc })
    }

    // TODO: figure out what to do with MulAdd1121 fnc call (fnc def is in Metalibm file extension.c)

  }

  class Extraction(units: Seq[Tree]) {

    val mainFnc: ExFncDef = units.filter(_.isInstanceOf[ExFncDef]).last.asInstanceOf[ExFncDef]

    private val consts = units.filter(_.isInstanceOf[ExConsts]).asInstanceOf[Seq[ExConsts]]
    private val funs = units.filter(_.isInstanceOf[ExFncDef]).asInstanceOf[Seq[ExFncDef]]

    def extractInputVar: Variable = {
      val fncs = units.filter(_.isInstanceOf[ExFncDef])
      val mainFnc = fncs.last.asInstanceOf[ExFncDef]
      val nonPointerArgs = mainFnc.getFncArgs.filter(_.isInstanceOf[Identifier]).asInstanceOf[Seq[Identifier]]
      Variable(nonPointerArgs.last)
    }

    def extractTree: Expr = {

      def addConstants(consts: Seq[ExConsts], fnc: Expr): Expr = consts match {
        case Nil => fnc
        case x: ExConsts => Let(x.id, RealLiteral(x.getValue, x.getValue.toString), fnc)
        case x::xs => Let(x.id, RealLiteral(x.getValue, x.getValue.toString), addConstants(xs, fnc))
      }

      val expr = extract(funs.reverse) // reverse so that main fnc is taken first
      val letTree = addConstants(consts, expr)
      letTree
    }

    private def extractNested(fnc: Tree): Expr = fnc match {
      case x: Identifier => Variable(x) // TODO: add a check to not call it for function id
      case x @ ExNumber(n) => RealLiteral(n, n.toString)
      case x @ ExPointer(id, address) => Variable(id)// TODO: differentiate between * and & pointers
      case x @ ExRecord(id, fld) => Variable(FreshIdentifier(id.name + "." + fld.name)) // TODO figure out how to handle it

      case x @ ExCond(lhs, op, rhs) =>
        val lhsExpr = extractNested(lhs)
        val rhsExpr = extractNested(rhs)
        op.op match {
          case "==" => Equals(lhsExpr, rhsExpr)
          case ">"  => GreaterThan(lhsExpr, rhsExpr)
          case ">=" => GreaterEquals(lhsExpr, rhsExpr)
          case "<"  => LessThan(lhsExpr, rhsExpr)
          case "<=" => LessEquals(lhsExpr, rhsExpr)
        }

      case x @ ExCast(ttype, expr) => ??? // TODO: handle casts
      case x @ ExBinaryOperation(lhs, op, rhs) =>
        val lhsExpr = extractNested(lhs)
        val rhsExpr = extractNested(rhs)
        op.op match {
          case "+" => Plus(lhsExpr, rhsExpr)
          case "*" => Times(lhsExpr, rhsExpr)
          case "-" => Minus(lhsExpr, rhsExpr)
          case o => throw new DaisyFatalError(Some(s"Source program contains operators Daisy cannot handle: $o")) // >>, << , &
        }

      case e => throw new DaisyFatalError(Some(s"Unexpected parsing token $e"))
    }

    private def extract(trees: Seq[Tree]): Expr = trees match {
      case  Nil => UnitLiteral.apply() // fixme is this ever called?
      case fnc :: rest => fnc match {
        case x @ ExVarDecl(ttype, id) => extract(rest) // ignore var declarations
        case x @ ExFncCall(id, args) =>
          val toInline = funs.find(_.head.fncName == id).getOrElse(throw new DaisyFatalError(Some("Calling undefined function")))
          // replace vars inside inlined fnc
          val oldVars = toInline.head.params
          val body = extract(toInline.body)
          replaceVars(body, oldVars, args)

        case x @ ExAssign(lhs, rhs) =>
          val rhsExpr = rhs match {
            case r @ ExFncCall(_, _) => extract(Seq(r))
            case r => extractNested(r)
          }
          lhs match {
            case z @ ExPointer(id, addr) => rhsExpr
            case z: Identifier =>
              val outlook = extract(rest)
              Let(z, rhsExpr, outlook)
          }

        case x @ ExFncDef(head, body) => extract(body) // TODO: do something with head; this obj should be only on top level

        case x @ ExIfElse(cond, thenn, elze) =>
          val condExpr = extractNested(cond)
          val thenExpr = extract(thenn)
          val elseExpr = extract(elze)

          IfExpr(condExpr, thenExpr, elseExpr)

        case e => throw new DaisyFatalError(Some(s"Unexpected parsing token $e"))
      }
    }
    private def replaceVars(expr: Expr, old: Seq[Tree], neww: Seq[Tree]): Expr = expr match {
      case x @ RealLiteral(_) => x
      case x @ Variable(id) =>
        val ind = old.indexOf(id)
        if (ind >= 0) {
          Variable(neww.apply(ind).asInstanceOf[Identifier]) // replace
        }
        else
          x
      case x @ Let(id, value, body) =>
        val ind = old.indexOf(id)
        if (ind >= 0)
          Let(neww.apply(ind).asInstanceOf[Identifier], replaceVars(value, old, neww), replaceVars(body, old, neww)) // replace
        else
          Let(id, replaceVars(value, old, neww), replaceVars(body, old, neww))

      case x @ Plus(lhs, rhs) => Plus(replaceVars(lhs, old, neww), replaceVars(rhs, old, neww))
      case x @ Minus(lhs, rhs) => Minus(replaceVars(lhs, old, neww), replaceVars(rhs, old, neww))
      case x @ Times(lhs, rhs) => Times(replaceVars(lhs, old, neww), replaceVars(rhs, old, neww))
      case x @ Equals(l, r) => Equals(replaceVars(l, old, neww), replaceVars(r, old, neww))
      case x @ LessThan(l, r) => LessThan(replaceVars(l, old, neww), replaceVars(r, old, neww))
      case x @ LessEquals(l, r) => LessEquals(replaceVars(l, old, neww), replaceVars(r, old, neww))
      case x @ GreaterThan(l, r) => GreaterThan(replaceVars(l, old, neww), replaceVars(r, old, neww))
      case x @ GreaterEquals(l, r) => GreaterEquals(replaceVars(l, old, neww), replaceVars(r, old, neww))
      case x @ IfExpr(cond, thenn, elze) =>
        IfExpr(
          replaceVars(cond, old, neww),
          replaceVars(thenn, old, neww),
          replaceVars(elze, old, neww)
        )
    }


    /**
      * Returns a list of function arguments excluding the pointers
      * @return List of [[ValDef]] for non pointer arguments
      */
    def extractFncVarArgs: Seq[ValDef] = {
      val args = mainFnc.getFncArgs
      val nonPointerArgs = args.filter(!_.isInstanceOf[ExPointer]).asInstanceOf[Seq[Identifier]]
      nonPointerArgs.map(ValDef)
    }
    /**
      * Returns a list of function pointer arguments
      * @return List of [[ValDef]] for pointer arguments
      */
    def extractFncPtrArgs: Seq[ValDef] = {
      val args = mainFnc.getFncArgs
      val nonPointerArgs = args.filter(_.isInstanceOf[ExPointer]).asInstanceOf[Seq[ExPointer]]
      nonPointerArgs.map(x => ValDef(x.idName))
    }
    /**
      * Returns a list of all function arguments
      * @return List of [[ValDef]] for both pointer and variable arguments
      */
    def extractFncArgs: Seq[ValDef] = {
      val args = mainFnc.getFncArgs
      args.map {
        case z: Identifier => ValDef(z)
        case z: ExPointer => ValDef(z.idName)
      }
    }

    def extractMainFncId: Identifier = mainFnc.head.fncName
  }
}