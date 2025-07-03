package daisy.utils

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.HashMap

import daisy.lang.Identifiers._
import daisy.lang.Trees._
import daisy.utils.CodePrinter
import daisy.tools.Rational

/**
  * Prints an expression to a string in prefix notation. Also extracts identifers
  * used within the expression such that it can later be accurately rebuilt. 
  *
  * This is used when passing an expression to Omelette. 
  */
class PrefixPrinter(identifiers: HashMap[String, Identifier], buffer: Appendable = new StringBuffer) extends CodePrinter(buffer) {
  def ppOperation(args: Seq[Expr], name: String) = {
    sb.append("(" + name)

    args.foreach(ex => {
      sb.append(" ")
      pp(ex, None)(0)
    })
    sb.append(")")
  }
  
  override def pp(tree: Tree, parent: Option[Tree])(implicit lvl: Int): Unit = {
    tree match {
      case UMinus(e) => ppOperation(Seq(e), "-")
      case Sqrt(e)   => ppOperation(Seq(e), "sqrt")
      case Sin(e)    => ppOperation(Seq(e), "sin")
      case Cos(e)    => ppOperation(Seq(e), "cos")
      case Tan(e)    => ppOperation(Seq(e), "tan")
      case Exp(e)    => ppOperation(Seq(e), "exp")
      case Log(e)    => ppOperation(Seq(e), "log")
      
      case Plus(a, b)     => ppOperation(Seq(a, b), "+")
      case Minus(a, b)    => ppOperation(Seq(a, b), "-")
      case Times(a, b)    => ppOperation(Seq(a, b), "*")
      case Division(a, b) => ppOperation(Seq(a, b), "/")
      case IntPow(a, b)   => ppOperation(Seq(a, RealLiteral(Rational(b))), "pow")

      case RealLiteral(r) => sb.append(s"(${r.toFractionString})")
      case Variable(id) => {
        val string = id.toString
        identifiers.put(string, id)
        sb.append(string)
      }
    }
  }
}

/**
  * Parses an expression from a string in s-expression syntax. Emplaces
  * identifiers extracted by the printer in variables. 
  *
  * This is used when parsing an expression returned from Omelette. 
  */
class PrefixParser(identifiers: Option[HashMap[String, Identifier]]) extends RegexParsers {
  def uminus: Parser[UMinus.type] = "-"    ^^ { _ => UMinus }
  def sqrt:   Parser[Sqrt.type]   = "sqrt" ^^ { _ => Sqrt   }
  def sin:    Parser[Sin.type]    = "sin"  ^^ { _ => Sin    }
  def cos:    Parser[Cos.type]    = "cos"  ^^ { _ => Cos    }
  def tan:    Parser[Tan.type]    = "tan"  ^^ { _ => Tan    }
  def exp:    Parser[Exp.type]    = "exp"  ^^ { _ => Exp    }
  def log:    Parser[Log.type]    = "log"  ^^ { _ => Log    }

  def plus:     Parser[Plus.type]     = "+"   ^^ { _ => Plus     }
  def minus:    Parser[Minus.type]    = "-"   ^^ { _ => Minus    }
  def times:    Parser[Times.type]    = "*"   ^^ { _ => Times    }
  def division: Parser[Division.type] = "/"   ^^ { _ => Division }
  def pow:      Parser[IntPow.type]   = "pow" ^^ { _ => IntPow   }

  def int: Parser[BigInt] = "-?\\d+".r ^^ { BigInt(_) }
  def rational: Parser[RealLiteral] = int ~ ("/" ~ int).? ^^ {
      case n ~ None          => RealLiteral(Rational(n, 1))
      case n ~ Some("/" ~ d) => RealLiteral(Rational(n, d))
  }
  def variable: Parser[Variable] = "[a-zA-Z_][\\w]*".r ^^ {
    str => Variable(identifiers
        .map(ids => ids.get(str).get)
        .getOrElse(FreshIdentifier(str))
    )
  }
  
  def unary_op: Parser[Expr] = "(" ~ (uminus | sqrt | sin | cos | tan | exp | log) ~ expr ~ ")" ^^ {
    case "(" ~ op ~ a ~ ")" => op(a)
  }
  def binary_op: Parser[Expr] = "(" ~ (plus | minus | times | division) ~ expr ~ expr ~ ")" ^^ {
    case "(" ~ op ~ a ~ b ~ ")" => op(a, b)
  }
  def pow_op: Parser[IntPow] = "(" ~ pow ~ expr ~ int.filter(x => x.isValidInt && x > 0) ~ ")" ^^ {
    // special case is required for pow since the second operand must be a 32-bit integer
    case "(" ~ _ ~ a ~ b ~ ")" => IntPow(a, b.toInt)
  }
  
  def expr: Parser[Expr] = (unary_op | binary_op | pow_op | rational | variable) ^^ { _.asInstanceOf[Expr] }
}
