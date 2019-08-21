package daisy.frontend

import daisy.lang.Identifiers.{FreshIdentifier, Identifier}
import daisy.lang.Trees.Tree
import daisy.tools.FinitePrecision.Float64
import daisy.tools.{Interval, Rational}
import daisy.lang.Types._

import scala.io.Source
import scala.util.parsing.combinator._

trait CReqExtraction {

  class ReqParser extends RegexParsers {
    /**
      * Function domain
      * @return
      */
    def dom: Parser[Interval] = "dom: [" ~ number ~ "," ~ number ~"]" ^^ {case _ ~ xlo ~ _ ~ xhi ~ _ => Interval(xlo, xhi)}

    /**
      * Number in either scientific or decimal notation
      * @return
      */
    def number: Parser[Rational] = ("""-?\d\.\d+[eE][\+|-]\d+""".r ||| """-?\d+(\.\d+)?""".r) ^^ {x => Rational.fromString(x)}

    /**
      * Number as power of 2
      * @return
      */
    def powOf2: Parser[Rational] = ("2^(" ~> number <~ ")" | number ~ """b\+?""".r ~ number) ^^ {
      case p: Rational => Rational.powerTwo(p.toInt)
      case fst ~ _ ~ scnd => (fst, scnd) match { case (x:Rational, y: Rational) => x * Rational.powerTwo(y.toInt) }
    }
    /**
      * Target error
      * @return
      */
    def err: Parser[Rational] = "err: " ~ (powOf2 ||| number) ^^ {case _ ~ e => e }

    /**
      * Function ID
      * @return
      */
    def id: Parser[String] = """[a-z|A-Z]+[[a-z|A-Z]*|_|\.|[0-9]*]*""".r

    /**
      * Parses input domain and target error from problemdef.sollya file
      * @return
      */
    def spec: Parser[Map[String, (Interval, Rational)]] = rep(id ~ "= {" ~ (dom | err) ~ (err | dom) <~ "}") ^^ {
      el => el.map({
        case fun ~ _ ~ x ~ y =>
        (x, y) match {
            case (i: Interval, e: Rational) => fun -> (i, e)
            case (e: Rational, i: Interval) => fun -> (i, e)
          }
      }).toMap[String, (Interval, Rational)]
    }

  }
}