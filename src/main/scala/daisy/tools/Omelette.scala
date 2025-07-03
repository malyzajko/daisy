package daisy.tools

import daisy.utils.PrefixPrinter
import daisy.utils.PrefixParser
import daisy.lang.Trees.Expr
import daisy.lang.Identifiers.Identifier
import scala.sys.process._
import scala.collection.mutable.HashMap

/**
  * Interface to the Omelette term rewriting and interval evaluation engine based on e-graphs. This interface
  * allows for processing several overlapping expressions at once, which exploits common sub-expressions for
  * performance. 
  *
  * @param iter_limit Maximmum number of iterations to run equality saturation for. 
  * @param cost Cost function used when extracting an expression. 
  */
class Omelette(val iter_limit: Long, val cost: String) {
  // Buffer for identifiers so we don't lose information during I/O with Omelette. 
  var identifiers: HashMap[String, Identifier] = new HashMap
  
  /**
    * Evaluates the interval of a sequence of overlapping expressions. 
    *
    * @param expr Sequence of overlapping expressions to process. 
    * @param intervals Union of the set of variables referenced in the expressions. 
    * @return Intervals of all expressions in input order. 
    */
  def evalRange(expr: Iterator[Expr], intervals: Map[Identifier, Interval]): Array[Interval] = {
    invoke(expr, intervals, this.iter_limit, None)
      .map({ case Array(i) => this.parse_interval(i) })
      .toArray
  }

  /**
    * Rewrites a sequence of overlapping expressions using the cost function given by `this.cost`. 
    *
    * @param expr Sequence of overlapping expressions to process. 
    * @param intervals Union of the set of variables referenced in the expressions. 
    * @return Rewritten expressions in input order. 
    */
  def evalExpr(expr: Iterator[Expr], intervals: Map[Identifier, Interval]): Array[Expr] = {
    invoke(expr, intervals, this.iter_limit, None)
      .map({ case Array(e, _) => this.parse_expr(e) })
      .toArray
  }

  /**
    * Rewrites a sequence of overlapping expressions using the cost function given by `this.cost` and
    * computes their intervals. 
    *
    * @param expr Sequence of overlapping expressions to process. 
    * @param intervals Union of the set of variables referenced in the expressions. 
    * @return Rewritten expressions and corresponding intervals in input order. 
    */
  def eval(expr: Iterator[Expr], intervals: Map[Identifier, Interval]): Array[(Expr, Interval)] = {
    invoke(expr, intervals, this.iter_limit, Some(this.cost))
      .map({ case Array(e, i) => (
        this.parse_expr(e), 
        this.parse_interval(i), 
      )})
      .toArray
  }

  private def parse_expr(string: String): Expr = {
    val parser = new PrefixParser(Some(this.identifiers))
    val error = (msg: String) => throw new IllegalArgumentException(
        s"Failed to parse '${string}' from Omelette: ${msg}"
      )
    parser.parse(parser.expr, string) match {
      case parser.Success(result, _) => result
      case parser.Failure(msg, _) => error(msg)
      case parser.Error(msg, _) => error(msg)
    }
  }

  private def parse_interval(string: String): Interval = {
    val Array(min, max) = string
      .stripPrefix("[")
      .stripSuffix("]")
      .split(",")
      .map(b => b.split("/") match {
        case Array(n, d) => Rational(BigInt(n), BigInt(d))
        case Array(i) => Rational(BigInt(i), 1)
      })
    Interval(min, max)
  }

  private def invoke(
    expr: Iterator[Expr], 
    intervals: Map[Identifier, Interval], 
    iter_limit: Long, 
    cost: Option[String], 
  ): Iterator[Array[String]] = {
    // serialise expressions
    val expressions_str = expr
      .map(e => {
        val printer = new PrefixPrinter(this.identifiers)
        printer.pp(e, None)(0)
        s""""${printer.toString()}""""
      })
      .mkString(" ")

    // serialise intervals
    val intervals_string = intervals
      .map({ case (id, Interval(lo, hi)) => 
        s"$id:[${lo.toFractionString},${hi.toFractionString}]"
      })
      .mkString(" ")

    // run omelette
    val cost_arg = cost.map(c => s"--cost=$c").getOrElse("")
    val iter_limit_arg = s"--iter-limit=$iter_limit"
    val output = s"""./lib/omelette
      $cost_arg
      $iter_limit_arg
      $expressions_str : $intervals_string
    """.!!

    // perform simple post-processing on output; parsing is performed lazily
    output
      .linesIterator
      .map(line => line.trim().split(" : "))
  }
}
