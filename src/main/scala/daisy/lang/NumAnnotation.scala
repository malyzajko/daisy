
package daisy
package lang

import utils.{Interval, Rational}

/**
  This trait has a number of write-once fields which are to be populated
  by certain analyses.

  The idea is that this is easily extensible,
  not openly mutable for sanity's sake,
  and does not necessitate duplicating the AST trees.

  This is a trait and not an abstract class, because we have a lot of methods
  Expr => Expr, which cannot distinguish between 'NumExpr' and 'Expr'.
  Hence, the annotation has to be non-intrusive.
*/
trait NumAnnotation {

  private var _interval: Option[Interval] = None
  private var _absError: Option[Rational] = None

  // PERF: @inline?
  def interval_=(i: Interval): Unit = {
    if (_interval.isEmpty) {
      _interval = Some(i)
    } else {
      throw new WriteTwiceException("Trying to write to 'interval', " +
        "but it's already defined.")
    }
  }
  def interval: Interval = _interval.get
  def hasInterval: Boolean = !_interval.isEmpty

  def absError_=(e: Rational): Unit = {
    require(e >= Rational.zero)
    if (_absError.isEmpty) {
      _absError = Some(e)
    } else {
      throw new WriteTwiceException("Trying to write to 'absError', " +
        "but it's already defined.")
    }
  }
  def absError: Rational = _absError.get
  def hasError: Boolean = !_absError.isEmpty
}
