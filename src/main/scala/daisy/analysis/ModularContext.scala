package daisy.analysis

import daisy.lang.Identifiers.Identifier
import daisy.lang.Trees.{Epsilon, Expr, FunctionInvocation, ValDef}
import daisy.tools.{Interval, MPFRInterval}

case class ModularContext(

  roundOffFirstOrderWrtEpsSum: List[Expr] = List(),
  roundOffRemainder: MPFRInterval = MPFRInterval.zero,
  roundOffFirstOrderWrtFuncEps: List[(Epsilon, Expr)] = List(),
  roundOffFunEpsMap: Map[Epsilon, FunctionInvocation] = Map(),
  roundOffRemaindersWrtEpsContainingFuncEps: List[Expr] = List(),
  roundOffRemainderWrtFunEps: List[(Expr, Expr, Expr)] = List(),
  roundOffOptReport: String = "noOpt",
  transcendentalEpsilons: Seq[Identifier] = Seq.empty,

  propFirstOder: List[(Identifier, Expr)] = List(),
  propRemainder: List[(Expr, Expr, Expr)] = List(),
  propFirstOrderWrtFun: List[(FunctionInvocation, Expr)] = List(),
  propOptReport: String = "noOpt",

  firstOderWrtInitAbsErrs: List[(Identifier, Expr)] = List(),
  absErrVarsErrBounds: Map[Identifier, Interval] = Map())

