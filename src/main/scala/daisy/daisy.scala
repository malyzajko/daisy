// Copyright 2017 MPI-SWS, Saarbruecken, Germany

import scala.collection.immutable.Seq

package object daisy {

  type PathCond = Seq[lang.Trees.Expr]
  val emptyPath = Seq[lang.Trees.Expr]()

}
