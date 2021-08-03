// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package lang

import Types._

object TypeOps {

  def leastUpperBound(t1: TypeTree, t2: TypeTree): Option[TypeTree] = (t1,t2) match {
    case (FunctionType(from1, to1), FunctionType(from2, to2)) =>
      if (from1 == from2) {
        leastUpperBound(to1, to2) map { FunctionType(from1, _) }
      } else {
        None
      }

    case (o1, o2) if o1 == o2 => Some(o1)
    case _ => None
  }

  def isSubtypeOf(t1: TypeTree, t2: TypeTree): Boolean = {
    leastUpperBound(t1, t2) == Some(t2)
  }

  def typesCompatible(t1: TypeTree, t2: TypeTree): Boolean = {
    leastUpperBound(t1, t2).isDefined
  }

}
