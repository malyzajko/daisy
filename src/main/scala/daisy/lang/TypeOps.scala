
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy
package lang

import scala.collection.immutable.Seq

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

  def typesCompatible(t1: TypeTree, t2: TypeTree) = {
    leastUpperBound(t1, t2).isDefined
  }

}
