// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package lang

import scala.collection.immutable.Seq
import tools.FinitePrecision.Precision

import Trees._

object Types {

  trait Typed {
    val getType: TypeTree
    def isTyped: Boolean = getType != Untyped
  }

  class TypeErrorException(msg: String) extends Exception(msg)

  object TypeErrorException {
    def apply(obj: Expr, exp: List[TypeTree]): TypeErrorException = {
      new TypeErrorException("Type error: " + obj + ", expected: " +
        exp.mkString(" or ") + ", found " + obj.getType)
    }

    def apply(obj: Expr, exp: TypeTree): TypeErrorException = {
      apply(obj, List(exp))
    }
  }

  abstract class TypeTree extends Tree with Typed {
    val getType = this

    // Checks whether the subtypes of this type contain Untyped,
    // and if so sets this to Untyped.
    // Assumes the subtypes are correctly formed, so it does not descend
    // deep into the TypeTree.
    def unveilUntyped: TypeTree = this match {
      case NAryType(tps, _) =>
        if (tps contains Untyped) Untyped else this
    }
    def deepCopy: TypeTree = this
  }

  case object Untyped extends TypeTree
  case object BooleanType extends TypeTree
  case object UnitType extends TypeTree
  case object IntegerType extends TypeTree
  case object Int16Type extends TypeTree
  case object Int32Type extends TypeTree
  case object Int64Type extends TypeTree
  case object RealType extends TypeTree
  case class FinitePrecisionType(prec: Precision) extends TypeTree

  // arbitrary-precision fixed-point types for Vivado HLS
  case class APFixedType(totalBit: Int, intBits: Int) extends TypeTree

  case class FunctionType(from: Seq[TypeTree], to: TypeTree) extends TypeTree

  case class TupleType(args: Seq[TypeTree]) extends TypeTree

  object NAryType {
    def unapply(t: TypeTree): Option[(Seq[TypeTree], Seq[TypeTree] => TypeTree)] = t match {
      case t => Some(Nil, _ => t)
    }
  }
}