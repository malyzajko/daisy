// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package solvers

import lang.Trees.Expr
import lang.Identifiers._

/* For now, this is just a map with a fancy name. */
class Model(protected val mapping: Map[Identifier, Expr])
  // extends AbstractModel[Model]
  extends (Identifier => Expr) {

  // def newBuilder = new ModelBuilder
  def isDefinedAt(id: Identifier): Boolean = mapping.isDefinedAt(id)
  def get(id: Identifier): Option[Expr] = mapping.get(id)
  def getOrElse[E >: Expr](id: Identifier, e: E): E = get(id).getOrElse(e)
  def apply(id: Identifier): Expr = get(id).getOrElse { throw new IllegalArgumentException }

  override def toString: String = {
    if (mapping.isEmpty) {
      "Model()"
    } else {
      (for ((k,v) <- mapping.toSeq.sortBy(_._1)) yield {
        f"  ${k.toString}%-20s -> ${v.toString}"
      }).mkString("Model(\n", ",\n", ")")
    }
  }
}

object Model {
  def empty: Model = new Model(Map.empty)
}

/* trait AbstractModel[+This <: Model with AbstractModel[This]]
  extends scala.collection.IterableLike[(Identifier, Expr), This] {

  protected val mapping: Map[Identifier, Expr]

  def fill(allVars: Iterable[Identifier]): This = {
    val builder = newBuilder
    builder ++= mapping ++ (allVars.toSet -- mapping.keys).map(id => id -> simplestValue(id.getType))
    builder.result
  }

  def ++(mapping: Map[Identifier, Expr]): This = {
    val builder = newBuilder
    builder ++= this.mapping ++ mapping
    builder.result
  }

  def filter(allVars: Iterable[Identifier]): This = {
    val builder = newBuilder
    for (p <- mapping.filterKeys(allVars.toSet)) {
      builder += p
    }
    builder.result
  }

  def iterator = mapping.iterator
  def seq = mapping.seq


  override def toString = {
    if (mapping.isEmpty) {
      "Model()"
    } else {
      (for ((k,v) <- mapping.toSeq.sortBy(_._1)) yield {
        f"  ${k.toString}%-20s -> ${v.toString}"
      }).mkString("Model(\n", ",\n", ")")
    }
  }
} */

/* trait AbstractModelBuilder[+This <: Model with AbstractModel[This]]
  extends scala.collection.mutable.Builder[(Identifier, Expr), This] {

  import scala.collection.mutable.MapBuilder
  protected val mapBuilder = new MapBuilder[Identifier, Expr, Map[Identifier, Expr]](Map.empty)

  def +=(elem: (Identifier, Expr)): this.type = {
    mapBuilder += elem
    this
  }

  def clear(): Unit = mapBuilder.clear
} */



/* class ModelBuilder extends AbstractModelBuilder[Model] {
  def result = new Model(mapBuilder.result)
} */