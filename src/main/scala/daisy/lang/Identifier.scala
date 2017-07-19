
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy
package lang

import Trees.{Delta, Epsilon, Tree, Variable}
import Types.{TypeTree, Typed, Untyped}

object Identifiers {

  /** Represents a unique symbol in Leon.
    *
    * The name is stored in the decoded (source code) form rather than encoded (JVM) form.
    * The type may be left blank (Untyped) for Identifiers that are not variables.
    */
  class Identifier private[Identifiers](
    val name: String,
    val globalId: Int,
    private[Identifiers] val id: Int,
    private val tpe: TypeTree,
    private val alwaysShowUniqueID: Boolean = false
  ) extends Tree with Typed with Ordered[Identifier] {

    self: Serializable =>

    val getType = tpe

    override def equals(other: Any): Boolean = other match {
      case null => false
      case i: Identifier => i.globalId == this.globalId
      case _ => false
    }

    override def hashCode: Int = globalId

    override def toString: String = {
      if (alwaysShowUniqueID) {
        name + (if (id > 0) id else "")
      } else {
        name
      }
    }

    def uniqueNameDelimited(delim: String) = name + delim + id

    def uniqueName: String = uniqueNameDelimited("")

    def toVariable: Variable = Variable(this)
    def toDeltaVariable: Delta = Delta(this)
    def toEpsilonVariable: Epsilon = Epsilon(this)
    // FIXME remove string comparison do something reasonable instead
    def isDeltaId: Boolean = this.toString.contains("delta")
    def isEpsilonId: Boolean = this.toString.contains("eps")

    def freshen: Identifier = FreshIdentifier(name, tpe, alwaysShowUniqueID).copiedFrom(this)

    override def compare(that: Identifier): Int = {
      val ord = implicitly[Ordering[(String, Int, Int)]]
      ord.compare(
        (this.name, this.id, this.globalId),
        (that.name, that.id, that.globalId)
      )
    }

    // keeps identifier intact
    def deepCopy: Identifier = this

    def changeType(newTpe: TypeTree): Identifier = {
      new Identifier(this.name, this.globalId, this.id, newTpe, this.alwaysShowUniqueID)
    }
  }

  class UniqueCounter[K] {

    private var globalId = -1
    private var nameIds = Map[K, Int]().withDefaultValue(-1)

    def next(key: K): Int = synchronized {
      nameIds += key -> (1+nameIds(key))
      nameIds(key)
    }

    def nextGlobal = synchronized {
      globalId += 1
      globalId
    }

  }

  object FreshIdentifier {

    private val uniqueCounter = new UniqueCounter[String]()

    // Replace $opcode inside a string with the symbolic operator name
    private def decode(s: String) =
      scala.reflect.NameTransformer.decode(s)

    /** Builds a fresh identifier
      *
      * @param name The name of the identifier
      * @param tpe The type of the identifier
      * @param alwaysShowUniqueID If the unique ID should always be shown
      */
    def apply(name: String, tpe: TypeTree = Untyped, alwaysShowUniqueID: Boolean = false) : Identifier =
      new Identifier(decode(name), uniqueCounter.nextGlobal, uniqueCounter.next(name), tpe, alwaysShowUniqueID)

    /** Builds a fresh identifier, whose ID is always shown
      *
      * @param name The name of the identifier
      * @param forceId The forced ID of the identifier
      * @param tpe The type of the identifier
      */
    def apply(name: String, forceId: Int, tpe: TypeTree): Identifier =
      new Identifier(decode(name), uniqueCounter.nextGlobal, forceId, tpe, alwaysShowUniqueID =  true)

  }

  def aliased(id1 : Identifier, id2 : Identifier) = {
    id1.toString == id2.toString
  }

  def aliased(ids1 : Set[Identifier], ids2 : Set[Identifier]) = {
    val s1 = ids1.groupBy{ _.toString }.keySet
    val s2 = ids2.groupBy{ _.toString }.keySet
    (s1 & s2).nonEmpty
  }

}