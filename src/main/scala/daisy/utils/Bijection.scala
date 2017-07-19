
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy
package utils

// This is a fancy name for a two-way map.
class Bijection[A, B] {
  protected var a2b = Map[A, B]()
  protected var b2a = Map[B, A]()

  def +=(a: A, b: B): Unit = {
    a2b += a -> b
    b2a += b -> a
  }

  def +=(t: (A,B)): Unit = {
    this += (t._1, t._2)
  }

  def clear(): Unit = {
    a2b = Map()
    b2a = Map()
  }

  def getA(b: B): Option[A] = b2a.get(b)
  def getB(a: A): Option[B] = a2b.get(a)

  def toA(b: B): A = getA(b).get
  def toB(a: A): B = getB(a).get

  def aToB: Map[A,B] = {
    a2b
  }

  def bToA: Map[B, A] = {
    b2a
  }

  //def fromA(a: A): B = getB(a).get
  //def fromB(b: B): A = getA(b).get

  /* Check if a is already present,
    if not, add a new mapping.
    */
  def getOrElseAddB(a: A)(c: => B) = {
    getB(a).getOrElse {
      val res = c
      this += a -> res
      res
    }
  }

  /* Check if b is already present,
    if not, add a new mapping.
    */
  def getOrElseAddA(b: B)(c: => A) = {
    getA(b).getOrElse {
      val res = c
      this += res -> b
      res
    }
  }

  def containsA(a: A) = a2b contains a
  def containsB(b: B) = b2a contains b

  def aSet = a2b.keySet
  def bSet = b2a.keySet
}
