package daisy.utils

import scala.collection.mutable

object CachingMap {
  def empty[K,V]() = new CachingMap[K,V]()
  def fromMap[K,V](m: mutable.HashMap[K,V]): CachingMap[K,V] = {
    val res = new CachingMap[K,V]()
    m.foreach({case (k, v) => res.put(k,v)})
    res
  }
}

// we have a special class here because of the getOrAdd function
class CachingMap[K, V] {
  private val underlying = mutable.HashMap[K, V]()

  def getOrAdd(k: K, v: K => V): V =
    underlying.getOrElseUpdate(k, v(k))

  def put(k: K, v: V): Option[V] =
    underlying.put(k, v)

  def get(k: K): Option[V] =
    underlying.get(k)

  def apply(k: K): V =
    underlying(k)

  def update(k: K, v: V): Unit =
    underlying.update(k, v)

  def remove(k: K): Option[V] =
    underlying.remove(k)

  def isEmpty(): Boolean =
    underlying.isEmpty

  def nonEmpty(): Boolean =
    underlying.nonEmpty

  def getOrElse(k: K, default: => V): V =
    underlying.getOrElse(k, default)

  def mapValues[W](f: V => W): Map[K, W] =
    underlying.view.mapValues(f).toMap

  def toMap: Map[K, V] =
    underlying.toMap
}