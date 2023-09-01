package daisy.utils

import scala.collection.mutable

object CachingMap {
  def empty[K,V]() = new CachingMap[K,V]()
  def fromMap[K,V](m: mutable.HashMap[K,V]): CachingMap[K,V] = {
    val res = new CachingMap[K,V]()
    m.foreach({case (k:K, v:V) => res.put(k,v)})
    res
  }
}

class CachingMap[K, V] extends mutable.HashMap[K,V] {
  def getOrAdd(k: K, v: K => V): V = {
    if (!contains(k)){
      put(k, v(k))
    }
    apply(k)
  }
}