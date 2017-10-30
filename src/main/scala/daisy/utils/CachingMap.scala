package daisy.utils

import scala.collection.mutable

class CachingMap[K, V] extends mutable.HashMap[K,V] {
  def getOrAdd(k: K, v: K => V): V = {
    if (!contains(k)){
      put(k, v(k))
    }
    apply(k)
  }
}