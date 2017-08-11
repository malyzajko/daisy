// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

class UniqueCounter[K] {

  private var globalId = -1
  private var nameIds = Map[K, Int]().withDefaultValue(-1)

  def next(key: K): Int = synchronized {
    nameIds += key -> (1 + nameIds(key))
    nameIds(key)
  }

  def nextGlobal: Int = synchronized {
    globalId += 1
    globalId
  }

}
