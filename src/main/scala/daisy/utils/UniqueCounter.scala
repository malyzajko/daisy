
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy
package utils

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
