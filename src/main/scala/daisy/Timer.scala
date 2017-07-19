
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy

import scala.language.dynamics


class Timer {
  var beginning: Long = 0L
  var end: Long = 0L

  def start: this.type = {
    beginning = System.currentTimeMillis
    end       = 0L
    this
  }

  def stop = {
    end         = System.currentTimeMillis
  }

  override def toString = {
    "%6d ms".format((end - beginning))
  }

}


// The fancy way of doing this
class TimerStorage extends Dynamic {
  var keys    = List[String]()
  var fields  = Map[String, Timer]()

  def get(name: String): Timer = {
    fields.get(name) match {
      case Some(t) =>
        t

      case None =>
        val t = new Timer()
        fields += name -> t
        keys ::= name
        t
    }
  }

  def selectDynamic(name: String): Timer = get(name)

  override def toString: String = {
    val sb = new StringBuffer

    keys.map(key => s"$key: ${fields(key).toString}").mkString(", ")
  }
}
