// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package utils

import java.io._
import scala.io.Source

/** Class to abstract out the common requirements of file logging.
 *
 * Handles basic logging requirements like 'write', 'append', 'write-if-empty'.
 * Abstracts out the folder creation, file handler functions etc.
 *
 * @constructor Create a FileLogger object which handles all file handling for logs
 * @param filePath The path of the file to be used for logging.
 * @param reporter Implicit parameter to be used for logging errors.
 */
class FileLogger(val filePath: String)(implicit var reporter: Reporter) {

  val folderPath = new File(filePath.split('/').reverse.tail.reverse.mkString("/"))
  if(!folderPath.exists()) {
    folderPath.mkdirs()
  }

  private val file = new File(filePath)
  private var writeFD: Option[BufferedWriter] = None
  private var appendFD: Option[BufferedWriter] = None

  /** Function to write a particular string to the log file
   *
   * Handles the file-descriptor opening and error checking as well.
   * @param str String to be written to file.
   */
  def write(str: String): Unit = {
    writeFD match {
      case None => try {
        writeFD = Some(new BufferedWriter(new FileWriter(file)))
        writeFD.get.write(str)
      } catch {
        case x: Throwable => reporter.error("Could not open/log to file: " + filePath + ".\n" + x)
      }
      case _ => try {
        writeFD.get.write(str)
      } catch {
        case x: Throwable => reporter.error("Could not write to file: " + filePath + ".\n" + x)
      }
    }
  }

  /** Function to append a particular string to the log file
   *
   * Handles the file-descriptor opening and error checking as well.
   * @param str String to be appended to file.
   */
  def append(str: String): Unit = {
    appendFD match {
      case None => try {
        appendFD = Some(new BufferedWriter(new FileWriter(file, true)))
        appendFD.get.append(str)
      } catch {
        case x: Throwable => reporter.error("Could not open/log to file: " + filePath + ".\n" + x)
      }
      case _ => try {
        appendFD.get.append(str)
      } catch {
        case x: Throwable => reporter.error("Could not append to file: " + filePath + ".\n" + x)
      }
    }
  }

  /** Function to write a particular string to the log file if the file is empty
   * or non-existing.
   *
   * Use this for writing headers to files.
   * Handles the file-descriptor opening and error checking as well.
   * @param str String to be written to file if file was empty.
   */
  def writeIfFileEmpty(str: String): Unit = {
    if (new java.io.File(filePath).exists) {
      if (!Source.fromFile(filePath).nonEmpty) {
        write(str)
      }
    } else {
      write(str)
    }
  }

  /** Close open file-descriptors.
   *
   * Do call this before discarding the FileLogger object.
   */
  def close(): Unit = {
    writeFD match {
      case Some(x) => x.close()
      case None =>
    }
    appendFD match {
      case Some(x) => x.close()
      case None =>
    }
  }
}
