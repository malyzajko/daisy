// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

abstract class CmdLineOption[+T: ClassTag] {
  val name: String
  def helpLine: String
}

/*
  If the flag is present (on), then the option is on.
  Default is off.
 */
case class FlagOption(name: String, description: String = "") extends CmdLineOption[Boolean] {
  override def helpLine: String = f"--$name%-30s $description"
}

/*
  Option which sets some parameter.
 */
case class StringOption(name: String, description: String = "") extends CmdLineOption[Option[String]] {
  override def helpLine: String = "--%-30s %s".format(s"$name=foo", description)
}

/*
  Option for providing a list of strings.
 */
case class MultiStringOption(name: String, examples: List[String], description: String = "")
  extends CmdLineOption[Seq[String]] {
  def helpLine: String = "--%-30s %s".format(s"$name=[${examples.mkString(":")}]", description)
}

/*
  Option which sets some numeric parameter. Default value is given in default.
 */
case class NumOption(name: String, default: Long, description: String = "")
  extends CmdLineOption[Long] {

  override def helpLine: String =
      "--%-30s %s".format(s"$name=$default", description)
}

/*
  Option for making a choice from several fixed alternatives.
 */
case class ChoiceOption[T: ClassTag](name: String, choices: Map[String, T], default: String, description: String = "")
  extends CmdLineOption[T] {

  def helpLine: String = "--%-30s %s".format(s"$name=$default",
    description + ". Valid options: " + choices.keySet.toSeq.sorted.mkString(", "))
}

object StringChoiceOption {
  def apply(_name: String, _choices: Set[String], _default: String, _description: String = ""): CmdLineOption[String] =
    ChoiceOption[String](_name, _choices.map(s => s -> s).toMap, _default, _description)
}

/*
  Option for making multiple choices from several fixed alternatives.
 */
case class MultiChoiceOption[T: ClassTag](name: String, choices: Map[String, T], description: String = "")
  extends CmdLineOption[List[T]] {

  def helpLine: String = "--%-30s %s".format(name + "=[a:b:...]", description + ". Valid options: all, "
    + choices.keySet.toSeq.sorted.mkString(", "))
}

object MultiStringChoiceOption {
  def apply(_name: String, _choices: Seq[String], _description: String = ""): CmdLineOption[List[String]] =
    MultiChoiceOption[String](_name, _choices.map(s => s -> s).toMap, _description)
}
