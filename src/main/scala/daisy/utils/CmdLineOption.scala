// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

abstract class CmdLineOption[+T: ClassTag] {
  val name: String
  def helpLine: String
  def apply(arg: Option[String], reporter: Reporter): T
}

/*
  If the flag is present (on), then the option is on.
  Default is off.
 */
case class FlagOption(name: String, description: String = "") extends CmdLineOption[Boolean] {
  override def helpLine: String = f"--$name%-30s $description"
  override def apply(arg: Option[String], reporter: Reporter): Boolean = arg.isDefined
}

/*
  Option which sets some parameter.
 */
case class StringOption(name: String, description: String = "")
  extends CmdLineOption[Option[String]] {

  override def helpLine: String = "--%-30s %s".format(s"$name=foo", description)

  override def apply(arg: Option[String], reporter: Reporter): Option[String] = arg
}

/*
  Option for providing a list of strings.
 */
case class MultiStringOption(name: String, examples: List[String], description: String = "")
  extends CmdLineOption[Seq[String]] {
  def helpLine: String = "--%-30s %s".format(s"$name=[${examples.mkString(":")}]", description)

  override def apply(arg: Option[String], reporter: Reporter): Seq[String] =
    arg.map(_.stripPrefix("[").stripPrefix("]").split(":").toList).getOrElse(Nil)
}

/*
  Option which sets some numeric parameter. Default value is given in default.
 */
case class NumOption(name: String, default: Long, description: String = "")
  extends CmdLineOption[Long] {

  override def helpLine: String =
      "--%-30s %s".format(s"$name=$default", description)

  override def apply(arg: Option[String], reporter: Reporter): Long = arg match {
    case None => default
    case Some(s) => try {
      s.toLong
    } catch {
      case e: NumberFormatException =>
        reporter.warning("Can't parse argument for option $name, using default")
        default
    }
  }
}

/*
  Option for making a choice from several fixed alternatives.
 */
case class ChoiceOption[T: ClassTag](name: String, choices: Map[String, T], default: String, description: String = "")
  extends CmdLineOption[T] {

  def helpLine: String = "--%-30s %s".format(s"$name=$default",
    description + ". Valid options: " + choices.keySet.toSeq.sorted.mkString(", "))

  override def apply(arg: Option[String], reporter: Reporter): T = arg match {
    case Some(s) if choices.keySet.contains(s) => choices(s)
    case Some(s) =>
      reporter.warning(s"Unknown choice value for $name: $s. Options: " +
        s"${choices.keySet.toSeq.sorted.mkString(", ")}. Using default $default")
      choices(default)
    case None => choices(default)
  }
}

object StringChoiceOption {
  def apply(_name: String, _choices: Set[String], _default: String, _description: String = ""): CmdLineOption[String] =
    ChoiceOption[String](_name, _choices.map(s => s -> s).toMap, _default, _description)
}

/*
  Option for making multiple choices from several fixed alternatives.
 */
case class MultiChoiceOptionDef[T: ClassTag](name: String, choices: Map[String, T], description: String = "")
  extends CmdLineOption[List[T]] {

  def helpLine: String = "--%-30s %s".format(name + "=[a:b:...]", description + ". Valid options: all, "
    + choices.keySet.toSeq.sorted.mkString(", "))

  override def apply(arg: Option[String], reporter: Reporter): List[T] = arg match {
    case Some("all") | Some("[all]") => choices.values.toList
    case Some(ss) => ss.stripPrefix("[").stripSuffix("]").split(":").toList.filter {
      case "all" =>
        reporter.warning(s"'all' in list for $name, ignoring"); false
      case s if !choices.keySet.contains(s) =>
        reporter.warning(s"Unknown choice value for $name: $s. Options: ${choices.keySet.toSeq.sorted.mkString(", ")}"); false
      case _ => true
    }.map(choices(_))
    case None => Nil
  }
}
