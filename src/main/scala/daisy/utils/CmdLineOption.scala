
/*
  The contents of this file is heaviy influenced and/or partly taken from
  the Leon Project which is released under the BSD 2 clauses license.
  See file LEON_LICENSE or go to https://github.com/epfl-lara/leon
  for full license details.
 */

package daisy

import scala.collection.immutable.Seq

abstract class CmdLineOptionDef[+T] {
  val name: String
  val description: String
  def helpLine: String

}

/*
  If the flag is present (on), then the option is on.
  Default is off.
 */
case class FlagOptionDef(name: String, description: String) extends CmdLineOptionDef[Boolean] {
  def helpLine: String = f"--$name%-30s $description"
}

/*
  Option which sets some parameter. This could be numeric or a string.
  Default value is given in default.
  */
case class ParamOptionDef(name: String, description: String, default: String) extends CmdLineOptionDef[String] {
  def helpLine: String =
    if (default != "") {
      "--%-30s %s".format(s"$name=$default", description + " Default value: " + default)
    } else {
      "--%-30s %s".format(s"$name=$default", description)
    }
}

/*
  Option for providing a list of things, like debug sections. Delimiter is ':'.
  Default is none.
 */
case class ListOptionDef(name: String, description: String, examples: List[String]) extends CmdLineOptionDef[Seq[String]] {
  def helpLine: String = "--%-30s %s".format(name + "=" + (examples.mkString(":")),
    description)
}

/*
  Option for making a choice from several fixed alternatives.
 */
case class ChoiceOptionDef(name: String, description: String,
  choices: Set[String], default: String) extends CmdLineOptionDef[String] {

  def helpLine: String = "--%-30s %s".format(s"$name=$default",
    description + " Valid options (choose one): " + choices.mkString(":"))
}

object CmdLineOption {
  def unapply[A](opt: CmdLineOption[A]) = Some((opt.name, opt.value))
}

abstract class CmdLineOption[+T] {
  val name: String
  val value: T
}

/** Boolean option, present means on. **/
case class FlagOption(name: String) extends CmdLineOption[Boolean] {
  val value = true  // if the option's there, then it's on
}
case class ParamOption(name: String, value: String) extends CmdLineOption[String]
case class ListOption(name: String, value: Seq[String]) extends CmdLineOption[Seq[String]]
case class ChoiceOption(name: String, value: String) extends CmdLineOption[String]