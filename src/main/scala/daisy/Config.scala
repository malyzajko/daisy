// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy

import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import lang.Trees.Expr
import lang.Identifiers._
import tools.{FinitePrecision, Interval, PartialInterval, Rational}
import FinitePrecision.{FixedPrecision, Precision}

case class Config(args: String*){

  // TODO figure out how to run this once at compile time
  verifyCmdLineOptions()

  val timers = new TimerStorage
  timers.total.start
  var reporter = new DefaultReporter(Set())

  // all available options from all phases
  private val allOptions: Set[CmdLineOption[Any]] = Main.globalOptions ++ Main.allComponents.flatMap(_.definedOptions)

  private val argsMap: Map[String,String] = args.filter(_.startsWith("--")).map(_.drop(2).split("=", 2).toList match {
    case List(name, value) => name -> value
    case List(name) => name -> "yes"
  }).toMap

  if (argsMap.keySet.contains("help")){
    showHelp(reporter)
  }

  argsMap.keySet.diff(allOptions.map(_.name)).foreach{
    name => reporter.warning(s"Unknown option: $name")
  }

  private var options: Map[String, Any] = allOptions.map(o => o.name -> o.apply(argsMap.get(o.name), reporter)).toMap

  val fixedPoint: Boolean = option[Precision]("precision") match { case FixedPrecision(_) => true case _ => false}
  val file: String = args.filterNot(_.startsWith("-")).toSeq match {
    case fs if fs.isEmpty => showHelp(reporter)
    case fs if fs.size > 1 => reporter.fatalError("More than one input file." + fs.mkString(":"))
    case fs => fs.head
  }
  val libFiles: List[String] = List("library/Real.scala")

  reporter = new DefaultReporter(option[List[DebugSection]]("debug").toSet,
    silent = hasFlag("silent"))

  def hasFlag(name: String): Boolean = option[Boolean](name)

  def option[T: ClassTag](name: String): T = options.get(name) map {
    case x: T => x
    case x: AnyRef => reporter.fatalError(s"Option $name ($x) has wrong type")
  } getOrElse reporter.fatalError(s"Unknown option $name")


  private def showHelp(reporter: Reporter): Nothing = {
    reporter.info("usage: [--help] [--debug=<N>] [..] <files>")
    reporter.info("")
    for (opt <- Main.globalOptions.toSeq.sortBy(_.name)) {
      reporter.info(opt.helpLine)
    }
    reporter.info("")
    reporter.info("Additional options, by component:")

    for (c <- Main.allComponents.toSeq.sortBy(_.name) if c.definedOptions.nonEmpty) {
      reporter.info("")
      reporter.info(s"${c.name} Phase")
      for(opt <- c.definedOptions.toSeq.sortBy(_.name)) {
        reporter.info(opt.helpLine)
      }
    }
    sys.exit(0)
  }

  private def verifyCmdLineOptions(): Unit = {
    val allOpts =
      Main.globalOptions.toList.map((_, "global")) ++
      Main.allComponents.flatMap(c => c.definedOptions.toList.map((_, c.name)))
    allOpts.groupBy(_._1.name).collect{
      case (name, opts) if opts.size > 1 =>
        Console.err.println(s"Duplicate command line option '$name' in " +
         opts.map("'"+_._2+"'").mkString(", "))
    }
  }

  // For testing only
  def copyWith(moreArgs: String*) = Config(args.toSeq ++ moreArgs.toSeq : _*)
}
