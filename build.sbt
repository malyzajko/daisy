name := "Daisy"

version := "0.1"

organization := "Uppsala universitet"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
    //"-deprecation",
    // "-optimize",
    // "-opt-warnings",
    "-Xno-patmat-analysis",  // Eva: takes a long time, but not so helpful for Daisy
    "-unchecked",
    "-feature",
    "-Xlint:infer-any",
    "-Wdead-code",                       // Warn when dead code is identified.
    // "-Wextra-implicit",                  // Warn when more than one implicit parameter section is defined.
    // "-Wnumeric-widen",                   // Warn when numerics are widened.
    // "-Wself-implicit",                   // Warn when an implicit resolves to an enclosing self-definition.
    // "-Wunused:imports",                  // Warn if an import selector is not referenced.
    // "-Wunused:patvars",                  // Warn if a variable bound in a pattern is unused.
    // "-Wunused:privates",                 // Warn if a private member is unused.
    // "-Wunused:locals",                   // Warn if a local definition is unused.
    // "-Wunused:explicits",                // Warn if an explicit parameter is unused.
    // "-Wunused:implicits",                // Warn if an implicit parameter is unused.
    // "-Wunused:params",                   // Enable -Wunused:explicits,implicits.
    // "-Wunused:linted",
    // "-Wvalue-discard"                   // Warn when non-Unit expression results are unused.
    //"-Ywarn-dead-code",
    //"-Xlint:_,-adapted-args"
  )

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.13.3",
    "org.scalatest" % "scalatest_2.13" % "3.2.2", //% "test",
    "com.storm-enroute" %% "scalameter" % "0.19",
    "org.fusesource.hawtjni" % "hawtjni-runtime" % "1.9",  //for JNI
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
    "com.regblanc" %% "scala-smtlib" % "0.2.1-42-gc68dbaa"
)

envVars := Map("LC_NUMERIC" -> "en_US.UTF-8")

Keys.fork in run := true

Keys.fork in Test := true   //for native libraries to be on correct path

javaOptions in run ++= Seq(
    "-Xms2048M", "-Xmx8G", "-XX:+UseConcMarkSweepGC")

parallelExecution in Test := false

// ivyLoggingLevel in clean := UpdateLogging.Quiet
// ivyLoggingLevel in Test := UpdateLogging.Quiet

lazy val scriptFile = file(".") / "daisy"

lazy val script = taskKey[Unit]("Generate the daisy Bash script")

script := {
  val s = streams.value
  try {
    val cps = (dependencyClasspath in Compile).value
    val out = (classDirectory      in Compile).value
    val res = (resourceDirectory   in Compile).value
    //val jar = (artifactPath in Compile in packageBin).value
    //val is64 = System.getProperty("sun.arch.data.model") == "64"
    val f = scriptFile
    if(f.exists) {
      s.log.info("Regenerating '"+f.getName+"' script ...")
      f.delete
    } else {
      s.log.info("Generating '"+f.getName+"' script ...")
    }
    val paths = (res.getAbsolutePath +: out.getAbsolutePath +: cps.map(_.data.absolutePath)).mkString(System.getProperty("path.separator"))
    //val base = baseDirectory.value.getAbsolutePath
    IO.write(f, s"""|#!/bin/bash --posix
                    |
                    |SCALACLASSPATH="$paths"
                    |
                    |TMP=$$LC_NUMERIC
                    |LC_NUMERIC=en_US.UTF-8
                    |
                    |java -Xmx2G -Xms1G -Xss1G -classpath "$${SCALACLASSPATH}" -Dscala.usejavacp=false scala.tools.nsc.MainGenericRunner -classpath "$${SCALACLASSPATH}" daisy.Main $$@ 2>&1 | tee -i last.log
                    |
                    |LC_NUMERIC=$$TMP
                    |""".stripMargin)
    f.setExecutable(true)
  } catch {
    case e: Throwable =>
      s.log.error("There was an error while generating the script file: " + e.getLocalizedMessage)
  }
}
