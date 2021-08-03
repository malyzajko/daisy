name := "Daisy"

version := "0.1"

organization := "org.mpi-sws.ava"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    //"-Ywarn-unused-import", unknown option in 2.13
    //"-Ywarn-unused",    too many false positives
    "-Ywarn-dead-code",
    "-Xlint:_,-adapted-args")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.13.3",
    "org.scalatest" % "scalatest_2.13" % "3.2.2", //% "test",
    "com.storm-enroute" %% "scalameter" % "0.19",
    "org.fusesource.hawtjni" % "hawtjni-runtime" % "1.9",  //for JNI
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.1"
)

envVars := Map("LC_NUMERIC" -> "en_US.UTF-8")

Keys.fork in run := true

javaOptions in run ++= Seq(
    "-Xms256M", "-Xmx4G", "-XX:+UseConcMarkSweepGC")

Keys.fork in Test := true   //for native libraries to be on correct path

parallelExecution in Test := false

val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")

testFrameworks += scalaMeterFramework

testOptions in Test += Tests.Argument(scalaMeterFramework, "-silent")

lazy val Benchmark = config("bench") extend Test

ivyLoggingLevel in clean := UpdateLogging.Quiet
ivyLoggingLevel in Test := UpdateLogging.Quiet

logBuffered := false

lazy val smtlib = RootProject(uri("git://github.com/regb/scala-smtlib"))

lazy val basic = Project("daisy", file(".")
) dependsOn (smtlib
) configs(
  Benchmark
) settings(
  inConfig(Benchmark)(Defaults.testSettings): _*
)

lazy val scriptFile = file(".") / "daisy"

clean := {
  clean.value
  if(scriptFile.exists && scriptFile.isFile) {
    scriptFile.delete
  }
}

lazy val script = taskKey[Unit]("Generate the daisy Bash script")

script := {
  val s = streams.value
  try {
    val cps = (dependencyClasspath in Compile).value
    val out = (classDirectory      in Compile).value
    val res = (resourceDirectory   in Compile).value
//    val jar = (artifactPath in Compile in packageBin).value
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
                    |java -Xmx4G -Xms512M -Xss64M -classpath "$${SCALACLASSPATH}" -Dscala.usejavacp=false scala.tools.nsc.MainGenericRunner -classpath "$${SCALACLASSPATH}" daisy.Main $$@ 2>&1 | tee -i last.log
                    |
                    |LC_NUMERIC=$$TMP
                    |""".stripMargin)
    f.setExecutable(true)
  } catch {
    case e: Throwable =>
      s.log.error("There was an error while generating the script file: " + e.getLocalizedMessage)
  }
}
