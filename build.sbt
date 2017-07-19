name := "Daisy"

version := "0.0"

organization := "org.mpi-sws.ava"

scalaVersion := "2.11.6"

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.11.6",
    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
    "com.storm-enroute" %% "scalameter" % "0.7",
    "org.fusesource.hawtjni" % "hawtjni-runtime" % "1.9"  //for JNI
)

Keys.fork in run := true

Keys.fork in Test := true   //for native libraries to be on correct path

val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")

testFrameworks += scalaMeterFramework

lazy val Benchmark = config("bench") extend Test

testOptions in Test += Tests.Argument(scalaMeterFramework, "-silent")

parallelExecution in Test := false

logBuffered := false

lazy val basic = Project("daisy", file(".")
) configs(
  Benchmark
) settings(
  inConfig(Benchmark)(Defaults.testSettings): _*
)

lazy val apps = project in file("apps")

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
                    |java -Xmx2G -Xms512M -Xss64M -classpath "$${SCALACLASSPATH}" -Dscala.usejavacp=false scala.tools.nsc.MainGenericRunner -classpath "$${SCALACLASSPATH}" daisy.Main $$@ 2>&1 | tee -i last.log
                    |""".stripMargin)
    f.setExecutable(true)
  } catch {
    case e: Throwable =>
      s.log.error("There was an error while generating the script file: " + e.getLocalizedMessage)
  }
}