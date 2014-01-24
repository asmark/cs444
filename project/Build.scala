import sbt._
import Keys._

object Dependencies {
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
}

object Tasks {
  val compileAll = TaskKey[Unit]("compile-all", "Compile everything")
}

object Joos1wCompilerBuild extends Build {

  val commonSettings = Defaults.defaultSettings ++ Seq(
    fork in run := true,
    version := "1.0.0",
    scalaVersion := "2.10.3",
    scalacOptions := Seq(
      // Turn on all warnings
      "-feature",
      "-deprecation",
      "-unchecked",
      "-target:jvm-1.6",
      "-encoding", "utf8"
    ),
    libraryDependencies ++= Seq(
      Dependencies.scalaTest
    )
  )

  // Codes shared across multiple components
  lazy val common = Project(
    id = "common",
    base = file("common"),
    settings = commonSettings
  )

  lazy val preprocessor = Project(
    id = "preprocessor",
    base = file("preprocessor"),
    settings = commonSettings
  ) dependsOn(common)

  lazy val scanner = Project(
    id = "scanner",
    base = file("scanner"),
    settings = commonSettings
  ) dependsOn(common, preprocessor)

  lazy val parser = Project(
    id = "parser",
    base = file("parser"),
    settings = commonSettings
  ) dependsOn(common, scanner)

  lazy val compiler = Project(
    id = "compiler",
    base = file("compiler"),
    settings = commonSettings ++ Seq(
      name := "Joos 1W Compiler"
    )
  ) dependsOn(common, preprocessor, scanner, parser)

  lazy val project = Project(
    id = "cs-444",
    base = file("."),
    settings = commonSettings ++ Seq(
      name := "CS 444",
      Tasks.compileAll := {
        val a = (compile in Compile in compiler).toTask.value
        val b = (run in Compile in preprocessor).toTask("").value
      }
    )
  ) aggregate(compiler, common, preprocessor, scanner, parser)

}
