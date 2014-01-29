import sbt._
import sbt.Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object Dependencies {
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
}

object Joos1wCompilerBuild extends Build {

  val grammar = "joos-1w-grammar.txt"
  val machineGrammar = "joos-1w-machine-grammar.txt"

  val commonSettings = Defaults.defaultSettings ++ Seq(
    crossPaths := false,
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
  ) ++ assemblySettings ++ Seq(
    assemblyOption in assembly ~= { _.copy(includeScala = false) }
  )

  // Codes shared across multiple components
  lazy val common = Project(
    id = "common",
    base = file("common"),
    settings = commonSettings
  )

  lazy val preprocessor: Project = Project(
    id = "preprocessor",
    base = file("preprocessor"),
    settings = commonSettings ++ Seq(
      resourceGenerators in Compile <+= Def.task {
        // Generate a build.properties that the preprocessor can use
        val file = (resourceManaged in Compile).value / "build.properties"
        val properties = Map(
          "grammar" -> grammar,
          "machine-grammar" -> machineGrammar
        )

        val builder = new StringBuilder()
        for ((key, value) <- properties) {
          builder
            .append(key)
            .append('=')
            .append(value)
            .append('\n')
        }

        IO.write(file, builder.toString())
        Seq(file)
      }
    )
  ) dependsOn (common)

  lazy val scanner = Project(
    id = "scanner",
    base = file("scanner"),
    settings = commonSettings
  ) dependsOn(common, preprocessor)

  lazy val parser = Project(
    id = "parser",
    base = file("parser"),
    settings = commonSettings
  ) dependsOn(common, scanner, preprocessor)

  lazy val compiler = Project(
    id = "compiler",
    base = file("compiler"),
    settings = commonSettings ++ Seq(
      description := "Joos 1W Compiler"
    )
  ) dependsOn(common, preprocessor, scanner, parser)

  lazy val project = Project(
    id = "cs-444",
    base = file("."),
    settings = commonSettings
  ) aggregate(compiler, common, preprocessor, scanner, parser)

}
